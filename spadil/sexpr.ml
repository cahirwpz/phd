open Aux
open ExtList
open Format

type sexpr =
  | Float of float
  | Group of sexpr list 
  | Int of int
  | Quote of sexpr
  | String of string
  | Symbol of string 
  | TreeDecl of int * sexpr
  | TreeRef of int

let is_compound = function
  | Group _ | Quote _ -> true
  | _ -> false

let not_nil_symbol = function
  | Symbol "NIL" -> false
  | _ -> true

let is_symbol = function
  | Symbol _ -> true
  | _ -> false

let rec print = function
  | Float n -> print_float n
  | Group l -> printf "@[<v 1>("; print_group l; printf ")@]"
  | Int n -> print_int n
  | Quote e -> print_char '\''; print e
  | String s -> printf "\"%s\"" s
  | Symbol s -> print_string s
  | TreeDecl (n, expr) -> printf "#%d=" n; print expr
  | TreeRef n -> printf "#%d#" n

and print_form_sep n sep lst =
  let (l1, l2) = List.split_nth n lst in
  printf "@[<v>";
  print_list print " " l1;
  printf "@,";
  print_list print sep l2;
  printf "@]"

and print_group = function
  | Symbol "SDEFUN"::_ 
  | Symbol "DEFUN"::_ as g -> print_form_sep 3 "@ " g
  | Symbol "LAMBDA"::_
  | Symbol "LET"::_
  | Symbol "BLOCK"::_
  | Symbol "LABEL"::_
  | Symbol "SPROG"::_
  | Symbol "PROG"::_ as g -> print_form_sep 2 "@ " g
  | Symbol "LOOP"::_
  | Symbol "WHILE"::_
  | Symbol "PROGN"::_
  | Symbol "RETURN"::_
  | Symbol "OR"::_
  | Symbol "AND"::_
  | Symbol "IF"::_
  | Symbol "SEQ"::_
  | Symbol "EXIT"::_
  | Symbol "UNWIND-PROTECT"::_
  | Symbol "COND"::_ as g -> print_form_sep 1 "@," g
  | _ as e -> print_list print " " e

(* Perform rewrites normally done by LISP reader. *)

let slots = Array.create 20 (Group [])

(* Handle subtree substitutions *)
let rec reduce_tree_subst = function
  | Group g -> Group (List.map reduce_tree_subst g)
  | Quote q -> Quote (reduce_tree_subst q)
  | TreeDecl (n, expr) ->
      let reduced = reduce_tree_subst expr
      in slots.(n) <- reduced; reduced
  | TreeRef n -> slots.(n)
  | _ as expr -> expr

(*
 * (a . (b c)) => (a b c)
 * '(a . b) => (cons 'a 'b)
 * '(a b c ...) => (list 'a 'b 'c) 
 *)
let rec unquote = function
  | Quote (Group lst) ->
      let unquoted = List.map (fun a -> Quote a) lst in
      Group (Symbol "LIST"::(List.map unquote unquoted))
  | Quote ((Int x) as value) ->
      value
  | Quote ((Float x) as value) ->
      value
  | Quote ((String n) as value) ->
      value
  | Group [a; Symbol "."; b] when not (is_compound a || is_compound b) ->
      Group [Symbol "CONS"; unquote (Quote a); unquote (Quote b)]
  | Group [a; Symbol "."; Group b] ->
      Group ((unquote a)::(List.map unquote b))
  | Group lst ->
      Group (List.map unquote lst)
  | x -> x

(* Rewrite rule tools *)
let make_progn = function
  | [] -> failwith "Cannot create block with no expressions!"
  | [x] -> x
  | body -> Group (Symbol "PROGN"::body)

let make_if pred if_true = 
  Group [Symbol "IF"; pred; if_true]
let make_if_else pred if_true if_false = 
  Group [Symbol "IF"; pred; if_true; if_false]
let make_setq name value =
  Group [Symbol "SETQ"; Symbol name; value]
let make_return value =
  Group [Symbol "RETURN"; value]
let make_label label body =
  Group ((Symbol "LABEL")::(Symbol label)::body)
let make_seq body =
  Group ((Symbol "SEQ")::body)
let make_loop body =
  Group ((Symbol "LOOP")::body)
let make_function name =
  Group [Symbol "FUNCTION"; Symbol name]
let make_list lst =
  List.fold_right (fun a b -> Group [Symbol "CONS"; a; b]) lst (Symbol "NIL")

exception NoMatch;;

let symgen = new Symbol.gen "|:T%d|"

(* s-expr rewrite engine *)
let rec rewrite fn = function
  | Group lst ->
      (try
        match (fn lst) with
        | Group lst' ->
            Group (List.map (rewrite fn) lst')
        | e -> e
      with NoMatch -> 
        Group (List.map (rewrite fn) lst))
  | e -> e

(* Rewrite cond to series of if forms *)
let rec cond_to_if = function
  | [Symbol "COND"; Group (Quote (Symbol "T")::body)] ->
      make_progn body 
  | [Symbol "COND"; Group (pred::body)] ->
      make_if pred (make_progn body)
  | Symbol "COND"::(Group (pred::body))::rest ->
      make_if_else pred (make_progn body) (cond_to_if (Symbol "COND"::rest))
  | _ -> raise NoMatch

(* Handle global variable shadowing (DECLARE (SPECIAL (...))). *)
let rec globals_shadowing = function
  | Symbol "DEFUN"::name::(Group vs)::(Group [Symbol "DECLARE"; special])::body ->
      Group (Symbol "DEFUN"::name::(rewrite_special vs body special))
  | Symbol "PROG"::(Group vs)::(Group [Symbol "DECLARE"; special])::body ->
      Group (Symbol "PROG"::(rewrite_special vs body special))
  | _ -> raise NoMatch

and rewrite_special vs body = function
  | Group (Symbol "SPECIAL"::vs2) ->
      let to_symbol = fun x -> Symbol x in
      let vs' = get_symbols [] vs and vs2' = get_symbols [] vs2 in
      let diffs = List.map to_symbol (VarSet.elements (VarSet.diff vs' vs2')) in
      let temps = List.map (fun _ -> symgen#get) vs2 in
      let temps' = List.map to_symbol temps in
      let saves = List.map2 make_setq temps vs2
      and restores = List.map2 make_setq (VarSet.elements vs2') temps' in
      [Group (temps' @ diffs); 
       make_progn saves; make_progn body; make_progn restores]
  | _ -> failwith "Expected special."

and get_symbols acc = function
  | [] -> VarSet.from_list acc
  | (Symbol x)::xs -> get_symbols (x::acc) xs
  | _ -> failwith "Symbols expected."

(* Rewrite lett as let *)
let lett_to_setq = function
  | Symbol "LETT"::Symbol var::value::_ ->
      make_setq var value
  | _ -> raise NoMatch

(* Reorganize body of seq into labelled blocks. *)
let rec seq_simplify = function
  | Symbol "SEQ"::body ->
      let body = List.filter not_nil_symbol body in
      let sliced_body = slice_with is_symbol body in
      make_seq (List.flatten @@ List.map block_add_label sliced_body)
  | _ -> raise NoMatch

and block_add_label = function
  | (Symbol label)::body -> [make_label label body]
  | body -> body

let reduce_trivial_exit = function
  | Symbol "SEQ"::rest ->
      let body = but_last rest in
      (match List.last rest with
      | Group [Symbol "EXIT"; Symbol "NIL"] ->
          make_progn body
      | Group [Symbol "EXIT"; value] ->
          make_progn (body @ [value])
      | Group [Symbol "BREAK"] ->
          make_progn rest
      | _ ->
          raise NoMatch)
  | _ -> raise NoMatch

let lift_seq = function
  | Symbol "SEQ"::body ->
      make_progn body
  | _ -> raise NoMatch

let identify_loops = function
  | Symbol "LABEL"::Symbol "G190"::rest ->
      (match List.last rest with
      | Group [Symbol "GO"; Symbol "G190"] ->
          make_loop (but_last rest)
      | _ ->
          raise NoMatch)
  | _ -> raise NoMatch

let rec identify_loop_breaks = function
  | Symbol "LOOP"::body ->
      make_loop (find_loop_break body)
  | _ ->
      raise NoMatch

and find_loop_break = function
  | Group [Symbol "IF"; cond; Group [Symbol "GO"; Symbol "G191"]]::rest ->
      (make_if cond (Group [Symbol "BREAK"]))::(find_loop_break rest)
  | expr::rest ->
      expr::(find_loop_break rest)
  | [] -> []

let rec split_if_or_break = function
  | [Symbol "IF"; Group (Symbol "OR"::conds); Group [Symbol "BREAK"]] ->
      make_progn (List.map break_when conds)
  | _ -> raise NoMatch

and break_when = function
  | Group (Symbol "PROGN"::body) when List.last body = Symbol "NIL" ->
      make_progn (but_last body)
  | cond ->
      make_if cond (Group [Symbol "BREAK"])

let expand_loop_exit = function
  | [Symbol "LABEL"; Symbol "G191"; exit] ->
      exit
  | _ -> raise NoMatch

let reduce_double_not = function
  | (Symbol "IF")::(Group [Symbol "NULL"; Group [Symbol "NOT"; expr]])::rest ->
      Group (Symbol "IF"::expr::rest)
  | _ -> raise NoMatch

let reduce_trivial_if = function
  | [Symbol "IF"; expr; Quote (Symbol "NIL"); Quote (Symbol "T")] ->
      Group [Symbol "NOT"; expr]
  | _ -> raise NoMatch

let simplify_exit = function
  | [Symbol "EXIT"; Group [Symbol "SETQ"; name; expr]] ->
      make_progn [Group [Symbol "SETQ"; name; expr]; Group [Symbol "EXIT"; name]]
  | _ -> raise NoMatch

let rec identify_return_stmt = function
  | [Symbol "SDEFUN"; fn; args;
     Group [Symbol "SPROG"; vars; 
      Group [Symbol "SEQ";
       Group [Symbol "EXIT"; body];
       Group [Symbol "LABEL"; Symbol label;
        Group [Symbol "EXIT"; Symbol var]]]]] ->
      Group [Symbol "SDEFUN"; fn; args;
       Group [Symbol "SPROG"; vars;
        replace_return_stmt label var body]]
  | _ -> raise NoMatch

and replace_return_stmt label var fnbody =
  let rewrite_return body = (match body with 
    | [Symbol "PROGN";
       Group [Symbol "SETQ"; Symbol var'; retval];
       Group [Symbol "GO"; Symbol label']]
      when var' = var && label' = label ->
        make_return retval
    | _ -> raise NoMatch) in
  rewrite rewrite_return fnbody

let rec identify_break_stmt = function
  | Symbol "SEQ"::body ->
      make_seq (locate_break_target body)
  | _ -> raise NoMatch

and locate_break_target = function
  | Group [Symbol "EXIT"; body]::
    Group [Symbol "LABEL"; Symbol label;
      Group [Symbol "EXIT"; Symbol var]]::rest ->
        (replace_break_stmt label var body)::(locate_break_target rest)
  | x::xs -> x::(locate_break_target xs)
  | [] -> []

and replace_break_stmt label var fnbody =
  let rewrite_break body = (match body with 
    | [Symbol "EXIT";
       Group [Symbol "PROGN";
         Group [Symbol "SETQ"; Symbol var'; Symbol "$NoValue"];
         Group [Symbol "GO"; Symbol label']]]
      when var' = var && label' = label ->
        Group [Symbol "BREAK"]
    | _ -> raise NoMatch) in
  rewrite rewrite_break fnbody

let setq_prog1_swap = function
  | [Symbol "SETQ"; Symbol var;
      Group (Symbol "PROG1"::exp::exps)] ->
      make_progn (make_setq var exp::exps)
  | _ -> raise NoMatch

let inc_SI = function
  | [Symbol "inc_SI"; value] ->
      Group [Symbol "add_SI"; value; Int 1]
  | _ -> raise NoMatch

let rec flatten_blocks = function
  | x::xs ->
      (match x with
      | Symbol "PROGN" | Symbol "LOOP" ->
          Group (x::flatten_body xs)
      | _ -> raise NoMatch)
  | _ -> raise NoMatch

and flatten_body = function
  | Group (Symbol "PROGN"::body')::body ->
      body' @ (flatten_body body)
  | expr::body ->
      expr::(flatten_body body)
  | [] -> []

let identify_fn = function
  | [Symbol "function"; Symbol name] ->
      make_function name
  | _ -> raise NoMatch

let spadcall = function
  | (Symbol "SPADCALL")::rest ->
      let (args, fn) = split_at_last rest in
      let fn_ptr = Group [Symbol "CAR"; fn]
      and fn_env = Group [Symbol "CDR"; fn] in
      Group (Symbol "FUNCALL"::fn_ptr::args @ [fn_env])
  | _ -> raise NoMatch

let list_to_cons = function
  | (Symbol "LIST")::args ->
      make_list args
  | _ -> raise NoMatch

(* set of rules to be applied when simplifying an s-expression *)
let rules = [
  list_to_cons;
  identify_fn;
  cond_to_if;
  lett_to_setq;
  seq_simplify;
  identify_return_stmt;
  identify_break_stmt;
  identify_loops;
  expand_loop_exit;
  identify_loop_breaks;
  split_if_or_break;
  reduce_trivial_if;
  reduce_double_not;
  reduce_trivial_exit;
  lift_seq;
  globals_shadowing;
  setq_prog1_swap;
  inc_SI;
  flatten_blocks;
  spadcall;
  ]

let reader_pass exp =
  unquote (reduce_tree_subst exp)

let simplify sexp =
  List.fold_left (fun sexp fn -> rewrite fn sexp) (reader_pass sexp) rules
