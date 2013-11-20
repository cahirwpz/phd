open Format
open ExtList
open Utils

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

let rec print = function
  | Float n -> print_float n
  | Group l -> printf "@[<v 1>("; print_group l; printf ")@]"
  | Int n -> print_int n
  | Quote e -> print_char '\''; print e
  | String s -> printf "\"%s\"" s
  | Symbol s -> print_string s
  | TreeDecl (n, expr) -> printf "#%d=" n; print expr
  | TreeRef n -> printf "#%d#" n

and print_list_sep sep lst =
  iter_join print (fun () -> printf sep) lst

and print_form_sep n sep lst =
  let (fst, snd) = split_at n lst in
  printf "@[<v>";
  print_list_sep " " fst;
  printf "@,";
  print_list_sep sep snd;
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
  | _ as e -> print_list_sep " " e

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
let make_progn body =
  if List.length body > 1 then
    Group (Symbol "PROGN"::body)
  else
    List.hd body
let make_if pred if_true = 
  Group [Symbol "IF"; pred; if_true]
let make_if_else pred if_true if_false = 
  Group [Symbol "IF"; pred; if_true; if_false]
let make_let vars body =
  Group (Symbol "LET"::vars::body)
let make_setq name value =
  Group [Symbol "SETQ"; Symbol name; value]
let make_return value =
  Group [Symbol "RETURN"; value]
let make_block label body =
  Group ((Symbol "BLOCK")::(Symbol label)::body)
let make_label label body =
  Group ((Symbol "LABEL")::(Symbol label)::body)
let make_while pred body = 
  Group ((Symbol "WHILE")::pred::body)
let make_seq body =
  Group ((Symbol "SEQ")::body)
let make_cons left right =
  Group [Symbol "CONS"; left; right]
let make_loop body =
  Group ((Symbol "LOOP")::body)

(* Find recursively all occurences of old term and replace them with new term. *)
let rec substitute oldTerm newTerm lst =
  let recurse = substitute oldTerm newTerm in
  match lst with
  | term::rest when term = oldTerm ->
      newTerm::(recurse rest)
  | (Group lst)::rest ->
      Group (recurse lst)::(recurse rest)
  | x::xs ->
      x::(recurse xs)
  | x -> x

exception NoMatch;;

let symgen = new Symbol.gen "|:T%d|"

(* s-expr rewrite engine *)
let rec rewrite func = function
  | Group lst ->
      let exp = (try (func lst) with NoMatch -> Group lst)
      in rewrite_rec func exp
  | e -> e
and rewrite_rec func = function
  | Group lst ->
      Group (List.map (rewrite func) lst)
  | e -> e

let rec rewrite_n fs exp =
  match fs with
  | f::fs -> rewrite_n fs (rewrite f exp)
  | [] -> exp

 
(* Rewrite cond to series of if forms *)
let rec cond_to_if = function
  | [Symbol "COND"; Group (Quote (Symbol "T")::body)] ->
      make_progn body 
  | [Symbol "COND"; Group (pred::_) as body] ->
      make_if pred (cond_clause body)
  | Symbol "COND"::(Group (pred::_) as body)::rest ->
      make_if_else pred (cond_clause body) (cond_to_if (Symbol "COND"::rest))
  | _ -> raise NoMatch

and cond_clause = function
  | Group [pred] ->
      pred
  | Group (pred::body) ->
      make_progn body
  | _ -> failwith "Malformed cond clause."

(* Rewrite prog / prog1 / prog2 to let *)
let rec prog_to_let = function
  | Symbol "PROG"::Group []::body ->
      make_block "NIL" body
  | Symbol "PROG"::vars::body ->
      make_block "NIL" [make_let vars body]
  | Symbol "PROG1"::value::rest ->
      let temp = symgen#get in
      make_let (Group [Symbol temp]) ((make_setq temp value)::rest)
  | Symbol "PROG2"::value1::value2::rest ->
      let temp = symgen#get in
      make_let (Group [Symbol temp]) (value1::(make_setq temp value2)::rest)
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

let not_nil_symbol = function
  | Symbol "NIL" -> false
  | _ -> true

let not_symbol = function
  | Symbol _ -> false
  | _ -> true

(* Rewrite seq as block seq *)
let rec seq_simplify = function
  | Symbol "SEQ"::body ->
      make_seq (seq_to_blocks (List.filter not_nil_symbol body))
  | _ -> raise NoMatch

and seq_to_blocks = function
  | Symbol label::body ->
      let rest = List.dropwhile not_symbol body
      and body = List.takewhile not_symbol body in
      (make_label label body)::(seq_to_blocks rest)
  | (x::xs) as body ->
      let rest = List.dropwhile not_symbol body
      and body = List.takewhile not_symbol body in
      body @ (seq_to_blocks rest)
  | [] -> []

let reduce_trivial_exit = function
  | Symbol "SEQ"::rest ->
      let body = Utils.but_last rest in
      (match List.last rest with
      | Group [Symbol "EXIT"; Symbol "NIL"] ->
          make_seq body
      | Group [Symbol "EXIT"; value] ->
            make_seq (body @ [value])
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
          make_loop (Utils.but_last rest)
      | _ -> raise NoMatch)
  | _ -> raise NoMatch

let merge_loop_cond = function
  | Symbol "LOOP"::(Group [Symbol "IF";
                           Group [Symbol "NULL"; cond];
                           Group [Symbol "GO"; Symbol "G191"]])::body ->
       make_while cond body
  | _ -> raise NoMatch

let expand_loop_exit = function
  | [Symbol "SEQ"; loop; Group [Symbol "LABEL"; Symbol "G191"; exit]] ->
      make_seq [loop; exit]
  | _ -> raise NoMatch

let reduce_trivial_if = function
  | [Symbol "IF"; expr; Quote (Symbol "NIL"); Quote (Symbol "T")] ->
      Group [Symbol "NOT"; expr]
  | _ -> raise NoMatch

let simplify_exit = function
  | [Symbol "EXIT"; Group [Symbol "SETQ"; name; expr]] ->
      make_progn [Group [Symbol "SETQ"; name; expr]; Group [Symbol "EXIT"; name]]
  | _ -> raise NoMatch

let rec transform_loops = function
  | Symbol "LOOP"::(Group [Symbol "IF"; p; t; f])::body when is_return_nil t ->
      Group (Symbol "WHILE"::(transform_while p (f::body)))
  | _ -> raise NoMatch

and is_return_nil = function
  | Group [Symbol "RETURN"; Symbol "NIL"] -> true
  | _ -> false

and transform_while p body =
  match p with
  | Group [Symbol "OR"; Group [Symbol "ATOM"; x] as p'; Group snd]
    when List.last snd = Symbol "NIL" ->
      p'::(Group (but_last snd))::body
  | _ -> p::body

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

(* set of rules to be applied when simplifying an s-expression *)
let rules = [
  cond_to_if;
  lett_to_setq;
  seq_simplify;
  identify_loops;
  expand_loop_exit;
  merge_loop_cond;
  reduce_trivial_if;
  reduce_trivial_exit;
  identify_return_stmt;
  lift_seq;
  globals_shadowing;
  prog_to_let;
  ]

let reader_pass exp =
  unquote (reduce_tree_subst exp)

let simplify exp =
  let exp' = reader_pass exp
  in rewrite_n rules exp'
