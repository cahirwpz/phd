open Format;;

type sexpr =
  | Float of float
  | Group of sexpr list 
  | Int of int
  | Quote of sexpr
  | String of string
  | Symbol of string 
  | TreeDecl of int * sexpr
  | TreeRef of int

let rec split n lst =
  split_rec n lst []
and split_rec n lst acc =
  if n > 0
  then split_rec (n - 1) (List.tl lst) (List.hd lst::acc)
  else (List.rev acc, lst)

let rec print e =
  print_rec e;
  printf "@."
and print_list_sep sep = function
  | head::(_::_ as tail) ->
      print_rec head;
      printf sep; 
      print_list_sep sep tail
  | head::_ ->
      print_rec head;
  | [] -> ()
and print_list lst = print_list_sep " " lst
and print_form_sep n sep lst =
  let (fst, snd) = split n lst in
  printf "@[<v>";
  print_list fst;
  printf "@,";
  print_list_sep sep snd;
  printf "@]"
and print_form n lst = print_form_sep n " " lst
and print_group = function
  | Symbol "defun"::_ as g -> print_form 3 g
  | Symbol "lambda"::_
  | Symbol "let"::_
  | Symbol "prog"::_ as g -> print_form 2 g
  | Symbol "loop"::_
  | Symbol "progn"::_
  | Symbol "return"::_
  | Symbol "or"::_
  | Symbol "and"::_
  | Symbol "if"::_
  | Symbol "seq"::_
  | Symbol "unwind-protect"::_
  | Symbol "cond"::_ as g -> print_form_sep 1 "@," g
  | _ as e -> print_list e
and print_rec = function
  | Float n -> print_float n
  | Group l -> printf "@[<v 1>("; print_group l; printf ")@]"
  | Int n -> print_int n
  | Quote e -> print_char '\''; print_rec e
  | String s -> printf "\"%s\"" s
  | Symbol s -> printf "|%s|" s
  | TreeDecl (n, expr) -> printf "#%d=" n; print_rec expr
  | TreeRef n -> printf "#%d#" n

let slots = Array.create 20 (Group [])

let rec reduce = function
  | Group g -> Group (List.map reduce g)
  | Quote q -> Quote (reduce q)
  | TreeDecl (n, expr) ->
      let reduced = reduce expr
      in slots.(n) <- reduced; reduced
  | TreeRef n -> slots.(n)
  | _ as expr -> expr

(* generate new symbols on demand *)
let symbol = ref 0;;

let make_symbol () =
  let n = !symbol in
  symbol := !symbol + 1;
  Symbol (sprintf "#:SPAD-%d" n)
 
(* Rewrite cond to series of if forms *)
let rec cond_to_if = function
  | Symbol "cond"::(Group (Quote (Symbol "t")::action))::[] ->
      Group (Symbol "progn"::action)
  | Symbol "cond"::(Group (pred::action))::[] ->
      let true_clause = Group (Symbol "progn"::action)
      in Group [Symbol "if"; pred; true_clause]
  | Symbol "cond"::(Group (pred::action))::rest ->
      let true_clause = Group (Symbol "progn"::action)
      and false_clause = cond_to_if (Symbol "cond"::rest)
      in Group [Symbol "if"; pred; true_clause; false_clause]
  | lst -> Group lst

(* Rewrite prog to let *)
let prog_to_let = function
  | Symbol "prog"::Group []::prog ->
      Group (Symbol "progn"::prog)
  | Symbol "prog"::vars::prog ->
      Group (Symbol "let"::vars::prog)
  | lst -> Group lst

(* Rewrite prog1 to let *)
let prog1_to_let = function
  | Symbol "prog1"::value::rest ->
      let temp = make_symbol () in
      let assign = Group [Symbol "setq"; temp; value] in
      let body = Group (Symbol "progn"::assign::rest @ [temp])
      in Group [Symbol "let"; Group [temp]; body]
  | lst -> Group lst

(* Remove progn if encloses one s-expr *)
let remove_single_progn = function 
  | Symbol "progn"::Group lst::[] -> Group lst
  | lst -> Group lst

(* Rewrite dots as cons *)
let rec dot_to_cons lst =
  Group (dot_to_cons_rec lst)
and dot_to_cons_rec = function
  | x::Symbol "."::y::rest ->
      (Group [Symbol "cons"; x; y])::(dot_to_cons_rec rest)
  | x::xs -> x::(dot_to_cons_rec xs)
  | [] -> []

(* Rewrite lett as let *)
let lett_to_setq = function
  | Symbol "lett"::symbol::rest ->
      Group [Symbol "setq"; symbol; Group rest]
  | lst -> Group lst

(* Rewrite seq as progn *)
let seq_to_progn = function
  | Symbol "seq"::rest ->
      Group (Symbol "progn"::rest)
  | lst -> Group lst

(* s-expr rewrite engine *)
let rec rewrite func = function
  | Group (_ as lst) ->
      rewrite_rec func (func lst)
  | e -> e
and rewrite_rec func = function
  | Group lst ->
      Group (List.map (rewrite func) lst)
  | e -> e

let rec rewrite_n fs exp =
  match fs with
  | f::fs -> rewrite_n fs (rewrite f exp)
  | [] -> exp

(* set of rules to be applied when simplifying an s-expression *)
let rules = [
  lett_to_setq;
  seq_to_progn;
  dot_to_cons;
  cond_to_if;
  prog1_to_let;
  prog_to_let;
  remove_single_progn;
  ]

let simplify expr = rewrite_n rules (reduce expr)
