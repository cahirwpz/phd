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
  | Symbol "block"::_
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

let rec split_by elem lst =
  split_by' elem [] lst
and split_by' elem left = function
  | x::xs when x = elem -> (List.rev left, xs)
  | x::xs -> split_by' elem (x::left) xs
  | [] -> (List.rev left, [])

let make_progn body =
  Group (Symbol "progn"::body)
let make_if pred if_true = 
  Group [Symbol "if"; pred; if_true]
let make_if_else pred if_true if_false = 
  Group [Symbol "if"; pred; if_true; if_false]
let make_let vars body =
  Group (Symbol "let"::vars::body)
let make_setq name value =
  Group [Symbol "setq"; Symbol name; value]
let make_let1 name body =
  Group ((Symbol "let1")::(Symbol name)::body)
let make_block label body =
  Group ((Symbol "block")::(Symbol label)::body)
let make_cons left right =
  Group [Symbol "cons"; left; right]
let make_seq body = 
  Group (Symbol "seq"::body)
let make_exit name = 
  Group [Symbol "exit"; Symbol name]
let make_loop name body =
  Group ((Symbol "loop")::(Symbol name)::body)
let make_return name value =
  Group [Symbol "return-from"; Symbol name; value]

(* Find recursively all occurences of old term and replace them with new term *)
let rec substitute oldTerm newTerm lst =
  let recurse = substitute oldTerm newTerm
  in match lst with
  | term::rest when term = oldTerm ->
      newTerm::(recurse rest)
  | (Group lst)::rest ->
      Group (recurse lst)::(recurse rest)
  | x::xs ->
      x::(recurse xs)
  | x -> x

exception NoMatch;;

(* generate new symbols on demand *)
let symbol = ref 0;;

let make_var () =
  let n = !symbol in
  symbol := !symbol + 1;
  sprintf "@@%d" n
 
(* Rewrite cond to series of if forms *)
let rec cond_to_if = function
  | [Symbol "cond"; Group (Quote (Symbol "t")::body)] ->
      make_progn body 
  | [Symbol "cond"; Group (pred::body)] ->
      make_if pred (make_progn body)
  | Symbol "cond"::(Group (pred::body))::rest ->
      make_if_else pred (make_progn body) (cond_to_if (Symbol "cond"::rest))
  | _ -> raise NoMatch

(* Rewrite prog / prog1 / prog2 to let *)
let prog_to_let = function
  | Symbol "prog"::Group []::body ->
      make_progn body
  | Symbol "prog"::vars::prog ->
      make_let vars prog
  | Symbol "prog1"::value::rest ->
      let temp = make_var ()
      in make_let1 temp ((make_setq temp value)::rest)
  | Symbol "prog2"::value1::value2::rest ->
      let temp = make_var ()
      in make_let1 temp (value1::(make_setq temp value2)::rest)
  | _ -> raise NoMatch

(* Rewrite dots as cons *)
let rec dot_to_cons lst =
  Group (dot_to_cons_rec lst)
and dot_to_cons_rec = function
  | x::Symbol "."::y::rest ->
      (make_cons x y)::(dot_to_cons_rec rest)
  | x::xs -> x::(dot_to_cons_rec xs)
  | [] -> []

(* Rewrite lett as let *)
let lett_to_setq = function
  | Symbol "lett"::Symbol var::value::_ ->
      make_setq var value
  | _ -> raise NoMatch

(* Rewrite seq as block seq *)
let rec seq_to_block = function
  | Symbol "seq"::body ->
      make_block "seq" (seq_to_block' body)
  | _ -> raise NoMatch
and seq_to_block' = function
  | Symbol label::rest ->
      let (exps, rest) = split_by (Symbol "g191") rest in
      [make_block label exps; make_block "g191" rest]
  | x::xs -> x::(seq_to_block' xs)
  | [] -> []

(* Find loops block g190 begin ... end goto g190 *)
let rec detect_loops = function
  | Symbol "block"::(Symbol label)::body -> (
      match (Utils.last body) with
      | Group [Symbol "go"; Symbol dst] when dst = label ->
          (make_loop label (Utils.but_last body))
      | _ -> raise NoMatch)
  | _ -> raise NoMatch

(* convert exit / return to return-from *)
let rec unify_returns = function
  | [Symbol "return"; value] ->
      make_return "nil" value
  | [Symbol "exit"; value] ->
      make_return "seq" value
  | _ -> raise NoMatch

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

(* set of rules to be applied when simplifying an s-expression *)
let rules = [
  seq_to_block;
  detect_loops;
  lett_to_setq;
  cond_to_if;
  unify_returns;
  prog_to_let;
  dot_to_cons;
  ]

let simplify expr = rewrite_n rules (reduce expr)
