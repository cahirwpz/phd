open Ast;;

exception NoMatch;;

let is_compound exp =
  match exp with
  | Char _ | Float _ | Int _ | String _ | Symbol _ -> false
  | _ -> true

(* generate new symbols on demand *)
let counter = ref 0;;

let make_var () =
  let n = !counter in
  counter := !counter + 1;
  "@" ^ (string_of_int n)

(* ast rewrite engine *)
let rec rewrite rule e =
  let r = apply rule and rn = apply_to_list rule in
  match e with
  | Apply (fn, exps) -> Apply (r fn, rn exps)
  | ArrayRef (name, exp) -> ArrayRef (name, r exp)
  | Assign (name, exp) -> Assign (name, r exp)
  | BinOp (op, exp1, exp2) -> BinOp (op, r exp1, r exp2)
  | Block (vars, exps) -> Block (vars, rn exps)
  | Cons (fst, snd) -> Cons (r fst, r snd)
  | Function (name, args, exp) -> Function (name, args, r exp)
  | IfThen (pred, t) -> IfThen (r pred, r t)
  | IfThenElse (pred, t, f) -> IfThenElse (r pred, r t, r f)
  | Label (name, exp) -> Label (name, r exp)
  | Leave (name, exp) -> Leave (name, r exp)
  | Lambda (args, exp) -> Lambda (args, r exp)
  | Loop exp -> Loop (r exp)
  | e -> e
and apply rule exp =
  let exp = (try (rule exp) with NoMatch -> exp)
  in rewrite rule exp
and apply_to_list rule exps =
  List.map (apply rule) exps

let rec rewrite_n ruleset exp =
  match ruleset with
  | rule::rules -> rewrite_n rules (rewrite rule exp)
  | [] -> exp

(* Used to simplify function calls *)
let assignments = ref [];;
let variables = ref [];;

let get_assignments () =
  let value = !assignments
  in assignments := []; value

let get_variables () = 
  let value = !variables
  in variables := []; value

(* Simplify function calls *)
let rec extract_compound_args exp =
  let n_exp = extract_compound_args' exp in
  let vars = get_variables ()
  and body = List.rev (n_exp::(get_assignments ())) in
  if List.length body > 1
  then Block ((if List.length vars > 0 then List.rev vars else []), body)
  else exp

and extract_compound_args' = function
  | BinOp (op, e1, e2) ->
      BinOp (op, recurse_if_compound e1, recurse_if_compound e1)
  | Apply (fn, args) ->
      Apply (fn, List.map recurse_if_compound args)
  | e -> e

and recurse_if_compound exp =
  match exp with
  | BinOp (_, _, _) | Apply (_, _) | IfThenElse (_, _, _) ->
      let n_exp = extract_compound_args' exp
      and t = make_var () in
      variables := t::!variables;
      assignments := (Assign (t, n_exp))::!assignments;
      Symbol t
  | Assign (x, y) ->
      assignments := exp::!assignments;
      Symbol x
  | _ -> exp

(* Reduce a block that contains only one expression*)
let reduce_block = function
  | Block (vars1, [Block (vars2, exps)]) ->
      Block (vars1 @ vars2, exps)
  | Block ([], [exp]) ->
      exp
  | _ -> raise NoMatch

(* Flatten structure of a block *)
let rec flatten_block = function
  | Block (vars, body) ->
      Block (vars @ (collect_vars body), collect_exps body)
  | _ -> raise NoMatch

and collect_vars = function
  | (Block (vs, _))::xs -> vs @ (collect_vars xs)
  | x::xs -> collect_vars xs
  | [] -> []
      
and collect_exps = function
  | (Block (_, x))::xs -> x @ (collect_exps xs)
  | x::xs -> x::(collect_exps xs)
  | [] -> []

(* If "leave $L with x" is last in $L block then remove it. *)
let rec reduce_last_leave = function
  | (Label (l1, Block (vars, exps))) ->
      Label (l1, Block (vars, reduce_last_leave' l1 exps))
  | _ -> raise NoMatch

and reduce_last_leave' l1 = function
  | [Leave (l2, exp)] when l1 = l2 ->
      [exp]
  | x::xs ->
      x::(reduce_last_leave' l1 xs)
  | [] -> []

(* Remove label if not used *)
(* TODO *)

let rules = [
  reduce_last_leave;
  extract_compound_args;
  reduce_block;
  flatten_block;
]

let simplify exp =
  counter := 0;
  rewrite_n rules exp
