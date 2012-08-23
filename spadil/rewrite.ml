open Ast
open Utils

exception NoMatch

let is_compound = function
  | Char _ | Float _ | Int _ | String _ | Symbol _ | Label _ | Global _ ->
      false
  | _ ->
      true

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
  | Assign (name, exp) -> Assign (name, r exp)
  | Block (vars, exps) -> Block (vars, rn exps)
  | Cons (fst, snd) -> Cons (r fst, r snd)
  | IfThenElse (pred, t, f) -> IfThenElse (r pred, r t, r f)
  | Lambda (args, exp) -> Lambda (args, r exp)
  | Loop exp -> Loop (r exp)
  | Return exp -> Return (r exp)
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
  then Block (makeStringSet vars, body)
  else exp

and extract_compound_args' = function
  | Apply (fn, args) ->
      Apply (rewrite_compound fn, List.map rewrite_compound args)
  | IfThenElse (pred, t, f) ->
      IfThenElse (rewrite_compound pred, t, f)
  | Assign (name, Apply (fn, args)) ->
      let value = Apply (rewrite_compound fn, List.map rewrite_compound args)
      in Assign (name, value)
  | Assign (name, IfThenElse (pred, t, f)) ->
      let value = IfThenElse (rewrite_compound pred, t, f)
      in Assign (name, value)
  | Return exp ->
      Return (rewrite_compound exp)
  | e -> e

and rewrite_compound exp =
  match exp with
  | Apply (_, _) | IfThenElse (_, _, _) | Lambda (_, _) | Return _ ->
      let n_exp = extract_compound_args' exp
      and t = make_var () in
      variables := t::!variables;
      assignments := (Assign (t, n_exp))::!assignments;
      Symbol t
  | Assign (x, y) ->
      assignments := exp::!assignments;
      Symbol x
  | Block (_, _) ->
      let t = make_var () in
      variables := t::!variables;
      assignments := (Assign (t, exp))::!assignments;
      Symbol t
  | _ -> exp

(* Reduce a block that contains only one expression*)
let reduce_block = function
  | Block (vars1, [Block (vars2, exps)]) ->
      Block (StringSet.union vars1 vars2, exps)
  | Block (vars, [exp]) when StringSet.is_empty vars ->
      exp
  | _ -> raise NoMatch

(* Flatten structure of a block *)
let rec flatten_block = function
  | Block (vars, body) ->
      let body = List.map flatten_block body in
      let vars = StringSet.union vars (collect_vars body) in
      Block (vars, collect_exps body)
  | x -> x

and collect_vars = function
  | (Block (vs, _))::xs -> StringSet.union vs (collect_vars xs)
  | x::xs -> collect_vars xs
  | [] -> StringSet.empty
      
and collect_exps = function
  | (Block (_, x))::xs -> x @ (collect_exps xs)
  | x::xs -> x::(collect_exps xs)
  | [] -> []

(* Reduce one-time lambda invocations *)
let rec reduce_lambda = function
  | Apply (Lambda ([], body), []) ->
      body
  | _ -> raise NoMatch

(* push assign deeper into the structure *)
let rec rewrite_assign = function
  | Assign (var, IfThenElse (pred, t, f)) ->
      let t = rewrite_assign (Assign (var, t))
      and f = rewrite_assign (Assign (var, f))
      in IfThenElse (pred, t, f)
  | Assign (var, Block (vars, exps)) when not (StringSet.mem var vars) ->
      let (xs, x) = split_at_last exps
      in Block (vars, xs @ [rewrite_assign (Assign (var, x))])
  | Assign (var1, Assign (var2, exp)) ->
      Block (StringSet.empty, [Assign (var2, exp); Assign (var1, Symbol var2)])
  | x -> x

(* push return deeper into the structure *)
let rec rewrite_return = function
  | Return (Block (vars, exps)) ->
      let (xs, x) = split_at_last exps
      in Block (vars, xs @ [rewrite_return (Return x)])
  | x -> x

let rules = [
  reduce_lambda;
  rewrite_return;
  extract_compound_args;
  rewrite_assign;
  flatten_block;
  reduce_block;
]

let simplify exp =
  counter := 0;
  rewrite_n rules exp
