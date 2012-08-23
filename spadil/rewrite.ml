open Ast;;

exception NoMatch;;

let is_compound exp =
  match exp with
  | Char _ | Float _ | Int _ | String _ | Symbol _ | Label _ -> false
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
  | Assign (name, exp) -> Assign (name, r exp)
  | Block (vars, exps) -> Block (vars, rn exps)
  | Cons (fst, snd) -> Cons (r fst, r snd)
  | IfThen (pred, t) -> IfThen (r pred, r t)
  | IfThenElse (pred, t, f) -> IfThenElse (r pred, r t, r f)
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
  | Apply (fn, args) ->
      Apply (rewrite_compound fn, List.map rewrite_compound args)
  | IfThenElse (pred, t, f) ->
      IfThenElse (rewrite_compound pred, t, f)
  | IfThen (pred, t) ->
      IfThen (rewrite_compound pred, t)
  | Assign (name, Apply (fn, args)) ->
      let value = Apply (rewrite_compound fn, List.map rewrite_compound args)
      in Assign (name, value)
  | Assign (name, IfThenElse (pred, t, f)) ->
      let value = IfThenElse (rewrite_compound pred, t, f)
      in Assign (name, value)
  | e -> e

and rewrite_compound exp =
  match exp with
  | Apply (_, _) | IfThenElse (_, _, _) | Lambda (_, _) ->
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
      let body = List.map flatten_block body
      in Block (vars @ (collect_vars body), collect_exps body)
  | x -> x

and collect_vars = function
  | (Block (vs, _))::xs -> vs @ (collect_vars xs)
  | x::xs -> collect_vars xs
  | [] -> []
      
and collect_exps = function
  | (Block (_, x))::xs -> x @ (collect_exps xs)
  | x::xs -> x::(collect_exps xs)
  | [] -> []

(* If "jump $L; label $L" is in a block then remove it. *)
let rec reduce_spurious_jumps = function
  | Block (vars, exps) ->
      Block (vars, reduce_spurious_jump' exps)
  | _ -> raise NoMatch

and reduce_spurious_jump' = function
  | (Jump l1)::(Label l2)::rest when l1 = l2 ->
      reduce_spurious_jump' rest
  | x::xs ->
      x::(reduce_spurious_jump' xs)
  | [] -> []

(* Reduce one-time lambda invocations *)
let rec reduce_lambda = function
  | Apply (Lambda ([], body), []) ->
      body
  | _ -> raise NoMatch

let rules = [
  reduce_lambda;
  extract_compound_args;
  flatten_block;
  reduce_spurious_jumps;
  reduce_block;
]

let simplify exp =
  counter := 0;
  rewrite_n rules exp
