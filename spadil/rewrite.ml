open Ast
open List
open Utils

exception NoMatch

let symgen = new Symbol.gen "@%d"

(* ast rewrite engine *)
let rec rewrite rule e =
  let r = apply rule and rn = map (apply rule) in
  match e with
  | Apply (fn, exps) -> Apply (r fn, rn exps)
  | Assign (name, exp) -> Assign (name, r exp)
  | Block (vars, exps) -> Block (vars, rn exps)
  | Cons (fst, snd) -> Cons (r fst, r snd)
  | IfThen (pred, t) -> IfThen (r pred, r t)
  | IfThenElse (pred, t, f) -> IfThenElse (r pred, r t, r f)
  | Lambda (args, exp) -> Lambda (args, r exp)
  | Return exp -> Return (r exp)
  | While (pred, exp) -> While (r pred, r exp)
  | e -> e

and apply rule exp =
  let exp = (try (rule exp) with NoMatch -> exp)
  in rewrite rule exp

let rec rewrite_n ruleset exp =
  match ruleset with
  | rule::rules -> rewrite_n rules (rewrite rule exp)
  | [] -> exp

(* Reduce a block that contains only one expression. *)
let reduce_block = function
  | Block (vars1, [Block (vars2, exps)]) ->
      Block (VarSet.union vars1 vars2, exps)
  | Block (vars, [exp]) when VarSet.is_empty vars ->
      exp
  | _ -> raise NoMatch

(* Flatten structure of a block. *)
let rec flatten_block = function
  | Block (vars, body) ->
      let body = map flatten_block body in
      let vars = VarSet.union vars (collect_vars body) in
      Block (vars, collect_exps body)
  | x -> x

and collect_vars = function
  | (Block (vs, _))::xs -> VarSet.union vs (collect_vars xs)
  | x::xs -> collect_vars xs
  | [] -> VarSet.empty
      
and collect_exps = function
  | (Block (_, x))::xs -> x @ (collect_exps xs)
  | x::xs -> x::(collect_exps xs)
  | [] -> []

(* Reduce one-time lambda invocations *)
let rec reduce_lambda exp =
  match exp with
  | Apply (Lambda (vs, body), args) when length vs = length args ->
      let assigns = map2 (fun n v -> Assign (n, v)) vs args in
      Block (VarSet.from_list vs, assigns @ [body])
  | _ -> raise NoMatch

(* push assign deeper into the structure *)
let rec rewrite_assign = function
  | Assign (var, IfThenElse (pred, t, f)) ->
      let t = rewrite_assign (Assign (var, t))
      and f = rewrite_assign (Assign (var, f))
      in IfThenElse (pred, t, f)
  | Assign (var, Block (vars, exps)) when not (VarSet.mem var vars) ->
      let (xs, x) = split_at_last exps
      in Block (vars, xs @ [rewrite_assign (Assign (var, x))])
  | Assign (var1, Assign (var2, exp)) ->
      Block (VarSet.empty, [Assign (var2, exp); Assign (var1, Symbol var2)])
  | x -> x

(* push return deeper into the structure *)
let rec rewrite_return = function
  | Return (Block (vars, exps)) ->
      let (xs, x) = split_at_last exps
      in Block (vars, xs @ [rewrite_return (Return x)])
  | Return (IfThenElse (p, t, f)) ->
      IfThenElse (p, rewrite_return (Return t), rewrite_return (Return f))
  | x -> x

(*
 * 1) p or return(x) => if not(p) then return(x)
 * 2) p and return(x) => if p then return(x)
 *)
let rec rewrite_logic_abbrev = function
  | Apply (Symbol "OR", [x; Return y]) ->
      IfThen (Apply (Symbol "NOT", [x]), Return y)
  | Apply (Symbol "AND", [x; Return y]) ->
      IfThen (x, Return y)
  | _ -> raise NoMatch

let rules = [
  reduce_lambda;
  rewrite_return;
  rewrite_logic_abbrev;
  rewrite_assign;
  flatten_block;
  reduce_block;
]

let simplify exp =
  symgen#reset;
  rewrite_n rules exp
