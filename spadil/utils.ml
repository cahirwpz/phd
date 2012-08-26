(* Set of strings *)
module StringSet = Set.Make(String)

let makeStringSet strings =
  List.fold_right StringSet.add strings StringSet.empty

(* Some helpful predicates and functions *)
let rec split n lst =
  split' n lst []
and split' n lst acc =
  if n > 0
  then split' (n - 1) (List.tl lst) (List.hd lst::acc)
  else (List.rev acc, lst)

let rec last = function
  | x::[] -> x
  | x::xs -> last xs
  | _ -> failwith "Empty list cannot have a last element."

let rec but_last = function
  | x::[] -> []
  | x::xs -> x::(but_last xs)
  | _ -> failwith "Expected list of at least one element."

let split_at_last xs = (but_last xs, last xs)
