(* Set of strings *)
module StringSet = Set.Make(String)

let makeStringSet strings =
  List.fold_right StringSet.add strings StringSet.empty

(* Iterate over all elements, calling function in between *)
let rec iter_join fn join_fn = function
  | [] -> ()
  | [head] -> fn head
  | head::tail ->
      fn head;
      join_fn (); 
      iter_join fn join_fn tail

(* Split list into two. First will have n elements. *)
let rec split_at n lst =
  split_at' n lst []
and split_at' n lst acc =
  if n > 0
  then split_at' (n - 1) (List.tl lst) (List.hd lst::acc)
  else (List.rev acc, lst)

(* Split a list by a given element. *)
let rec split_by elem lst =
  split_by' elem [] lst
and split_by' elem left = function
  | x::xs when x = elem -> (List.rev left, xs)
  | x::xs -> split_by' elem (x::left) xs
  | [] -> (List.rev left, [])

(* Return last element of a list. *)
let last lst =
  List.nth lst (List.length lst - 1)

let rec but_last = function
  | x::[] -> []
  | x::xs -> x::(but_last xs)
  | _ -> failwith "Expected list of at least one element."

let split_at_last xs = (but_last xs, last xs)
