open ExtList

(* Set of strings *)
module VarSet =
  struct
    include Set.Make(String)

    let from_list strings =
      List.fold_right add strings empty
  end

(* Iterate over all elements, calling function in between *)
let rec iter_join fn join_fn = function
  | [] -> ()
  | [head] -> fn head
  | head::tail ->
      fn head;
      join_fn (); 
      iter_join fn join_fn tail

(* [x; x; y; x; x; y; x; y] => [[x; x]; [y; x; x]; [y; x]; [y]] *)
let slice_with fn lst =
  let rec slice_with' fn acc = (function
    | x::xs when fn(x) ->
        if List.length acc > 0 then
          (List.rev acc)::(slice_with' fn [x] xs)
        else
          slice_with' fn [x] xs
    | x::xs ->
        slice_with' fn (x::acc) xs
    | [] -> [List.rev acc])
  in
    slice_with' fn [] lst

let rec but_last = function
  | [x] -> []
  | x::xs -> x::(but_last xs)
  | _ -> failwith "Expected list of at least one element."

let split_at_last xs = (but_last xs, List.last xs)
