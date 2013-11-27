open ExtHashtbl
open ExtList

(* Set of strings *)
module VarSet =
  struct
    include Set.Make(String)

    let from_list strings =
      List.fold_right add strings empty
  end

class ['a] symbolmap =
  object (self)
    val map : (string, 'a Stack.t) Hashtbl.t =
      Hashtbl.create 10
    method private get_stack name =
      match Hashtbl.find_option map name with
      | Some stack ->
          stack
      | None ->
          let stack = Stack.create () in
          Hashtbl.add map name stack;
          stack
    method add name value =
      Stack.push value (self#get_stack name)
    method rem name =
      let stack = self#get_stack name in
      ignore (Stack.pop stack)
    method get name =
      let stack = self#get_stack name in
      if Stack.is_empty stack then
        None
      else 
        Some (Stack.top stack)
  end

(* Iterate over all elements, calling function in between *)
let rec print_list print_fn sep = function
  | [] -> ()
  | [head] -> print_fn head
  | head::tail ->
      print_fn head;
      Format.printf sep; 
      print_list print_fn sep tail

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
