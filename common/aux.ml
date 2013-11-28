open ExtHashtbl
open ExtList

(* Set of strings *)
module VarSet =
  struct
    include Set.Make(String)

    let from_list strings =
      List.fold_right add strings empty
  end

(* Map from string to stack of 'a *)
module SymbolMap :
  sig
    type 'a t
    val create : int -> 'a t
    val add : 'a t -> string -> 'a -> unit
    val remove : 'a t -> string -> unit
    val get : 'a t -> string -> 'a option
  end = struct
    type 'a t = (string, 'a Stack.t) Hashtbl.t

    let get_stack map name =
      match Hashtbl.find_option map name with
      | Some stack -> stack
      | None ->
          let stack = Stack.create () in
          Hashtbl.add map name stack;
          stack

    let create size = Hashtbl.create size
    let add map name value =
      Stack.push value (get_stack map name)
    let remove map name =
      ignore (Stack.pop (get_stack map name))
    let get map name =
      let stack = get_stack map name in
      if Stack.is_empty stack then 
        None
      else
        Some (Stack.top stack)
  end

module ScopedList :
  sig
    type 'a t
    val create : unit -> 'a t
    val enter : 'a t -> unit
    val exit: 'a t -> 'a list
    val add : 'a t -> 'a -> unit
  end = struct
    type 'a t = ('a list) Stack.t

    let create () =
      let stack = Stack.create () in
      Stack.push [] stack; stack
    let enter stack =
      Stack.push [] stack
    let exit stack =
      Stack.pop stack
    let add stack item =
      let items = Stack.pop stack in
      Stack.push (item::items) stack
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
