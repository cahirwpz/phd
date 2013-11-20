open ExtHashtbl
open Printf

class symbolmap =
  object (self)
    val map : (string, (Ast.spadtype * Llvm.llvalue) Stack.t) Hashtbl.t =
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
