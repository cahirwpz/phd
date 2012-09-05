open Printf

exception NameError of string
exception TypeError of string

class valueStore = 
  object (self)
    val map : (string, Llvm.llvalue Stack.t) Hashtbl.t = Hashtbl.create 10
    method private get_stack name =
      try
        Hashtbl.find map name
      with Not_found ->
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
      if Stack.is_empty stack
      then raise (NameError (sprintf "Cannot find '%s' variable." name));
      Stack.top stack
  end
