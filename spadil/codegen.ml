open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "some-module"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen expr =
  raise (Error "Not implemented.")
