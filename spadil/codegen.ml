open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "some-module"
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let i8_type = i8_type context
let i32_type = i32_type context
let const_stringz = const_stringz context

let rec codegen = function
  | Ast.Char c ->
      const_int i8_type (Char.code c)
  | Ast.Float n ->
      const_float double_type n
  | Ast.Int n ->
      const_int i32_type n
  | Ast.String s ->
      const_stringz s
  | _ ->
      raise (Error "Not implemented.")
