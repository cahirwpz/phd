open Llvm
open Format

exception Error of string

let the_context = global_context ()
let the_module = create_module the_context "some-name"
let the_builder = builder the_context

class structure_map =
  object
    val map : (string, lltype) Hashtbl.t = Hashtbl.create 10
    method get name = Hashtbl.find map name
    method add name types packed =
      let the_struct = named_struct_type the_context name in
      struct_set_body the_struct (Array.of_list types) packed;
      Hashtbl.add map name the_struct
  end

class value_map = 
  object
    val map : (string, llvalue) Hashtbl.t = Hashtbl.create 10
    method get name = Hashtbl.find map name
    method add name value = Hashtbl.add map name value
  end

let structures = new structure_map
let values = new value_map

let double_type = double_type the_context
let i1_type = i1_type the_context
let i8_type = i8_type the_context
let i32_type = i32_type the_context
let const_stringz = const_stringz the_context
let struct_type = struct_type the_context

let rec codegen = function
  | Ast.Char c ->
      const_int i8_type (Char.code c)
  | Ast.Float n ->
      const_float double_type n
  | Ast.Int n ->
      const_int i32_type n
  | Ast.String s ->
      const_stringz s
  | Ast.Value s ->
      values#get s
  | Ast.Block (vars, exps) ->
      let values = List.map codegen exps in
      Utils.last values
  | Ast.Return x ->
      codegen x
  | Ast.Call ("+", [lhs; rhs]) ->
      let lhs_val = codegen lhs in
      let rhs_val = codegen rhs in
      build_add lhs_val rhs_val "addtmp" the_builder
  | Ast.Global (name, None) ->
      declare_global i32_type name the_module
  | Ast.Assign (name, Ast.Lambda (args, body)) ->
      let args' = Array.make (List.length args) i32_type in
      let ft = function_type i32_type args' in
      let the_function = declare_function name ft the_module in
      (* Create a new basic block to start insertion into. *)
      begin
        Array.iteri (fun i a ->
          let n = List.nth args i in
          set_value_name n a;
          values#add n a;
          ) (params the_function);

        let bb = append_block the_context "entry" the_function in
        position_at_end bb the_builder;

        try
          let ret_val = codegen body in

          (* Finish off the function. *)
          let _ = build_ret ret_val the_builder in

          (* Validate the generated code, checking for consistency. *)
          (* Llvm_analysis.assert_valid_function the_function;*)

          the_function

        with e ->
          delete_function the_function;
          raise e
      end
  | _ ->
      const_int i32_type 0

let codegen_safe il =
    try
      ignore (codegen il)
    with
    | Error s -> printf "@.%s@." s

let init () =
  structures#add "some" [i8_type] false;
  structures#add "char" [i8_type; i32_type] false;
  structures#add "int" [i8_type; i32_type] false;
  structures#add "bool" [i8_type; i1_type] false;
  structures#add "float" [i8_type; double_type] false
