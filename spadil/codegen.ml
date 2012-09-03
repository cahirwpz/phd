open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts
open Format
open Utils

exception Error of string

let the_context = global_context ()
let the_module = create_module the_context "some-name"
let the_builder = builder the_context
let the_execution_engine = ExecutionEngine.create the_module
let the_fpm = PassManager.create_function the_module

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
    method add name value =
      begin
        try
          Hashtbl.add map name value
        with Not_found ->
          failwith (sprintf "Variable '%s' not found." name)
      end
  end

let structures = new structure_map
let values = new value_map
let functions = new value_map

let double_type = double_type the_context
let i1_type = i1_type the_context
let i8_type = i8_type the_context
let i32_type = i32_type the_context
let const_stringz = const_stringz the_context
let struct_type = struct_type the_context

(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca the_function var_name =
  let builder = builder_at the_context (instr_begin (entry_block the_function)) in
  build_alloca i32_type var_name builder

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
      build_load (values#get s) s the_builder
  | Ast.Block (vars, exps) ->
      List.iter (fun name ->
        let alloca = build_alloca i32_type name the_builder in
        values#add name alloca)
      (VarSet.elements vars);
      let values = List.map codegen exps in
      Utils.last values
  | Ast.Return x ->
      codegen x
  | Ast.Call ("+", [lhs; rhs]) ->
      build_add (codegen lhs) (codegen rhs) "addtmp" the_builder
  | Ast.Call ("-", [lhs; rhs]) ->
      build_sub (codegen lhs) (codegen rhs) "subtmp" the_builder
  | Ast.Call ("*", [lhs; rhs]) ->
      build_mul (codegen lhs) (codegen rhs) "multmp" the_builder
  | Ast.Call (name, args) ->
      let args = List.map codegen args in
      build_call (functions#get name) (Array.of_list args) "calltmp" the_builder
  | Ast.Global (name, None) ->
      declare_global i32_type name the_module
  | Ast.Assign (name, Ast.Lambda (args, body)) ->
      codegen_function name args body
  | Ast.Assign (name, value) ->
      build_store (codegen value) (values#get name) the_builder
  | _ ->
      const_int i32_type 0

and create_arguments the_function args =
  Array.iteri (fun i value ->
    let name = List.nth args i in
    set_value_name name value;
    values#add name value 
  ) (params the_function);

and create_argument_allocas the_function args =
  Array.iteri (fun i value ->
    let name = List.nth args i in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function name in

    (* Store the initial value into the alloca. *)
    ignore(build_store value alloca the_builder);

    values#add name alloca;
  ) (params the_function)

and codegen_function name args body =
  let args' = Array.make (List.length args) i32_type in
  let ft = function_type i32_type args' in
  let the_function = declare_function name ft the_module in
  create_arguments the_function args;
  (* Create a new basic block to start insertion into. *)
  begin
    position_at_end (append_block the_context "entry" the_function) the_builder;

    try
      create_argument_allocas the_function args;

      let ret_val = codegen body in

      (* Finish off the function. *)
      ignore(build_ret ret_val the_builder);

      (* Validate the generated code, checking for consistency. *)
      Llvm_analysis.assert_valid_function the_function;

      (* Optimize the function. *)
      ignore(PassManager.run_function the_function the_fpm);

      functions#add name the_function;

      the_function;
    with e ->
      delete_function the_function;
      raise e
  end

let codegen_safe il =
  try
    ignore (codegen il);
  with
  | Error s -> printf "@.%s@." s

let init () =
  structures#add "some" [i8_type] false;
  structures#add "char" [i8_type; i32_type] false;
  structures#add "int" [i8_type; i32_type] false;
  structures#add "bool" [i8_type; i1_type] false;
  structures#add "float" [i8_type; double_type] false;

  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  TargetData.add (ExecutionEngine.target_data the_execution_engine) the_fpm;

  (* Promote allocas to registers. *)
  add_memory_to_register_promotion the_fpm;

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

