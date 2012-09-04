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

class valueStore = 
  object (self)
    val map : (string, llvalue Stack.t) Hashtbl.t = Hashtbl.create 10
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
      then failwith (sprintf "Cannot find %s var." name);
      Stack.top stack
  end

let structures = new structure_map
let values = new valueStore
let functions = new valueStore

let double_type = double_type the_context
let i1_type = i1_type the_context
let i8_type = i8_type the_context
let i32_type = i32_type the_context
let const_stringz = const_stringz the_context
let struct_type = struct_type the_context

let izero = const_int i32_type 0
let fzero = const_float double_type 0.0

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
      codegen_block (VarSet.elements vars) exps
  | Ast.Return x ->
      codegen x
  | Ast.Call (op, [exp]) when List.mem op Ast.unary ->
      codegen_unary_op op (codegen exp)
  | Ast.Call (op, [lhs; rhs]) when List.mem op Ast.binary ->
      codegen_binary_op op (codegen lhs) (codegen rhs)
  | Ast.Call (name, args) ->
      codegen_fun_call (Ast.literal_symbol name) (Array.of_list args)
  | Ast.IfThenElse (pred, t, f) ->
      codegen_if_then_else (codegen pred) t f
  | Ast.Global (name, None) ->
      let var = declare_global i32_type name the_module in
      values#add name var; var
  | Ast.Global (name, Some value) ->
      let var = define_global name (codegen value) the_module in
      values#add name var; var
  | Ast.Assign (name, Ast.Lambda (args, body)) ->
      codegen_function (Ast.literal_symbol name) (Array.of_list args) body
  | Ast.Assign (name, value) ->
      build_store (codegen value) (values#get name) the_builder
  | _ ->
      const_int i32_type 0

and create_arguments the_function args =
  Array.iteri (fun i value ->
    let name = args.(i) in
    set_value_name name value;
    values#add name value 
  ) (params the_function);

and create_argument_allocas the_function args =
  Array.iteri (fun i value ->
    let name = args.(i) in
    (* Create an alloca for this variable. *)
    let alloca = create_entry_block_alloca the_function name in

    (* Store the initial value into the alloca. *)
    ignore(build_store value alloca the_builder);

    values#add name alloca;
  ) (params the_function)

and codegen_block vars exps =
  let create_local_var name =
    values#add name (build_alloca i32_type name the_builder) in
  List.iter create_local_var vars;
  let last = Utils.last (List.map codegen exps) in
  List.iter (fun name -> values#rem name) vars;
  last

and codegen_unary_op op exp =
  match op with
  | "-" ->
      build_sub izero exp "neg_tmp" the_builder
  | "NOT" ->
      let one = const_int i1_type 1 in
      build_xor one exp "not_tmp" the_builder
  | _ ->
      failwith (sprintf "Unknown operator '%s'." op)

and codegen_binary_op op lhs rhs =
  match op with
  | "+" -> build_add lhs rhs "add_tmp" the_builder
  | "-" -> build_sub lhs rhs "sub_tmp" the_builder
  | "*" -> build_mul lhs rhs "mul_tmp" the_builder
  | "/" -> build_sdiv lhs rhs "sdiv_tmp" the_builder
  | "REM" -> build_srem rhs lhs "rem_tmp" the_builder
  | "AND" ->
      let lhs' = codegen_predicate lhs
      and rhs' = codegen_predicate rhs in
      build_and lhs' rhs' "and_tmp" the_builder
  | "OR" ->
      let lhs' = codegen_predicate lhs
      and rhs' = codegen_predicate rhs in
      build_or lhs' rhs' "or_tmp" the_builder
  | ">" -> build_icmp Icmp.Sgt lhs rhs "gt_tmp" the_builder
  | "<" -> build_icmp Icmp.Slt lhs rhs "lt_tmp" the_builder
  | ">=" -> build_icmp Icmp.Sge lhs rhs "ge_tmp" the_builder
  | "<=" -> build_icmp Icmp.Sle lhs rhs "le_tmp" the_builder
  | "=" -> build_icmp Icmp.Eq lhs rhs "eq_tmp" the_builder
  | "~=" -> build_icmp Icmp.Ne lhs rhs "ne_tmp" the_builder
  | _ ->
      failwith (sprintf "Unknown operator '%s'." op)

and codegen_fun_call name args =
  let args' = Array.map codegen args in
  build_call (functions#get name) args' "call_tmp" the_builder

and codegen_predicate pred =
  match type_of pred with
  | t when t = i32_type ->
      build_icmp Icmp.Ne pred izero "nz_tmp" the_builder
  | t when t = double_type ->
      build_fcmp Fcmp.Une pred fzero "fnz_tmp" the_builder
  | t when t = i1_type ->
      pred
  | _ -> failwith "Type not handled."

and codegen_if_then_else pred t f =
  (* Convert condition to a bool by comparing equal to 0. *)
  let cond_val = codegen_predicate pred in

  (* Grab the first block so that we might later add the conditional branch
   * to it at the end of the function. *)
  let start_bb = insertion_block the_builder in
  let the_function = block_parent start_bb in
  let else_bb = append_block the_context "else" the_function in
  let then_bb = append_block the_context "then" the_function in

  (* Emit 'then' value. *)
  position_at_end then_bb the_builder;
  let then_val = codegen t in

  (* Codegen of 'then' can change the current block, update then_bb for the
   * phi. We create a new name because one is used for the phi node, and the
   * other is used for the conditional branch. *)
  let new_then_bb = insertion_block the_builder in

  (* Emit 'else' value. *)
  position_at_end else_bb the_builder;
  let else_val = codegen f in

  (* Codegen of 'else' can change the current block, update else_bb for the
   * phi. *)
  let new_else_bb = insertion_block the_builder in

  (* Emit merge block. *)
  let merge_bb = append_block the_context "ifcont" the_function in
  position_at_end merge_bb the_builder;
  let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
  let phi = build_phi incoming "iftmp" the_builder in

  (* Return to the start block to add the conditional branch. *)
  position_at_end start_bb the_builder;
  ignore (build_cond_br cond_val then_bb else_bb the_builder);

  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'merge' block. *)
  position_at_end new_then_bb the_builder;
  ignore (build_br merge_bb the_builder);
  position_at_end new_else_bb the_builder;
  ignore (build_br merge_bb the_builder);

  (* Finally, set the builder to the end of the merge block. *)
  position_at_end merge_bb the_builder;

  phi

and codegen_function name args body =
  let args' = Array.make (Array.length args) i32_type in
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
