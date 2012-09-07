open Codegen_dt
open Codegen_base
open Printf 
open Utils

let cast_to_bool builder cond =
  match Llvm.type_of cond with
  | t when t = i32_type ->
      builder#build_icmp Icmp.Ne cond izero
  | t when t = double_type ->
      builder#build_fcmp Fcmp.Une cond fzero
  | t when t = i1_type ->
      cond
  | _ -> raise (TypeError "Type not handled.")

(* Code generation starts here *)
let rec codegen builder exp =
  let codegen = codegen builder in
  match exp with
  | Ast.Char c ->
      const_int i8_type (Char.code c)
  | Ast.Float n ->
      const_float double_type n
  | Ast.Int n ->
      const_int i32_type n
  | Ast.String s ->
      const_stringz s
  | Ast.Value name ->
      builder#build_load name
  | Ast.Block (vars, exps) ->
      codegen_block builder (VarSet.elements vars) exps
  | Ast.Return x ->
      codegen x
  | Ast.Call (op, [exp]) when List.mem op Ast.unary ->
      codegen_unary_op builder op (codegen exp)
  | Ast.Call (op, [lhs; rhs]) when List.mem op Ast.binary ->
      codegen_binary_op builder op (codegen lhs) (codegen rhs)
  | Ast.Call (name, args) ->
      codegen_fun_call builder (Ast.literal_symbol name) (Array.of_list args)
  | Ast.IfThenElse (cond, t, f) ->
      codegen_if_then_else builder (codegen cond) t f
  | Ast.While (cond, body) ->
      codegen_while builder cond body
  | Ast.Assign (name, value) ->
      builder#build_store (codegen value) name
  | _ ->
      const_int i32_type 0

and codegen_block builder vars exps =
  let codegen = codegen builder in
  let create_local_var name = ignore (builder#var_intro i32_type name) in
  List.iter create_local_var vars;
  let last = Utils.last (List.map codegen exps) in
  List.iter builder#var_forget vars;
  last

and codegen_unary_op builder op exp =
  match op with
  | "-" -> builder#build_neg exp
  | "NOT" -> builder#build_not (cast_to_bool builder exp)
  | _ -> raise (NameError (sprintf "Unknown operator '%s'." op))

and codegen_binary_op builder op lhs rhs =
  let cast_to_bool = cast_to_bool builder in
  match op with
  | "+" -> builder#build_add lhs rhs
  | "-" -> builder#build_sub lhs rhs
  | "*" -> builder#build_mul lhs rhs
  | "/" -> builder#build_sdiv lhs rhs
  | "REM" -> builder#build_srem rhs lhs
  | "AND" -> builder#build_and (cast_to_bool lhs) (cast_to_bool rhs)
  | "OR" -> builder#build_or (cast_to_bool lhs) (cast_to_bool rhs)
  | ">" -> builder#build_icmp Icmp.Sgt lhs rhs
  | "<" -> builder#build_icmp Icmp.Slt lhs rhs
  | ">=" -> builder#build_icmp Icmp.Sge lhs rhs
  | "<=" -> builder#build_icmp Icmp.Sle lhs rhs
  | "=" -> builder#build_icmp Icmp.Eq lhs rhs
  | "~=" -> builder#build_icmp Icmp.Ne lhs rhs
  | _ -> raise (NameError (sprintf "Unknown operator '%s'." op))

and codegen_fun_call builder name args =
  let codegen = codegen builder in
  let args = Array.map codegen args in
  builder#build_call name args

and codegen_if_then_else builder cond t f =
  let codegen = codegen builder in
  (* Convert condition to a bool by comparing equal to 0. *)
  let cond_val = cast_to_bool builder cond in

  (* Grab the first block so that we might later add the conditional branch
   * to it at the end of the function. *)
  let start_bb = builder#insertion_block in
  let the_function = Llvm.block_parent start_bb in
  let then_bb = builder#append_block "then" the_function in
  let else_bb = builder#append_block "else" the_function in

  (* Emit 'then' value. *)
  builder#position_at_end then_bb;
  let then_val = codegen t in

  (* Codegen of 'then' can change the current block, update then_bb for the
   * phi. We create a new name because one is used for the phi node, and the
   * other is used for the conditional branch. *)
  let new_then_bb = builder#insertion_block in

  (* Emit 'else' value. *)
  builder#position_at_end else_bb;
  let else_val = codegen f in

  (* Codegen of 'else' can change the current block, update else_bb for the
   * phi. *)
  let new_else_bb = builder#insertion_block in

  (* Emit merge block. *)
  let merge_bb = builder#append_block "ifcont" the_function in
  builder#position_at_end merge_bb;
  let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
  let phi = builder#build_phi incoming in

  (* Return to the start block to add the conditional branch. *)
  builder#position_at_end start_bb;
  ignore (builder#build_cond_br cond_val then_bb else_bb);

  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'merge' block. *)
  builder#position_at_end new_then_bb;
  ignore (builder#build_br merge_bb);
  builder#position_at_end new_else_bb;
  ignore (builder#build_br merge_bb);

  (* Finally, set the builder to the end of the merge block. *)
  builder#position_at_end merge_bb;

  phi

and codegen_while builder cond body =
  let start_bb = builder#insertion_block in
  let the_function = Llvm.block_parent start_bb in
  let loop_bb = builder#append_block "loop" the_function in
  let body_bb = builder#append_block "body" the_function in
  let end_loop_bb = builder#append_block "end_loop" the_function in

  (* Terminate predecessor block with uncoditional jump to loop. *)
  builder#position_at_end start_bb;
  ignore (builder#build_br loop_bb);

  builder#position_at_end loop_bb;
  let cond_val = codegen builder cond in
  ignore (builder#build_cond_br cond_val body_bb end_loop_bb);

  builder#position_at_end body_bb;
  ignore (codegen builder body);
  ignore (builder#build_br loop_bb);

  builder#position_at_end end_loop_bb;
  izero

let rec codegen_toplevel pkg tree =
  try
    match tree with
    | Ast.Global (name, None) ->
        Some (pkg#declare_global i32_type name)
    | Ast.Global (name, Some value) ->
        let bdr = pkg#new_builder in
        Some (pkg#define_global name (codegen bdr value))
    | Ast.Assign (name, Ast.Lambda (args, body)) ->
        let name = Ast.literal_symbol name
        and args = Array.of_list args in
        let fn = codegen_function_decl pkg name args in
        begin
          try
            let bdr = pkg#new_builder in
            Some (codegen_function_def bdr fn args body)
          with e ->
            Llvm.delete_function fn; raise e
        end
    | _ ->
        print_string "Syntax Error: Not a toplevel construction.\n";
        None
  with
  | NameError s ->
      printf "Name Error: %s\n" s;
      None
  | TypeError s ->
      printf "Type Error: %s\n" s;
      None

and codegen_function_decl pkg name args =
  (* Declare function type. *)
  let args_t = Array.make (Array.length args) i32_type
  and return_t = i32_type in
  let fn_type = Llvm.function_type return_t args_t in
  (* Create the function declaration and add it to the module. *)
  let fn = pkg#declare_function name fn_type in
  (* Specify argument parameters. *)
  let set_param_name = (fun i value -> Llvm.set_value_name args.(i) value) in
  Array.iteri set_param_name (Llvm.params fn);
  fn

and codegen_function_def builder fn args body =
  (* Create a new basic block to start insertion into. *)
  let entry_bb = builder#append_block "entry" fn in
  builder#position_at_end entry_bb;

  (* Create an alloca instruction in the entry block of the function. This
   * is used for mutable variables etc. *)
  Array.iteri (fun i value ->
    let name = args.(i) in

    (* Create an alloca for this variable. *)
    ignore(builder#var_intro i32_type name);

    (* Store the initial value into the alloca. *)
    ignore(builder#build_store value name);
    ) (Llvm.params fn);

  (* Finish off the function. *)
  let body_val = codegen builder body in
  ignore(builder#build_ret body_val);

  (* Validate the generated code, checking for consistency. *)
  Llvm_analysis.assert_valid_function fn;

  (* Return defined function. *)
  fn
