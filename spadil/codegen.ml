open Codegen_dt
open Codegen_base
open ExtList
open Printf 
open Utils

class counter =
  object
    val mutable counter = 0
    method get =
      counter <- counter + 1; string_of_int counter
    method reset =
      counter <- 0
  end

let unique = new counter

let cast_SI builder v =
  match Llvm.type_of v with
  | t when t = gen_type ->
      builder#build_call_fn "unbox_SI" [| v |]
  | t when t = i32_type ->
      v
  | t ->
      raise @@ CoerceError (v, "SI")

let cast_DF builder v =
  match Llvm.type_of v with
  | t when t = gen_type ->
      builder#build_call_fn "unbox_DF" [| v |]
  | t when t = double_type ->
      v
  | t ->
      raise @@ CoerceError (v, "DF")

let cast_GEN builder v = 
  match Llvm.type_of v with
  | t when t = i32_type ->
      builder#build_call_fn "box_SI" [| v |]
  | t when t = gen_type ->
      v
  | t when t = void_type ->
      const_pointer_null
  | t ->
      raise @@ CoerceError (v, "GEN")

let cast_BOOL builder cond =
  match Llvm.type_of cond with
  | t when t = i32_type ->
      builder#build_icmp Icmp.Ne cond izero
  | t when t = double_type ->
      builder#build_fcmp Fcmp.Une cond fzero
  | t when t = i1_type ->
      cond
  | t -> 
      raise @@ CoerceError (cond, "BOOL")

let cast builder (value, to_type) =
  match to_type with
  | t when t = i32_type ->
      cast_SI builder value
  | t when t = double_type ->
      cast_DF builder value
  | t when t = gen_type ->
      cast_GEN builder value
  | t -> 
      raise @@ CoerceError (value, Llvm.string_of_lltype to_type)

let rec translate_type = function
  | Ast.Boolean -> i1_type
  | Ast.DF -> double_type
  | Ast.SI -> i32_type
  | Ast.Exit
  | Ast.Void -> void_type
  | Ast.Array _
  | Ast.Cons _
  | Ast.UserType _
  | Ast.Generic -> gen_type
  | Ast.Mapping ts ->
      let ts = List.map translate_type ts in
      Llvm.function_type (List.last ts) (Array.of_list @@ but_last ts)

(* Code generation starts here *)
let rec codegen builder exp =
  let codegen = codegen builder in
  let codegen_some exp = Option.get (codegen exp) in 
  match exp with
  | Ast.Symbol "T" ->
      Some (const_int i1_type 1)
  | Ast.Symbol "NIL" ->
      Some (const_int i1_type 0)
  | Ast.Char c ->
      Some (const_int i8_type (Char.code c))
  | Ast.Float n ->
      Some (const_float double_type n)
  | Ast.Int n ->
      Some (const_int i32_type n)
  | Ast.String s ->
      Some (const_stringz s)
  | Ast.Var name when name = "NIL" ->
      Some (const_pointer_null)
  | Ast.Var name ->
      Some (builder#build_load_var name)
  | Ast.Block (vars, exps) ->
      codegen_block builder vars exps
  | Ast.UnaryOp (op, exp) ->
      Some (codegen_unary_op builder op (codegen_some exp))
  | Ast.BinaryOp (op, lhs, rhs) ->
      Some (codegen_binary_op builder op (codegen_some lhs) (codegen_some rhs))
  | Ast.Call (name, args) ->
      let name = Ast.literal_symbol name in
      let fn = builder#lookup_function name in
      let args = List.map codegen_some args in
      let types = List.map Llvm.type_of (Array.to_list @@ Llvm.params fn) in
      let typed_args = Array.of_list (List.combine args types) in
      Some (builder#build_call_fn name (Array.map (cast builder) typed_args))
  | Ast.IfThenElse (cond, t, f) ->
      codegen_if_then_else builder (codegen_some cond) t f
  | Ast.IfThen (cond, t) ->
      ignore(codegen_if_then builder (codegen_some cond) t);
      None
  | Ast.Loop body ->
      ignore(codegen_loop builder body);
      None
  | Ast.While (cond, body) ->
      ignore(codegen_while builder cond body);
      None
  | Ast.Assign (Ast.Var name, value) ->
      let value = codegen_some value in
      let var = builder#lookup_var name in
      let value_type = Llvm.element_type @@ Llvm.type_of var in
      let value = cast builder (value, value_type) in
      builder#build_store_var value name;
      None
  | Ast.Assign (Ast.ArrayRef (vector, index), value) ->
      let value = codegen_some value
      and index = codegen_some index
      and vector = codegen_some @@ Ast.Call ("QVREF", [vector]) in
      let item = builder#build_gep vector index in
      builder#build_store (cast_GEN builder value) item;
      None
  | Ast.ArrayRef (vector, index) ->
      let index = codegen_some index
      and vector = codegen_some @@ Ast.Call ("QVREF", [vector]) in
      let item = builder#build_gep vector index in
      Some (builder#build_load item (Llvm.value_name vector))
  | Ast.Return value ->
      let ret_val = codegen_some value in
      builder#add_ret_bb ret_val builder#insertion_block;
      let after_bb = builder#append_block "after" builder#function_block in
      builder#position_at_end after_bb;
      None
  | x ->
      Ast.print x; print_char '\n'; failwith "not handled"

and codegen_var_intro builder (name, atype) =
  ignore(builder#var_intro name atype (translate_type atype))

and codegen_block builder tvars exps =
  let codegen = codegen builder in
  List.iter (codegen_var_intro builder) tvars;
  let last = List.last (List.map codegen exps) in
  List.iter (fun tvar -> builder#var_forget (fst tvar)) tvars;
  last

and codegen_unary_op builder op exp =
  let cast_BOOL = cast_BOOL builder in
  match op with
  | "-" -> builder#build_neg exp
  | "NOT" -> builder#build_not @@ cast_BOOL exp
  | "abs_DF" -> builder#build_call_builtin "llvm.fabs.f64" [| exp |]
  | _ -> raise (NameError (sprintf "Unknown operator '%s'." op))

and codegen_binary_op builder op lhs rhs =
  let cast_SI = cast_SI builder
  and cast_DF = cast_DF builder in
  match op with
  (* SingleInteger *)
  | "add_SI" -> builder#build_add (cast_SI lhs) (cast_SI rhs)
  | "sub_SI" -> builder#build_sub (cast_SI lhs) (cast_SI rhs)
  | "mul_SI" -> builder#build_mul (cast_SI lhs) (cast_SI rhs)
  | "quo_SI" -> builder#build_sdiv (cast_SI lhs) (cast_SI rhs)
  | "rem_SI" -> builder#build_srem (cast_SI lhs) (cast_SI rhs)
  | "greater_SI" -> builder#build_icmp Icmp.Sgt (cast_SI lhs) (cast_SI rhs)
  | "less_SI" -> builder#build_icmp Icmp.Slt (cast_SI lhs) (cast_SI rhs)
  | "eql_SI" -> builder#build_icmp Icmp.Eq (cast_SI lhs) (cast_SI rhs)
  (* DoubleFloat *)
  | "add_DF" -> builder#build_fadd (cast_DF lhs) (cast_DF rhs)
  | "sub_DF" -> builder#build_fsub (cast_DF lhs) (cast_DF rhs)
  | "mul_DF" -> builder#build_fmul (cast_DF lhs) (cast_DF rhs)
  | "div_DF" -> builder#build_fdiv (cast_DF lhs) (cast_DF rhs)
  | "greater_DF" -> builder#build_fcmp Fcmp.Ogt (cast_DF lhs) (cast_DF rhs)
  | "less_DF" -> builder#build_fcmp Fcmp.Olt (cast_DF lhs) (cast_DF rhs)
  | "eql_DF" -> builder#build_fcmp Fcmp.Oeq (cast_DF lhs) (cast_DF rhs)
  (*
  | "REM" -> builder#build_srem rhs lhs
  | "AND" -> builder#build_and (cast_BOOL lhs) (cast_BOOL rhs)
  | "OR" -> builder#build_or (cast_BOOL lhs) (cast_BOOL rhs)
  | ">=" -> builder#build_icmp Icmp.Sge lhs rhs
  | "<=" -> builder#build_icmp Icmp.Sle lhs rhs
  | "~=" -> builder#build_icmp Icmp.Ne lhs rhs
  *)
  | _ -> raise (NameError (sprintf "Unknown operator '%s'." op))

(* 'if-then' construct is always a statement. *)
and codegen_if_then builder cond t =
  let codegen = codegen builder in
  (* Convert condition to a bool by comparing equal to 0. *)
  let cond_val = cast_BOOL builder cond in
  let i = unique#get in

  (* Grab the first block so that we might later add the conditional branch
   * to it at the end of the function. *)
  let start_bb = builder#insertion_block in
  let the_function = Llvm.block_parent start_bb in
  let then_bb = builder#append_block ("then." ^ i) the_function in

  (* Emit 'then' value. *)
  builder#position_at_end then_bb;
  ignore(codegen t);

  (* Codegen of 'then' can change the current block, update then_bb for the
   * phi. We create a new name because one is used for the phi node, and the
   * other is used for the conditional branch. *)
  let new_then_bb = builder#insertion_block in

  (* Emit 'endif' block. *)
  let endif_bb = builder#append_block ("endif." ^ i) the_function in
  builder#position_at_end endif_bb;

  ignore(builder#build_phi [(iundef, new_then_bb); (iundef, start_bb)]);

  (* Return to the start block to add the conditional branch. *)
  builder#position_at_end start_bb;
  builder#build_cond_br cond_val then_bb endif_bb;

  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'endif' block. *)
  builder#position_at_end new_then_bb;
  ignore (builder#build_br endif_bb);

  (* Finally, set the builder to the end of the endif block. *)
  builder#position_at_end endif_bb;

  None

(* 'if-then-else' may be a statement if both branches return values. We silently
 * assume both values will be of the same type. *)
and codegen_if_then_else builder cond t f =
  let codegen = codegen builder in
  (* Convert condition to a bool by comparing equal to 0. *)
  let cond_val = cast_BOOL builder cond in
  let i = unique#get in

  (* Grab the first block so that we might later add the conditional branch
   * to it at the end of the function. *)
  let start_bb = builder#insertion_block in
  let the_function = Llvm.block_parent start_bb in
  let then_bb = builder#append_block ("then." ^ i) the_function in
  let else_bb = builder#append_block ("else." ^ i) the_function in

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

  (* Emit 'endif' block. *)
  let endif_bb = builder#append_block ("endif." ^ i) the_function in
  builder#position_at_end endif_bb;

  let phi_has_value = Option.is_some then_val && Option.is_some else_val in

  let then_val = (if phi_has_value then Option.get then_val else iundef)
  and else_val = (if phi_has_value then Option.get else_val else iundef) in
  let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in

  (* FIXME: check if type_of(then_val) equals to type_of(else_val) *)
  (* dump_value then_val; dump_value else_val; *)
  let phi = builder#build_phi incoming in

  (* Return to the start block to add the conditional branch. *)
  builder#position_at_end start_bb;
  ignore (builder#build_cond_br cond_val then_bb else_bb);

  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'endif' block. *)
  builder#position_at_end new_then_bb;
  ignore (builder#build_br endif_bb);
  builder#position_at_end new_else_bb;
  ignore (builder#build_br endif_bb);

  (* Finally, set the builder to the end of the endif block. *)
  builder#position_at_end endif_bb;

  if phi_has_value then Some phi else None

(* 'loop' construct is always a statement. *)
and codegen_loop builder body =
  let codegen = codegen builder in
  let i = unique#get in

  let start_bb = builder#insertion_block in
  let the_function = Llvm.block_parent start_bb in
  let loop_body_bb = builder#append_block ("loop." ^ i) the_function in

  (* Terminate predecessor block with uncoditional jump to loop. *)
  builder#position_at_end start_bb;
  ignore (builder#build_br loop_body_bb);
  builder#position_at_end loop_body_bb;
  ignore(codegen body);
  builder#build_br loop_body_bb;
  None

(* 'while' construct is always a statement. *)
and codegen_while builder cond body =
  let codegen = codegen builder in
  let codegen_some exp = Option.get (codegen exp) in 
  let i = unique#get in

  let start_bb = builder#insertion_block in
  let the_function = Llvm.block_parent start_bb in
  let loop_begin_bb = builder#append_block ("while." ^ i) the_function in

  (* Terminate predecessor block with uncoditional jump to loop. *)
  builder#position_at_end start_bb;
  ignore (builder#build_br loop_begin_bb);
  builder#position_at_end loop_begin_bb;

  (* Encode loop exit condition check. *)
  let cond_val = codegen_some cond in

  let loop_body_bb = builder#append_block ("iter." ^ i) the_function in

  builder#position_at_end loop_body_bb;
  ignore(codegen body);
  builder#build_br loop_begin_bb;

  let loop_end_bb = builder#append_block ("endwhile." ^ i) the_function in

  builder#position_at_end loop_begin_bb;
  ignore (builder#build_cond_br cond_val loop_body_bb loop_end_bb);
  builder#position_at_end loop_end_bb;
  None

let rec codegen_toplevel pkg tree =
  try
    match tree with
    (*
    | Ast.Global (name, None) ->
        Some (pkg#declare_global gen_type name)
    | Ast.Global (name, Some value) ->
        Some (pkg#define_global name (codegen pkg#new_builder value))
    *)
    | Ast.Assign (Ast.Var name, Ast.Lambda (args, fn_type, body)) ->
        let name = Ast.literal_symbol name
        and args = Array.of_list args in
        let fn = codegen_typed_function_decl pkg name args fn_type in
        begin
          try
            Some (codegen_function_def pkg fn args body)
          with e ->
            Printexc.print_backtrace stderr; Llvm.delete_function fn; raise e
        end
    | _ ->
        print_string "Syntax Error: Not a toplevel construction.\n";
        Ast.print tree;
        None
  with
  | CoerceError (v, t) ->
      dump_value v;
      printf "Coerce Error: cannot coerce to '%s'!\n" t;
      Printexc.print_backtrace stderr;
      None
  | NameError s ->
      printf "Name Error: %s\n" s;
      Printexc.print_backtrace stderr;
      None
  | TypeError s ->
      printf "Type Error: %s\n" s;
      Printexc.print_backtrace stderr;
      None

and codegen_typed_function_decl pkg name args fn_type =
  (* Declare function type. *)
  let fn_lltype = translate_type fn_type in
  (* Create the function declaration and add it to the module. *)
  let fn = pkg#declare_function name fn_type fn_lltype in
  (* Specify argument parameters. *)
  let set_param_name = (fun i value -> Llvm.set_value_name args.(i) value) in
  Array.iteri set_param_name (Llvm.params fn);
  fn

and codegen_function_def pkg fn args body =
  let builder = pkg#new_function_builder (Llvm.value_name fn) in

  (* Create a new basic block to start insertion into. *)
  let entry_bb = builder#append_block "entry" fn in
  builder#position_at_end entry_bb;

  (* Create an alloca instruction in the entry block of the function. This
   * is used for mutable variables etc. *)
  let fn_lltype = Llvm.get_function_type fn in
  let types = Llvm.param_types fn_lltype in
  Array.iteri (fun i value ->
    let name = args.(i)
    and arg_type = builder#arg_type i
    and arg_lltype = types.(i) in

    (* Create an alloca for this variable. *)
    ignore(builder#var_intro name arg_type arg_lltype);

    (* Store the initial value into the alloca. *)
    ignore(builder#build_store value (builder#lookup_var name));
    ) (Llvm.params fn);

  (* Finish off the function. *)
  let body_val = codegen builder body in
  let body_end_bb = builder#insertion_block in

  (match builder#ret_type with
  | Ast.Void ->
      ignore(builder#build_ret_void)
  | _ ->
      let returning_bbs = builder#list_ret_bb in
      if List.length returning_bbs > 0 then
        begin
          (* Create merge bb for all returning bbs *)
          let return_bb = builder#append_block "return" fn in
          builder#position_at_end return_bb;
          (* Use phi to merge all returning branches. *)
          let incoming = (
            match body_val with
            | Some value ->
                [(value, body_end_bb)] @ returning_bbs
            | None ->
                returning_bbs) in
          List.iter (fun (_, bb) ->
            builder#position_at_end bb; ignore(builder#build_br return_bb)) incoming;
          builder#position_at_end return_bb;
          let ret_val = builder#build_phi incoming in
          ignore(builder#build_ret ret_val)
        end
      else
        ignore(builder#build_ret (Option.get body_val)));

  (* Validate the generated code, checking for consistency. *)
  Llvm_analysis.assert_valid_function fn;

  (* Return defined function. *)
  fn
