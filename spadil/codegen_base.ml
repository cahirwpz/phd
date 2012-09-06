open Codegen_dt
open Printf

exception NameError of string
exception TypeError of string

module Icmp = Llvm.Icmp
module Fcmp = Llvm.Fcmp
module Jit = Llvm_executionengine.ExecutionEngine

let string_of_icmp = function
  | Icmp.Eq -> "eq"
  | Icmp.Ne -> "ne"
  | Icmp.Ugt -> "ugt"
  | Icmp.Uge -> "uge"
  | Icmp.Ult -> "ult"
  | Icmp.Ule -> "ule"
  | Icmp.Sgt -> "sgt"
  | Icmp.Sge -> "sge"
  | Icmp.Slt -> "slt"
  | Icmp.Sle -> "sle"

let string_of_fcmp = function
  | Fcmp.False -> "false"
  | Fcmp.Oeq -> "oeq"
  | Fcmp.Ogt -> "ogt"
  | Fcmp.Oge -> "oge"
  | Fcmp.Olt -> "olt"
  | Fcmp.Ole -> "ole"
  | Fcmp.One -> "one"
  | Fcmp.Ord -> "ord"
  | Fcmp.Uno -> "uno"
  | Fcmp.Ueq -> "ueq"
  | Fcmp.Ugt -> "ugt"
  | Fcmp.Uge -> "uge"
  | Fcmp.Ult -> "ult"
  | Fcmp.Ule -> "ule"
  | Fcmp.Une -> "une"
  | Fcmp.True -> "true"

class code_builder pkg =
  let ctx = pkg#get_context in
  object (self)
    val builder = Llvm.builder ctx
    val package = pkg
    val context = ctx
    val locals = new variables

    (* Basic types. *)
    val double_type = Llvm.double_type ctx
    val i1_type = Llvm.i1_type ctx
    val i8_type = Llvm.i8_type ctx
    val i32_type = Llvm.i32_type ctx
    val const_stringz = Llvm.const_stringz ctx
    val struct_type = Llvm.struct_type ctx

    (* Type modifiers. *)
    val const_int = Llvm.const_int
    val const_float = Llvm.const_float

    method append_block name value =
      Llvm.append_block context name value
    method insertion_block =
      Llvm.insertion_block builder
    method position_at_end block =
      Llvm.position_at_end block builder

    (* Control flow. *)
    method build_call (name : string) args =
      Llvm.build_call (package#lookup_function name) args "call_tmp" builder
    method build_phi incoming =
      Llvm.build_phi incoming "if_tmp" builder
    method build_cond_br cond then_bb else_bb =
      Llvm.build_cond_br cond then_bb else_bb builder
    method build_br dest_bb =
      Llvm.build_br dest_bb builder
    method build_ret value =
      Llvm.build_ret value builder

    (* Memory access instructions. *)
    method build_load name = 
      Llvm.build_load (self#lookup_var name) name builder
    method build_store src name =
      Llvm.build_store src (self#lookup_var name) builder

    (* Arithmetic instructions. *)
    method build_add lhs rhs = 
      Llvm.build_add lhs rhs "add_tmp" builder
    method build_sub lhs rhs =
      Llvm.build_sub lhs rhs "sub_tmp" builder
    method build_mul lhs rhs =
      Llvm.build_mul lhs rhs "mul_tmp" builder
    method build_sdiv lhs rhs =
      Llvm.build_sdiv lhs rhs "sdiv_tmp" builder
    method build_srem lhs rhs =
      Llvm.build_srem rhs lhs "srem_tmp" builder
    method build_neg exp = 
      Llvm.build_sub (const_int i32_type 0) exp "neg_tmp" builder

    (* Logic instructions. *)
    method build_and lhs rhs = 
      Llvm.build_and lhs rhs "and_tmp" builder
    method build_or lhs rhs =
      Llvm.build_or lhs rhs "or_tmp" builder
    method build_xor lhs rhs =
      Llvm.build_xor lhs rhs "xor_tmp" builder
    method build_not exp =
      Llvm.build_xor (const_int i1_type 1) exp "not_tmp" builder

    (* Comparison instructions. *)
    method build_icmp cmp lhs rhs =
      let name = sprintf "icmp_%s_tmp" (string_of_icmp cmp) in
      Llvm.build_icmp cmp lhs rhs name builder
    method build_fcmp cmp lhs rhs =
      let name = sprintf "fcmp_%s_tmp" (string_of_fcmp cmp) in
      Llvm.build_fcmp cmp lhs rhs name builder

    (* Handling local variables. *)
    method var_intro var_type var_name =
      let alloca = Llvm.build_alloca var_type var_name builder in
      locals#add var_name alloca;
      alloca
    method var_forget name =
      locals#rem name

    (* Resolving names. *)
    method lookup_var name =
      match locals#get name with
      | None ->
          begin
            match package#lookup_global name with
            | None ->
                let msg = sprintf "Unknown variable '%s'." name in
                raise (NameError msg)
            | Some var ->
                var
          end
      | Some var ->
          var
  end;;

class package name =
  object (self)
    val package = Llvm.create_module (Llvm.global_context ()) name

    method define_global name value =
      Llvm.define_global name value package

    method declare_global var_type name =
      Llvm.declare_global var_type name package

    method lookup_global name =
      Llvm.lookup_global name package

    method declare_function name fn_type =
      Llvm.declare_function name fn_type package

    method lookup_function name =
      match Llvm.lookup_function name package with
      | None ->
          let msg = sprintf "Unknown function '%s'." name in
          raise (NameError msg)
      | Some var ->
          var

    method iter_functions f =
      Llvm.iter_functions f package
 
    method get_context =
      Llvm.module_context package

    method get_module = package

    method new_builder =
      new code_builder self 

    method dump =
      Llvm.dump_module package
  end;;

(* JIT Interpreter. *)
class execution_engine pkg =
  object (self)
    val jit = Jit.create pkg#get_module

    method target_data =
      Jit.target_data jit

    method run_function fn args =
      Jit.run_function fn args jit

    method dispose = 
      Jit.dispose jit
  end;;

(* function-by-function pass pipeline over the package [pkg]. It does not take
 * ownership of [pkg]. This type of pipeline is suitable for code generation and
 * JIT compilation tasks. *)
class function_pass_manager pkg =
  object (self)
    val fpm = Llvm.PassManager.create_function pkg#get_module

    method initialize =
      (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
      Llvm_scalar_opts.add_instruction_combination fpm;
      (* reassociate expressions. *)
      Llvm_scalar_opts.add_reassociation fpm;
      (* Eliminate Common SubExpressions. *)
      Llvm_scalar_opts.add_gvn fpm;
      (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
      Llvm_scalar_opts.add_cfg_simplification fpm;
      (* Finally... initialize it! *)
      Llvm.PassManager.initialize fpm

    method run_function fn =
      Llvm.PassManager.run_function fn fpm

    method finalize =
      Llvm.PassManager.finalize fpm
  end;;

let ctx = Llvm.global_context ()
let double_type = Llvm.double_type ctx
let i1_type = Llvm.i1_type ctx
let i8_type = Llvm.i8_type ctx
let i32_type = Llvm.i32_type ctx
let const_stringz = Llvm.const_stringz ctx
let struct_type = Llvm.struct_type ctx
let const_int = Llvm.const_int
let const_float = Llvm.const_float

let izero = const_int i32_type 0
let fzero = const_float double_type 0.0
