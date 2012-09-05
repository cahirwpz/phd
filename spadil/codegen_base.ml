open Codegen_dt
open Printf

let values = new valueStore
let functions = new valueStore

module Icmp = Llvm.Icmp
module Fcmp = Llvm.Fcmp

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
  let ctx = Llvm.module_context pkg in
  object (self)
    val builder = Llvm.builder ctx
    val package = pkg
    val context = ctx

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
    method build_call name args =
      Llvm.build_call (functions#get name) args "call_tmp" builder
    method build_phi incoming =
      Llvm.build_phi incoming "if_tmp" builder
    method build_cond_br cond then_bb else_bb =
      Llvm.build_cond_br cond then_bb else_bb builder
    method build_br dest_bb =
      Llvm.build_br dest_bb builder
    method build_ret value =
      Llvm.build_ret value builder

    (* Memory access instructions. *)
    method build_load var_name = 
      Llvm.build_load (values#get var_name) var_name builder
    method build_store src var_name =
      Llvm.build_store src (values#get var_name) builder
    method build_alloca var_type var_name =
      let alloca = Llvm.build_alloca var_type var_name builder in
      values#add var_name alloca;
      alloca

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

  end

class package name =
  object (self)
    val package = Llvm.create_module (Llvm.global_context ()) name

    method add_global_def name value =
      let var = Llvm.define_global name value package in
      values#add name var; var

    method add_global_decl var_type name =
      let var = Llvm.declare_global var_type name package in
      values#add name var; var

    method add_function_decl name fn_type =
      let fn = Llvm.declare_function name fn_type package in
      functions#add name fn; fn

    method new_builder =
      new code_builder package

    method dump =
      Llvm.dump_module package
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
