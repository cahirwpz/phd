open Codegen_dt
open ExtHashtbl
open ExtString
open Printf

exception NameError of string
exception TypeError of string
exception CoerceError of Llvm.llvalue * string

module Icmp = Llvm.Icmp
module Fcmp = Llvm.Fcmp
module Jit = Llvm_executionengine.ExecutionEngine

let the_context = Llvm.global_context ()

let dump_type_of v =
  printf "%s : %s" (Llvm.value_name v) (Llvm.string_of_lltype @@ Llvm.type_of v)

let dump_type t =
  print_string @@ Llvm.string_of_lltype t

let dump_value v =
  print_string @@ Llvm.string_of_llvalue v

let double_type = Llvm.double_type the_context
let void_type = Llvm.void_type the_context
let i1_type = Llvm.i1_type the_context
let i8_type = Llvm.i8_type the_context
let i32_type = Llvm.i32_type the_context
let i64_type = Llvm.i64_type the_context
let const_stringz = Llvm.const_stringz the_context
let struct_type = Llvm.struct_type the_context
let const_int = Llvm.const_int
let const_float = Llvm.const_float
let gen_type = Llvm.pointer_type i8_type
let const_null = Llvm.const_null gen_type
let const_pointer_null = Llvm.const_pointer_null gen_type

let int_type = i64_type

let mdstring = Llvm.mdstring the_context
let mdnode = Llvm.mdnode the_context
let mdkind_id = Llvm.mdkind_id the_context

let izero = const_int int_type 0
let fzero = const_float double_type 0.0
let iundef = Llvm.undef int_type

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

class code_builder =
  object (self)
    val builder = Llvm.builder the_context

    method append_block name value =
      Llvm.append_block the_context name value
    method insertion_block =
      Llvm.insertion_block builder
    method position_at_end block =
      Llvm.position_at_end block builder
    method function_block =
      Llvm.block_parent @@ Llvm.insertion_block builder

    (* Control flow. *)
    method build_call fn args =
      let res_type = Llvm.return_type @@ Llvm.get_function_type fn in
      let res_name = (if res_type = void_type then "" else "call_tmp") in
      Llvm.build_call fn args res_name builder

    method build_phi incoming =
      Llvm.build_phi incoming "if_tmp" builder
    method build_cond_br cond then_bb else_bb =
      Llvm.build_cond_br cond then_bb else_bb builder
    method build_br dest_bb =
      Llvm.build_br dest_bb builder
    method build_ret value =
      Llvm.build_ret value builder
    method build_ret_void =
      Llvm.build_ret_void builder

    (* Memory access instructions. *)
    method build_load src name = 
      Llvm.build_load src name builder
    method build_store value dst =
      Llvm.build_store value dst builder

    (* Aggregate access instruction. *)
    method build_gep vector index =
      Llvm.build_gep vector [| index |] "gep_tmp" builder

    (* Arithmetic instructions on integers. *)
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
      Llvm.build_sub (const_int int_type 0) exp "neg_tmp" builder

    (* Arithmetic instructions on floating point numbers. *)
    method build_fadd lhs rhs = 
      Llvm.build_fadd lhs rhs "fadd_tmp" builder
    method build_fsub lhs rhs =
      Llvm.build_fsub lhs rhs "fsub_tmp" builder
    method build_fmul lhs rhs =
      Llvm.build_fmul lhs rhs "fmul_tmp" builder
    method build_fdiv lhs rhs =
      Llvm.build_fdiv lhs rhs "fdiv_tmp" builder
    method build_fneg exp = 
      Llvm.build_sub (const_int double_type 0) exp "fneg_tmp" builder

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
  end;;

class function_builder pkg fn_name =
  object (self)
    inherit code_builder

    val package = pkg
    val local_vars = new symbolmap
    val fn_type = 
      match pkg#lookup_function_type fn_name with
      | Ast.Mapping fn_type -> Array.of_list fn_type
      | _ -> failwith "Function is not of Mapping type!";
    val mutable ret_bbs : (Llvm.llvalue * Llvm.llbasicblock) list = []

    (* Variable load / store instructions. *)
    method build_load_var name =
      Llvm.build_load (self#lookup_var name) name builder
    method build_store_var src name =
      Llvm.build_store src (self#lookup_var name) builder

    (* Calling declared functions and intrinsics. *)
    method build_call_fn (name : string) args =
      self#build_call (package#lookup_function name) args
    method build_call_builtin (name : string) args =
      self#build_call (package#lookup_builtin name) args

    (* Handling local variables. *)
    method var_intro name var_type var_lltype =
      let alloca = Llvm.build_alloca var_lltype name builder in
      local_vars#add name (var_type, alloca);
      alloca
    method var_forget name =
      local_vars#rem name

    method add_ret_bb value bb =
      ret_bbs <- (value, bb)::ret_bbs

    method list_ret_bb =
      ret_bbs

    (* Resolving names. *)
    method lookup_function name =
      package#lookup_function name

    method lookup_var name =
      match local_vars#get name with
      | None ->
          begin
            match package#lookup_global name with
            | None ->
                raise @@ NameError (sprintf "Unknown variable '%s'." name)
            | Some var ->
                var
          end
      | Some var ->
          snd var

    (* Type inquiries. *)
    method var_type name =
      match local_vars#get name with
      | Some var ->
          fst var
      | None ->
          Ast.Any

    method arg_type i =
      if i < Array.length fn_type - 1 then
        fn_type.(i)
      else
        raise (Invalid_argument "index out of bounds")

    method ret_type =
      fn_type.(Array.length fn_type - 1)
  end;;

(* function-by-function pass pipeline over the package [pkg]. It does not take
 * ownership of [pkg]. This type of pipeline is suitable for code generation and
 * JIT compilation tasks. *)
class function_pass_manager pkg =
  object (self)
    val fpm = Llvm.PassManager.create_function pkg#get_module

    method initialize =
      (* Promote allocas to registers - without that while loops don't work. *)
      Llvm_scalar_opts.add_memory_to_register_promotion fpm;
      (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
      Llvm_scalar_opts.add_instruction_combination fpm;
      (* reassociate expressions. *)
      Llvm_scalar_opts.add_reassociation fpm;
      (* Eliminate Common SubExpressions. *)
      Llvm_scalar_opts.add_gvn fpm;
      (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
      Llvm_scalar_opts.add_cfg_simplification fpm;
      (* Remove tail recursion *)
      Llvm_scalar_opts.add_tail_call_elimination fpm;
      let pmbuilder = Llvm_passmgr_builder.create () in
      Llvm_passmgr_builder.populate_function_pass_manager fpm pmbuilder;
      (* Finally... initialize it! *)
      Llvm.PassManager.initialize fpm

    method run_function fn =
      Llvm.PassManager.run_function fn fpm

    method finalize =
      Llvm.PassManager.finalize fpm

    method dispose = 
      Llvm.PassManager.dispose fpm
  end;;

(* Whole-module pass pipeline. This type of pipeline is suitable for link-time
 * optimization and whole-module transformations. *)
class module_pass_manager =
  object (self)
    val mpm = Llvm.PassManager.create ()

    method initialize =
      let pmbuilder = Llvm_passmgr_builder.create () in
      (* Llvm_ipo.add_function_inlining mpm; *)
      (* Llvm_ipo.add_always_inliner mpm; *)
      Llvm_passmgr_builder.populate_module_pass_manager mpm pmbuilder

    method run_module pkg =
      Llvm.PassManager.run_module pkg mpm

    method dispose =
      Llvm.PassManager.dispose mpm
  end;;

class package a_module a_jit =
  object (self)
    val package = a_module
    val jit = a_jit
    val localfuns : (string, Ast.spadtype) Hashtbl.t = Hashtbl.create 10

    method define_global name value =
      Llvm.define_global name value package

    method declare_global var_type name =
      Llvm.declare_global var_type name package

    method lookup_global name =
      Llvm.lookup_global name package

    method declare_function name fn_type fn_lltype =
      Hashtbl.add localfuns name fn_type;
      Llvm.declare_function name fn_lltype package

    method lookup_builtin name =
      match name with
      | "llvm.fabs.f64" ->
          let fn_type = (Ast.Mapping [Ast.DF; Ast.DF])
          and fn_lltype = Llvm.function_type double_type [| double_type |] in
          self#declare_function name fn_type fn_lltype 
      | _ ->
          raise @@ NameError (sprintf "Unknown LLVM intrinsic: '%s'" name)

    method lookup_function name =
      match Llvm.lookup_function name package with
      | Some fn -> fn
      | None -> 
          let fn = jit#lookup_function name in
          let name = Llvm.value_name fn in
          let fn_type = Llvm.element_type (Llvm.type_of fn) in
          let fn_decl = Llvm.declare_function name fn_type package in
          jit#add_global_mapping fn_decl fn;
          fn_decl

    method lookup_function_type name =
      match Hashtbl.find_option localfuns name with
      | None ->
          raise @@ NameError (sprintf "Unknown function '%s'." name)
      | Some atype ->
          atype

    method iter_functions fn =
      Llvm.iter_functions fn package
 
    method get_module = package

    method new_builder = new code_builder
    method new_function_builder name = new function_builder self name

    method dump =
      print_string @@ Llvm.string_of_llmodule package

    method optimize = ()
      (*
      let fpm = new function_pass_manager self
      and mpm = new module_pass_manager in
      ignore (fpm#initialize);
      ignore (mpm#initialize);
      self#iter_functions (fun fn -> ignore (fpm#run_function fn));
      ignore (mpm#run_module self#get_module);
      ignore (fpm#finalize);
      fpm#dispose;
      mpm#dispose
      *)
  end;;

(* JIT Interpreter. *)
class execution_engine =
  (* We have to call it, otherwise you can expect strage behaviour of generated
   * code (ie. problems with endianness of global / stack variables). *)
  let _ = Llvm_executionengine.initialize_native_target () in

  object (self)
    val map : (string, package) Hashtbl.t = Hashtbl.create 10
    val jit = Jit.create (Llvm.create_module the_context "empty")

    method target_data =
      Jit.target_data jit

    method run_function fn args =
      Jit.run_function fn args jit

    method lookup_function name =
      match Jit.find_function name jit with
      | None ->
          raise @@ NameError (sprintf "Unknown function '%s'." name)
      | Some var ->
          var

    method add_global_mapping decl_value def_value =
      Jit.add_global_mapping decl_value def_value jit

    method dispose = 
      Jit.dispose jit

    method add_package name =
      (* http://lists.cs.uiuc.edu/pipermail/llvmdev/2011-September/042966.html *)
      let runtime = (Hashtbl.find map "runtime")#get_module in
      let pkg = new package (Llvm_utils.clone_module runtime) self in
      pkg#iter_functions (fun (f) ->
        Llvm.add_function_attr f Llvm.Attribute.Alwaysinline);
      Hashtbl.add map name pkg;
      Jit.add_module pkg#get_module jit;
      pkg

    method load_package filename =
      let module MemBuf = Llvm.MemoryBuffer in
      let module BitReader = Llvm_bitreader in
      let membuf = MemBuf.of_file filename in
      let maybe_pkg = (
        try
          let from_file = BitReader.parse_bitcode the_context membuf in
          let pkg = new package from_file self in
          Hashtbl.add map (Filename.chop_extension filename) pkg;
          Jit.add_module pkg#get_module jit;
          Some pkg
        with BitReader.Error msg ->
          printf "Could not load '%s' file: %s.\n" filename msg;
          None) in
      MemBuf.dispose membuf;
      maybe_pkg

    method iter_packages fn =
      Hashtbl.iter (fun _ pkg -> fn pkg) map
  end;;
