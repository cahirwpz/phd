open Lexing
open Format
open Lextools

let as_int32 = Llvm_executionengine.GenericValue.as_int32
let as_int64 = Llvm_executionengine.GenericValue.as_int64
let as_pointer = Llvm_executionengine.GenericValue.as_pointer
let of_pointer = Llvm_executionengine.GenericValue.of_pointer

(* Parse loaded source. *)
let rec parse_file pkg lexbuf =
  List.iter (parse_unit pkg) (Parser.program Lexer.token lexbuf)

and parse_unit pkg input =
  printf "@[<v 2>LISP (original)@,@,"; Sexpr.print (Sexpr.reader_pass input); printf "@]@.@.";
  let lisp_opt = Sexpr.simplify input in
  printf "@[<v 2>LISP (rewritten)@,@,"; Sexpr.print lisp_opt; printf "@]@.@.";
  let il = Ast.convert lisp_opt in
  (* printf "@[<v 2>IL (original)@,@,"; Ast.print il; printf "@]@.@."; *)
  let il_opt = Rewrite.simplify il in
  (* printf "@[<v 2>IL (rewritten)@,@,"; Ast.print il_opt; printf "@]@.@."; *)
  Ast.print il_opt; printf "@.@.";
  ignore(Codegen.codegen_toplevel pkg il)

(* Load compiled modules. *)
let rec load_runtime jit bclist =
  List.iter (load_bc jit) bclist;
  printf "@."

and load_bc jit bc = 
  match jit#load_package bc with
  | Some pkg -> pkg#dump
  | None -> failwith (sprintf "Error reading '%s' file." bc)

(* Load source files to be compiled. *)
let rec load_packages jit args =
  if List.length args > 0 then
    List.iter (load_package jit) args
  else
    parse_file (jit#add_package "stdin") (open_named_lexbuf stdin "<stdin>")

and load_package jit filename =
  let pkg = jit#add_package (Filename.chop_extension filename) in
  let file = open_in filename in
  printf "; Source = '%s'@." filename; 
  parse_file pkg (open_named_lexbuf file filename);
  pkg#optimize;
  pkg#dump;
  close_in file

let execute jit = 
  let main_fn = jit#lookup_function "main"
  and time_start_fn = jit#lookup_function "time_start"
  and time_stop_fn = jit#lookup_function "time_stop"
  and time_diff_fn = jit#lookup_function "time_diff"
  and gc_start_fn = jit#lookup_function "gc_start" in
  printf "@.; Initialize garbage collection@.";
  ignore(jit#run_function gc_start_fn [||]);
  printf "; Evaluate main function@.";
  ignore(jit#run_function time_start_fn [||]);
  ignore(jit#run_function main_fn [| of_pointer 0 |]);
  ignore(jit#run_function time_stop_fn [||]);
  let diff = Int64.to_int(as_int64(jit#run_function time_diff_fn [||])) in
  ignore(printf "; Took %d us@." diff)

let main () =
  let jit = new Codegen_base.execution_engine in
  load_runtime jit ["vmdata.bc";"runtime.bc";"benchmark.bc"];
  load_packages jit (List.tl @@ Array.to_list Sys.argv);
  execute jit;
  jit#dispose

let _ =
  Printexc.record_backtrace true;
  Printexc.print main ()
