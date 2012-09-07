open Lexing
open Format
open Lextools

let pkg = Codegen_base.create_package "some-name"

let print lisp =
  (* printf "@[<v 2>LISP (original)@,@,"; Sexpr.print lisp; printf "@]@.@."; *)
  let lisp_opt = Sexpr.simplify lisp in
  (* printf "@[<v 2>LISP (rewritten)@,@,"; Sexpr.print lisp_opt; printf "@]@.@."; *)
  let il = Ast.convert lisp_opt in
  (* printf "@[<v 2>IL (original)@,@,"; Ast.print il; printf "@]@.@."; *)
  let il_opt = Rewrite.simplify il in
  printf "@[<v 2>IL (rewritten)@,@,"; Ast.print il_opt; printf "@]@.@.";
  match Codegen.codegen_toplevel pkg il with
  | Some _ -> ()
  | None -> exit(1)

let parse lexbuf =
  let trees = Parser.program Lexer.token lexbuf
  in List.iter print trees

let execute pkg = 
  let fpm = new Codegen_base.function_pass_manager pkg in
  let jit = new Codegen_base.execution_engine pkg in
  let td = jit#target_data in
  fpm#set_target_data td;
  ignore (fpm#initialize);
  pkg#iter_functions (fun fn -> ignore (fpm#run_function fn));
  ignore (fpm#finalize);
  pkg#dump;
  let result = jit#run_function (pkg#lookup_function "main") [||] in
  let result = Llvm_executionengine.GenericValue.as_int32 result in
  printf "Evaluated to %d\n" (Int32.to_int result);
  jit#dispose;
  ()

let load_package filename =
  match Codegen_base.load_package filename with
  | Some pkg ->
      pkg#dump
  | None ->
      ()

let main () =
  ignore (load_package "runtime.bc");
  if Array.length Sys.argv > 1
  then
    for i = 1 to Array.length Sys.argv - 1 do
      let filename = Sys.argv.(i) in
      let file = open_in filename in
      printf "**** %s ****@." filename;
      parse (open_named_lexbuf file filename);
      close_in file;
      (* Print out all the generated code. *)
      execute pkg
    done
  else parse (open_named_lexbuf stdin "<stdin>")

let _ = Printexc.print main ()
