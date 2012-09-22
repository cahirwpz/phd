open Lexing
open Format
open Lextools

let rec parse_file pkg lexbuf =
  let parse_unit' = parse_unit pkg in
  List.iter parse_unit' (Parser.program Lexer.token lexbuf)

and parse_unit pkg input =
  (* printf "@[<v 2>LISP (original)@,@,"; Sexpr.print input; printf "@]@.@."; *)
  let lisp_opt = Sexpr.simplify input in
  (* printf "@[<v 2>LISP (rewritten)@,@,"; Sexpr.print lisp_opt; printf "@]@.@."; *)
  let il = Ast.convert lisp_opt in
  (* printf "@[<v 2>IL (original)@,@,"; Ast.print il; printf "@]@.@."; *)
  let il_opt = Rewrite.simplify il in
  printf "@[<v 2>IL (rewritten)@,@,"; Ast.print il_opt; printf "@]@.@.";
  match Codegen.codegen_toplevel pkg il with
  | Some _ -> ()
  | None -> exit 1

let load_packages jit = function
  | []
  | _::[] ->
    let pkg = jit#add_package "stdin" in
    parse_file pkg (open_named_lexbuf stdin "<stdin>")
  | program::args ->
    let load_package filename =
      let pkg = jit#add_package (Filename.chop_extension filename) in
      let file = open_in filename in
      printf "**** %s ****@." filename;
      parse_file pkg (open_named_lexbuf file filename);
      close_in file
    in List.iter load_package args

let execute jit = 
  let result = jit#run_function (jit#lookup_function "main") [||] in
  let result' = Llvm_executionengine.GenericValue.as_int32 result in
  printf "Evaluated to %d\n" (Int32.to_int result')

let main () =
  let jit = new Codegen_base.execution_engine in
  ignore (jit#load_package "runtime.bc");
  load_packages jit (Array.to_list Sys.argv);
  jit#optimize;
  jit#iter_packages (fun pkg -> pkg#dump);
  execute jit;
  jit#dispose

let _ = Printexc.print main ()
