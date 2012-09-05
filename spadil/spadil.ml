open Lexing
open Format
open Lextools

let pkg = new Codegen_base.package "some-name"

let print lisp =
  printf "@[<v 2>LISP (original)@,@,"; Sexpr.print lisp; printf "@]@.@.";
  let lisp_opt = Sexpr.simplify lisp in
  printf "@[<v 2>LISP (rewritten)@,@,"; Sexpr.print lisp_opt; printf "@]@.@.";
  let il = Ast.convert lisp_opt in
  printf "@[<v 2>IL (original)@,@,"; Ast.print il; printf "@]@.@.";
  let il_opt = Rewrite.simplify il in
  printf "@[<v 2>IL (rewritten)@,@,"; Ast.print il_opt; printf "@]@.@.";
  ignore (Codegen.codegen_toplevel pkg il)

let parse lexbuf =
  let trees = Parser.program Lexer.token lexbuf
  in List.iter print trees

let main () =
  if Array.length Sys.argv > 1
  then
   for i = 1 to Array.length Sys.argv - 1 do
     let filename = Sys.argv.(i) in
     let file = open_in filename in
     printf "**** %s ****@." filename;
     parse (open_named_lexbuf file filename);
     close_in file;
     (* Print out all the generated code. *)
     pkg#dump
   done
  else parse (open_named_lexbuf stdin "<stdin>")

let _ = Printexc.print main ()
