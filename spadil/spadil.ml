open Lexing
open Format

let open_lexbuf input fname =
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
  lexbuf

let print lisp =
  let lisp_opt = Sexpr.simplify lisp in
  let il = Ast.convert lisp_opt in
  let il_opt = Rewrite.simplify il in
  printf "@[<v 2>LISP (original)@,@,"; Sexpr.print lisp; printf "@]@.";
  printf "@[<v 2>LISP (rewritten)@,@,"; Sexpr.print lisp_opt; printf "@]@.";
  printf "@[<v 2>IL (original)@,@,"; Ast.print il; printf "@]@.";
  printf "@[<v 2>IL (rewritten)@,@,"; Ast.print il_opt; printf "@]@."

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
     parse (open_lexbuf file filename);
     close_in file
   done
  else parse (open_lexbuf stdin "<stdin>")

let _ = Printexc.print main ()
