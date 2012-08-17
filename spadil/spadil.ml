open Lexing
open Printf

let open_lexbuf input fname =
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
  lexbuf

let print e =
  printf "%s\n\n%s\n\n" (Sexpr.as_string e) (Ast.as_string e)

let parse lexbuf =
  let trees = Parser.program Lexer.token lexbuf
  in List.iter print trees

let main () =
  if Array.length Sys.argv > 1
  then
   for i = 1 to Array.length Sys.argv - 1 do
     let filename = Sys.argv.(i) in
     let file = open_in filename in
     print_string ("**** " ^ filename ^ " ****\n");
     parse (open_lexbuf file filename);
     close_in file
   done
  else parse (open_lexbuf stdin "<stdin>")

let _ = Printexc.print main ()
