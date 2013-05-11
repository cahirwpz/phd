open Lextools

let rec tokenize lexbuf =
  try
    tokenize' lexbuf []
  with Lexer.Failure (pos, msg) ->
    Printf.printf "%s %s\n" pos#as_string msg; exit 0

and tokenize' lexbuf tokens =
  match Lexer.token lexbuf with
  | Token.Eof -> List.rev tokens
  | token -> tokenize' lexbuf (token::tokens)

let main () =
  if Array.length Sys.argv > 1
  then
   for i = 1 to Array.length Sys.argv - 1 do
     let filename = Sys.argv.(i) in
     let file = open_in filename in
     Printf.printf "Tokenizing file '%s':\n" filename;
     Token.print (tokenize (open_named_lexbuf file filename));
     close_in file
   done
  else Token.print (tokenize (open_named_lexbuf stdin "<stdin>"))

let _ = Printexc.print main ()
