open Lextools
open Token

let rec tokenize lexbuf =
  try
    Stream.of_list (tokenize' lexbuf [])
  with LexerError (pos, msg) ->
    Printf.printf "%s %s\n" (tokpos_to_string pos) msg; exit 0

and tokenize' lexbuf tokens =
  match Lexer.token lexbuf with
  | {typ = Token.Eof} -> List.rev tokens
  | token -> tokenize' lexbuf (token::tokens)

let print_tokens tokens =
  Stream.iter Highlight.print_token tokens

let main () =
  if Array.length Sys.argv > 1 then
   for i = 1 to Array.length Sys.argv - 1 do
     let filename = Sys.argv.(i) in
     let file = open_in filename in
     Printf.printf "Tokenizing file '%s':\n" filename;
     print_tokens (tokenize (open_named_lexbuf file filename));
     close_in file
   done
  else
    print_tokens (tokenize (open_named_lexbuf stdin "<stdin>"))

let _ = Printexc.print main ()
