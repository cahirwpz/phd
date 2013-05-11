open Lextools
open Token
open Colors

let rec tokenize lexbuf =
  try
    tokenize' lexbuf []
  with Lexer.Failure (pos, msg) ->
    Printf.printf "%s %s\n" pos#as_string msg; exit 0

and tokenize' lexbuf tokens =
  match Lexer.token lexbuf with
  | Token.Eof -> List.rev tokens
  | token -> tokenize' lexbuf (token::tokens)

let nochange = fun x -> x
let unescape = Str.global_replace (Str.regexp "_") ""

let highlight t =
  let k = token_kind t in
  (match k with
  | `comment -> cyan 
  | `string -> magenta
  | `operator -> green
  | `reserved -> blue
  | `number -> red
  | `keyword -> yellow
  | `separator -> white
  | _ -> nochange) (as_string t)

let print_tokens tokens =
  List.iter (fun x -> print_string (highlight x)) tokens 
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
