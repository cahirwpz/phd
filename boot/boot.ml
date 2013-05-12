open Colors
open Lextools
open Token

module StringSet = Set.Make(String)

let rec tokenize lexbuf =
  try
    tokenize' lexbuf []
  with LexerError (tok, msg) ->
    Printf.printf "%s %s\n" (token_to_string tok) msg; exit 0

and tokenize' lexbuf tokens =
  match Lexer.token lexbuf with
  | {typ=Token.Eof} -> List.rev tokens
  | token -> tokenize' lexbuf (token::tokens)

let read_symbols path =
  let open StringSet in
  let contents = Std.input_all (open_in path) in
  let strings = Str.split (Str.regexp "[ \n]+") contents in
  List.fold_right add strings empty

let lisp = read_symbols "symbols.lst"
let builtins = read_symbols "builtin.lst" 

let nochange = fun x -> x
let unescape = Str.global_replace (Str.regexp "_") ""

let highlight t =
  let kind = kind_of_token t
  and text = t.token.text in
  (match kind with
  | `comment -> cyan 
  | `string -> magenta
  | `operator -> green
  | `reserved -> blue
  | `number -> red
  | `keyword -> yellow
  | `separator -> white
  | `symbol ->
      (match t.typ with
      | Symbol ->
          if StringSet.mem text builtins then
            blue
          else if StringSet.mem (unescape text) lisp then
            inverse
          else if text.[0] == '$' then
            underline
          else
            nochange
      | _ -> nochange)
  | _ -> nochange) text

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
