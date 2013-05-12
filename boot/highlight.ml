open Lextools
open Colors
open Token

module StringSet = Set.Make(String)

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
  let kind = Token.kind_of_token t in
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
          if StringSet.mem t.text builtins then
            blue
          else if StringSet.mem (unescape t.text) lisp then
            inverse
          else if t.text.[0] == '$' then
            underline
          else
            nochange
      | _ -> nochange)
  | _ -> nochange) t.text

let print_token t = 
  print_string (highlight t)
