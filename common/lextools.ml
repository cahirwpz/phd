open Lexing

type tokpos = { source : string; line : int; column : int }

let tokpos_to_string t =
  Printf.sprintf "%s:%d:%d" t.source t.line (t.column + 1)

let tokpos_from_lexbuf lexbuf =
  let p = lexeme_start_p lexbuf in
  {source = p.pos_fname; line = p.pos_lnum; column = p.pos_cnum - p.pos_bol}

exception LexerError of tokpos * string

type 'a token = { typ : 'a; text : string; pos : tokpos; }

let token_from_lexbuf typ lexbuf =
  {typ = typ; text = lexeme lexbuf; pos = tokpos_from_lexbuf lexbuf}

let open_named_lexbuf input fname =
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
  lexbuf

(* Useful for counting indentations. *)
let count_spaces spaces =
  let l = ref 0 in
  let counter c = 
    l := !l + (if c = '\t' then 8 else 1)
  in String.iter counter spaces; !l
