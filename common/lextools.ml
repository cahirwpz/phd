open Lexing

type token = { text : string; source : string; line : int; column : int; }

let token_to_string t =
  Printf.sprintf "%s:%d:%d {%s}" t.source t.line (t.column + 1) t.text

exception LexerError of token * string

let token_from_lexbuf lexbuf =
  let p = lexeme_start_p lexbuf in
  let fname = p.pos_fname
  and lnum = p.pos_lnum
  and cnum = p.pos_cnum - p.pos_bol in
  {text=lexeme lexbuf; source=fname; line=lnum; column=cnum}

let open_named_lexbuf input fname =
  let lexbuf = from_channel input in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
  lexbuf

(* String buffer class - a simple wrapper for stdlib's Buffer. *)
class strbuf =
  object (self)
    val buffer = Buffer.create 1
    method putc c =
      Buffer.add_char buffer c
    method puts s = 
      Buffer.add_string buffer s
    method gets =
      Buffer.contents buffer
  end

let strbuf_from_str str =
  let buf = new strbuf in
  buf#puts str;
  buf

(* Useful for counting indentations. *)
let count_spaces spaces =
  let l = ref 0 in
  let counter c = 
    l := !l + (if c = '\t' then 8 else 1)
  in String.iter counter spaces; !l
