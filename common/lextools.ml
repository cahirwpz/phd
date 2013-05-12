open Lexing

(* Word position within a file. *)
class wordpos filename line column size =
  object (self)
    val filename : string = filename
    val line : int = line
    val column : int = column
    val size : int = size
    method as_string =
      Printf.sprintf "%s:%d:%d (#%d)" filename line (column + 1) size
    method get_size = size
  end

exception LexerError of wordpos * string

let wordpos_from_lexbuf lexbuf =
  let p = lexeme_start_p lexbuf in
  let fname = p.pos_fname
  and lnum = p.pos_lnum 
  and cnum = p.pos_cnum - p.pos_bol
  and size = lexeme_end lexbuf - lexeme_start lexbuf
  in new wordpos fname lnum cnum size

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

class strbuf_with str =
  object (self)
    inherit strbuf
    initializer self#puts str
  end

let count_spaces spaces =
  let l = ref 0 in
  let counter c = 
    l := !l + (if c = '\t' then 8 else 1)
  in String.iter counter spaces; !l
