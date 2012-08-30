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

let wordpos_from_lexbuf lexbuf =
  let p = lexeme_start_p lexbuf in
  let fname = p.pos_fname
  and lnum = p.pos_lnum 
  and cnum = p.pos_cnum - p.pos_bol
  and size = lexeme_end lexbuf - lexeme_start lexbuf
  in new wordpos fname lnum cnum size
