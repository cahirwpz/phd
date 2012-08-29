open Lexing

(* token position *)
class tokpos filename line column =
  object (self)
    val filename : string = filename
    val line : int = line
    val column : int = column
    method as_string =
      Printf.sprintf "%s:%d:%d" filename line column
  end

let tokpos_from_lexbuf lexbuf =
  let p = lexeme_start_p lexbuf in
  let f_name = p.pos_fname
  and l_num = p.pos_lnum 
  and c_num = p.pos_cnum - p.pos_bol
  in new tokpos f_name l_num c_num
