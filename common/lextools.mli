class wordpos : string -> int -> int ->
  object
    val column : int
    val filename : string
    val line : int
    method as_string : string
  end

exception LexerError of wordpos * string

val wordpos_from_lexbuf : Lexing.lexbuf -> wordpos
val open_named_lexbuf : in_channel -> string -> Lexing.lexbuf

class strbuf :
  object
    method gets : string
    method putc : char -> unit
    method puts : string -> unit
  end

val strbuf_from_str : string -> strbuf

val count_spaces: string -> int
