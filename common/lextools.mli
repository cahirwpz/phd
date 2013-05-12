type token = { text : string; source : string; line : int; column : int; }

val token_to_string : token -> string
val token_from_lexbuf : Lexing.lexbuf -> token

exception LexerError of token * string

val open_named_lexbuf : in_channel -> string -> Lexing.lexbuf

class strbuf :
  object
    method gets : string
    method putc : char -> unit
    method puts : string -> unit
  end

val strbuf_from_str : string -> strbuf

val count_spaces: string -> int
