type tokpos = { source : string; line : int; column : int; }

val tokpos_to_string : tokpos -> string
val tokpos_from_lexbuf : Lexing.lexbuf -> tokpos

exception LexerError of tokpos * string

type 'a token = { typ : 'a; text : string; pos : tokpos; }

val token_from_lexbuf : 'a -> Lexing.lexbuf -> 'a token

val open_named_lexbuf : in_channel -> string -> Lexing.lexbuf

val count_spaces: string -> int
