{
  open Parser
  open Lexing
  open Lextools

  let unknown_char lexbuf c = 
    let s = tokpos_to_string (tokpos_from_lexbuf lexbuf) in
    Printf.printf "%s Unrecognized character '%c'\n" s c

  (* convert digit character to number *)
  let int_of_digit d = Char.code d - Char.code '0'

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
      method gets_case_sensitive =
        let s = Buffer.contents buffer in
        let l = String.length s in
        String.sub s 1 (l - 2)
    end
}

let digit = ['0'-'9']
let space = [' ' '\t']
let alpha = ['a'-'z' 'A'-'Z']
let graph = ['_' '+' '-' '.' '&' ':' '*' '$' '%' '{' '}' '[' ']' '!' '^' '?'
             '@' '~' '/' '>' '<' '=']
let symbol = (alpha | graph)
let id = symbol (symbol | digit)*

let integer = '-'? digit+
let real = '-'? digit+ '.' digit*

rule token = parse
  | space+ { token lexbuf }
  | '#' { reader lexbuf }
  | '|' { let buf = new strbuf in buf#putc '|'; SYMBOL (bar buf lexbuf) }
  | '"' { string (new strbuf) lexbuf }
  | "'" { QUOTE }
  | '\n' { new_line lexbuf; token lexbuf }
  | ';' [^'\n']* { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | real as num { FNUM (float_of_string num) }
  | integer as num { INUM (int_of_string num) }
  | id as name { SYMBOL (String.uppercase name) }
  | eof	{ EOF }
  | _ as c { unknown_char lexbuf c; token lexbuf }

and string buf = parse
  | '"' { STRING buf#gets }
  | '\\' (_ as c) { buf#putc '\\'; buf#putc c; string buf lexbuf }
  | _ as c { buf#putc c; string buf lexbuf }

and bar buf = parse
  | '|' as c { buf#putc c; buf#gets_case_sensitive }
  | "\\" (_ as c) { buf#putc '\\'; buf#putc c; bar buf lexbuf }
  | _ as c { buf#putc c; bar buf lexbuf }

and reader = parse
  | (integer as num) '=' { TREE_DECL (int_of_string num) }
  | (integer as num) '#' { TREE_REF (int_of_string num) }
  | ':' (id as name) { LABEL name }
  | '(' { VECTOR }
  | "'" '|'{ let buf = new strbuf in buf#putc '|'; FUNCTION (bar buf lexbuf) }
  | "'" (id as name) { FUNCTION (String.uppercase name) }
  | _ as c { unknown_char lexbuf c; token lexbuf }
