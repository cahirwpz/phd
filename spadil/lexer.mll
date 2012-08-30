{
  open Parser
  open Lexing
  open Strbuf
  open Wordpos

  let unknown_char lexbuf c = 
    let s = (wordpos_from_lexbuf lexbuf)#as_string in
    Printf.printf "%s Unrecognized character '%c'\n" s c

  (* convert digit character to number *)
  let int_of_digit d = Char.code d - Char.code '0'
}

let digit = ['0'-'9']
let space = [' ' '\t']
let alpha = ['a'-'z' 'A'-'Z']
let graph = ['_' '+' '-' '.' '&' ':' '*' '$' '%' '{' '}' '[' ']' '!' '^' '?'
             '@' '~' '/' '>' '<' '=']
let symbol = (alpha | graph)
let id = symbol (symbol | digit)*

let integer = digit+
let real = digit+ '.' digit*

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
  | '"' { STRING (Scanf.unescaped buf#gets) }
  | '\\' (_ as c) { buf#putc '\\'; buf#putc c; string buf lexbuf }
  | _ as c { buf#putc c; string buf lexbuf }

and bar buf = parse
  | '|' as c { buf#putc c; Scanf.unescaped buf#gets }
  | "\\" (_ as c) { buf#putc '\\'; buf#putc c; bar buf lexbuf }
  | _ as c { buf#putc c; bar buf lexbuf }

and reader = parse
  | (integer as num) '=' { TREE_DECL (int_of_string num) }
  | (integer as num) '#' { TREE_REF (int_of_string num) }
  | ':' (id as name) { LABEL name }
  | '(' { VECTOR }
  | "'" '|'{ let buf = new strbuf in buf#putc '|'; FUNCTION (bar buf lexbuf) }
  | "'" (id as name) { FUNCTION name }
  | _ as c { unknown_char lexbuf c; token lexbuf }
