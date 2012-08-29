{
  open Printf
  open Parser
  open Lexing

  (* token position *)
  let get_lnum buf = buf.lex_curr_p.pos_lnum
  let get_cnum buf = buf.lex_curr_p.pos_cnum

  type tokpos = { p_fname : string; p_line : int; p_col : int }

  let get_tokpos lexbuf =
    let p = lexeme_start_p lexbuf in
    let f_name = p.pos_fname
    and l_num = p.pos_lnum 
    and c_num = p.pos_cnum - p.pos_bol in
    {p_fname = f_name; p_line = l_num; p_col = c_num}

  let string_of_tokpos p =
    let file = p.p_fname and line = p.p_line and column = p.p_col in
    sprintf "%s:%d:%d" file line column

  let unknown_char lexbuf c = 
    let s = string_of_tokpos (get_tokpos lexbuf) in
    printf "%s Unrecognized character '%c'\n" s c

  (* String buffer class - a simple wrapper for stdlib's Buffer. *)
  class strbuf =
    object (self)
      val buffer = Buffer.create 1
      method putc c =
        Buffer.add_char buffer c
      method puts s = 
        Buffer.add_string buffer s
      method gets =
        Scanf.unescaped (Buffer.contents buffer)
    end

  (* convert digit character to number *)
  let int_of_digit d = int_of_char d - int_of_char '0'
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
  | '"' { STRING buf#gets }
  | '\\' (_ as c) { buf#putc '\\'; buf#putc c; string buf lexbuf }
  | _ as c { buf#putc c; string buf lexbuf }

and bar buf = parse
  | '|' as c { buf#putc c; buf#gets }
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
