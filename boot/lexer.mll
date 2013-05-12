{
  open Lexing
  open Lextools
  open Token

  let mk_tok typ lexbuf =
    { typ = typ; token = token_from_lexbuf lexbuf; }
}

let digit = ['0'-'9']
let space = [' ' '\t']
let alpha = ['a'-'z' 'A'-'Z']
let extra = ['!' '?' '\'' '_']
let graph = ['&' '*' '(' ')' ':' '-' '>' '<' '%' '$' '"' ' ' '_' '=' '^' '{' '}'
             '^' '\\' '[' ']' '!' '#' '`' '.' '/' '~' '|' '?' ',' ';' '+' '\''
             '@']
let escaped = '_' (graph | digit)
let symbol =
  (alpha | escaped | '_' | '%' | '$' | '?') (alpha | digit | extra | escaped)*
let comment = [^'\n']*

let integer = digit+
let real = digit+ '.' digit+
let str = '"' ('_' _ | [^'"' '_'])* '"'

rule token = parse
  (* Indentation: [\s\t]+ *)
  | space+ { mk_tok Indent lexbuf }

  (* Escaping delimeter. *)
  | '_' '\n' { new_line lexbuf; mk_tok Lc lexbuf }
  
  (* Strings (the contents will be unescaped). *)
  | str { mk_tok String lexbuf }

  (* Multiline comments. *)
  | ")if false" _* ")endif" { mk_tok Comment lexbuf }

  (* Directives. *)
  | ")package" { mk_tok Package lexbuf }

  (* One character tokens: '()[]*,;^#|' *)
  | '(' { mk_tok LParen lexbuf }
  | ')' { mk_tok RParen lexbuf }
  | '[' { mk_tok LBracket lexbuf }
  | ']' { mk_tok RBracket lexbuf }
  | '*' { mk_tok Times lexbuf }
  | ',' { mk_tok Comma lexbuf }
  | ';' { mk_tok Semicolon lexbuf }
  | '^' { mk_tok Pow lexbuf }
  | '#' { mk_tok Length lexbuf }
  | '|' { mk_tok Bar lexbuf }
  | '\'' { mk_tok Quote lexbuf }

  (* ':', ':=' *)
  | ":=" { mk_tok Assign lexbuf }
  | ':' { mk_tok Colon lexbuf }

  (* '=', '==', '=>', '==>' *)
  | "==>" { mk_tok Macro lexbuf }
  | "==" { mk_tok Bind lexbuf }
  | "=>" { mk_tok When lexbuf }
  | '=' { mk_tok Eq lexbuf }

  (* '+', '++' *)
  | "++" comment { mk_tok Comment lexbuf }
  | '+' { mk_tok Plus lexbuf }

  (* '-', '--' *)
  | "--" comment { mk_tok Comment lexbuf }
  | "-" { mk_tok Minus lexbuf }

  (* '.', '..' *)
  | ".." { mk_tok Ellipsis lexbuf }
  | '.' { mk_tok Dot lexbuf }

  (* '<', '<=' *)
  | "<=" { mk_tok Le lexbuf }
  | '<' { mk_tok Lt lexbuf }

  (* '>', '>=' *)
  | ">=" { mk_tok Ge lexbuf }
  | '>' { mk_tok Gt lexbuf }

  (* '/', '~=' *)
  | "~=" { mk_tok Ne lexbuf }
  | '/' { mk_tok Div lexbuf }

  (* number: [0-9.]+ *)
  | real { mk_tok Float lexbuf }
  | integer { mk_tok Int lexbuf }

  (* Finally identifiers: functions, variables, types, etc. *)
  | symbol as name { 
    match name with
    | "and"       -> mk_tok And lexbuf
    | "break"     -> mk_tok Break lexbuf
    | "by"        -> mk_tok Step lexbuf
    | "else"      -> mk_tok Else lexbuf
    | "for"       -> mk_tok For lexbuf
    | "if"        -> mk_tok If lexbuf
    | "in"        -> mk_tok In lexbuf
    | "is"        -> mk_tok Is lexbuf
    | "isnt"      -> mk_tok IsNot lexbuf
    | "local"     -> mk_tok Local lexbuf
    | "not"       -> mk_tok Not lexbuf
    | "of"        -> mk_tok Of lexbuf
    | "or"        -> mk_tok Or lexbuf
    | "repeat"    -> mk_tok Repeat lexbuf
    | "return"    -> mk_tok Return lexbuf
    | "then"      -> mk_tok Then lexbuf
    | "until"     -> mk_tok Until lexbuf
    | "where"     -> mk_tok Where lexbuf
    | "while"     -> mk_tok While lexbuf
    | _           -> mk_tok Symbol lexbuf
  }

  (* Delimeters. *)
  | '\n' { new_line lexbuf; mk_tok Eol lexbuf }
  | eof { mk_tok Eof lexbuf }

  | _ as c { let tok = token_from_lexbuf lexbuf
             and msg = Printf.sprintf "Unrecognized character '%c'" c
             in raise (LexerError (tok, msg)) }
