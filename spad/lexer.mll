{
  open Lexing
  open Lextools
  open Token

  let mk_tok = token_from_lexbuf
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
  (alpha | escaped | '_' | '%') (alpha | digit | extra | escaped)*
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

  (* Directives. *)
  | ")abbrev" { mk_tok Abbrev lexbuf }

  (* One character tokens: '()[]$@*,;^' *)
  | '(' { mk_tok LParen lexbuf }
  | ')' { mk_tok RParen lexbuf }
  | '[' { mk_tok LBracket lexbuf }
  | ']' { mk_tok RBracket lexbuf }
  | '$' { mk_tok UsesType lexbuf }
  | '@' { mk_tok ReturnsType lexbuf }
  | '*' { mk_tok Times lexbuf }
  | ',' { mk_tok Comma lexbuf }
  | ';' { mk_tok Semicolon lexbuf }
  | '^' { mk_tok Pow lexbuf }
  | '#' { mk_tok Length lexbuf }
  | '%' { mk_tok TypeName lexbuf }
  | '|' { mk_tok Bar lexbuf }
  | '\'' { mk_tok Quote lexbuf }

  (* Unambiguous two character tokens: '\/' *)
  | '\\' ('_')? '/' { mk_tok Or lexbuf }

  (* Ambiguous tokens that begin with colon character - ':', ':=', '::'. *)
  | ":=" { mk_tok Assign lexbuf }
  | "::" { mk_tok ToType lexbuf }
  | ':' { mk_tok OfType lexbuf }

  (* Ambiguous tokens that begin with equals character - '=', '==', '==>'. *)
  | "==>" { mk_tok Macro lexbuf }
  | "==" { mk_tok Bind lexbuf }
  | '=' { mk_tok Eq lexbuf }

  (* Ambiguous tokens that begin with plus character - '+', '++', '+->'. *)
  | "+->" { mk_tok Lambda lexbuf }
  | "++" comment { mk_tok Comment lexbuf }
  | '+' { mk_tok Plus lexbuf }

  (* Ambiguous tokens that begin with minus character - '-', '->', '--'. *)
  | "--" comment { mk_tok Comment lexbuf }
  | "->" { mk_tok Arrow lexbuf }
  | "-" { mk_tok Minus lexbuf }

  (* Ambiguous tokens that begin with dot character - '.', '..'. *)
  | ".." { mk_tok Ellipsis lexbuf }
  | '.' { mk_tok Dot lexbuf }

  (* Ambiguous tokens that begin with slash character - '/', '/\'. *)
  | '/' ('_')? '\\' { mk_tok And lexbuf }
  | '/' { mk_tok Div lexbuf }

  (* Ambiguous tokens that begin with less-than character - '<', '<='. *)
  | "<=" { mk_tok Le lexbuf }
  | '<' { mk_tok Lt lexbuf }

  (* Ambiguous tokens that begin with greater-than character - '>', '>='. *)
  | ">=" { mk_tok Ge lexbuf }
  | '>' { mk_tok Gt lexbuf }

  (* Ambiguous tokens that begin with greater-than character - '~', '~='. *)
  | "~=" { mk_tok Ne lexbuf }
  | '~' { mk_tok Not lexbuf }

  (* number: [0-9.]+ *)
  | real { mk_tok Float lexbuf }
  | integer { mk_tok Int lexbuf }

  (* Finally identifiers: functions, variables, types, etc. *)
  | symbol as name { 
    match name with
    | "add"       -> mk_tok Add lexbuf
    | "and"       -> mk_tok And lexbuf
    | "break"     -> mk_tok Break lexbuf
    | "by"        -> mk_tok Step lexbuf
    | "case"      -> mk_tok HasType lexbuf
    | "else"      -> mk_tok Else lexbuf
    | "error"     -> mk_tok Error lexbuf
    | "exquo"     -> mk_tok Exquo lexbuf
    | "for"       -> mk_tok For lexbuf
    | "has"       -> mk_tok Has lexbuf
    | "if"        -> mk_tok If lexbuf
    | "import"    -> mk_tok Import lexbuf
    | "in"        -> mk_tok In lexbuf
    | "iterate"   -> mk_tok Continue lexbuf
    | "not"       -> mk_tok Not lexbuf
    | "or"        -> mk_tok Or lexbuf
    | "pretend"   -> mk_tok Pretend lexbuf
    | "quo"       -> mk_tok Quo lexbuf
    | "rem"       -> mk_tok Rem lexbuf
    | "repeat"    -> mk_tok Repeat lexbuf
    | "return"    -> mk_tok Return lexbuf
    | "then"      -> mk_tok Then lexbuf
    | "when"      -> mk_tok When lexbuf
    | "where"     -> mk_tok Where lexbuf
    | "with"      -> mk_tok With lexbuf
    | _ ->
        begin
          match name.[0] with
          | 'a'..'z' | '%' ->
              mk_tok Symbol lexbuf
          | _ ->
              mk_tok TypeName lexbuf
        end
  }

  (* Delimeters. *)
  | '\n' { new_line lexbuf; mk_tok Eol lexbuf }
  | eof { mk_tok Eof lexbuf }

  | _ as c { let pos = tokpos_from_lexbuf lexbuf
             and msg = Printf.sprintf "Unrecognized character '%c'" c
             in raise (LexerError (pos, msg)) }
