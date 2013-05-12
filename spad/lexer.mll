{
  open Lexing
  open Lextools
  open Token
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
let comment = [^ '\n']+

let integer = digit+
let real = digit+ '.' digit+

rule token = parse
  (* Indentation: [\s\t]+ *)
  | space+ as spaces { Indent (count_spaces spaces) }

  (* Escaping delimeter. *)
  | '_' '\n' { new_line lexbuf; Lc }
  
  (* Strings (the contents will unescaped). *)
  | '"' { lex_string (new strbuf) lexbuf }

  (* Directives. *)
  | ")abbrev" { Abbrev }

  (* One character tokens: '()[]$@*,;^' *)
  | '(' { LParen }
  | ')' { RParen }
  | '[' { LBracket }
  | ']' { RBracket }
  | '$' { UsesType }
  | '@' { ReturnsType }
  | '*' { Times }
  | ',' { Comma }
  | ';' { Semicolon }
  | '^' { Pow }
  | '#' { Length }
  | '%' { TypeName "%" }
  | '|' { Bar }
  | '\'' { Quote }

  (* Unambiguous two character tokens: '\/' *)
  | '\\' ('_')? '/' { Or }

  (* Ambiguous tokens that begin with colon character - ':', ':=', '::'. *)
  | ":=" { Assign }
  | "::" { ToType }
  | ':' { OfType }

  (* Ambiguous tokens that begin with equals character - '=', '==', '==>'. *)
  | "==>" { Macro }
  | "==" { Bind }
  | '=' { Eq }

  (* Ambiguous tokens that begin with plus character - '+', '++', '+->'. *)
  | "+->" { Lambda }
  | "++" as p { lex_comment (strbuf_from_str p) lexbuf }
  | '+' { Plus }

  (* Ambiguous tokens that begin with minus character - '-', '->', '--'. *)
  | "--" as p { lex_comment (strbuf_from_str p) lexbuf }
  | "->" { Arrow }
  | "-" { Minus }

  (* Ambiguous tokens that begin with dot character - '.', '..'. *)
  | ".." { Ellipsis }
  | '.' { Dot }

  (* Ambiguous tokens that begin with slash character - '/', '/\'. *)
  | '/' ('_')? '\\' { And }
  | '/' { Div }

  (* Ambiguous tokens that begin with less-than character - '<', '<='. *)
  | "<=" { Le }
  | '<' { Lt }

  (* Ambiguous tokens that begin with greater-than character - '>', '>='. *)
  | ">=" { Ge }
  | '>' { Gt }

  (* Ambiguous tokens that begin with greater-than character - '~', '~='. *)
  | "~=" { Ne }
  | '~' { Not }

  (* number: [0-9.]+ *)
  | real as num { Float (float_of_string num) }
  | integer as num { Int (int_of_string num) }

  (* Finally identifiers: functions, variables, types, etc. *)
  | symbol as name { 
    match name with
    | "add"       -> Add
    | "and"       -> And
    | "break"     -> Break
    | "by"        -> Step
    | "case"      -> HasType
    | "else"      -> Else
    | "error"     -> Error
    | "exquo"     -> Exquo
    | "for"       -> For
    | "has"       -> Has
    | "if"        -> If
    | "import"    -> Import
    | "in"        -> In
    | "iterate"   -> Continue
    | "not"       -> Not
    | "or"        -> Or
    | "pretend"   -> Pretend
    | "quo"       -> Quo
    | "rem"       -> Rem
    | "repeat"    -> Repeat
    | "return"    -> Return
    | "then"      -> Then
    | "when"      -> When
    | "where"     -> Where
    | "with"      -> With
    | _ ->
        begin
          match name.[0] with
          | 'a'..'z' | '%' ->
              Name name
          | _ ->
              TypeName name
        end
  }

  (* Delimeters. *)
  | '\n' { new_line lexbuf; Eol }
  | eof { Eof }

  | _ as c { let pos = wordpos_from_lexbuf lexbuf
             and msg = Printf.sprintf "Unrecognized character '%c'" c
             in raise (LexerError (pos, msg)) }

and lex_comment buf = parse
  | [^'\n']* as s { buf#puts s; Comment buf#gets }

and lex_string buf = parse
  | '"' { String buf#gets }
  | '_' (_ as c) { buf#putc '_'; buf#putc c; lex_string buf lexbuf }
  | _ as c {
    if c = '\n' then new_line lexbuf; buf#putc c; lex_string buf lexbuf }
