{
  open Lexing
  open Lextools
  open Strbuf
  open Token
  open Tokpos

  let count_spaces spaces =
    let l = ref 0 in
    let counter c = 
      l := !l + (if c = '\t' then 8 else 1)
    in String.iter counter spaces; !l

  let unknown_char lexbuf c = 
    let s = (tokpos_from_lexbuf lexbuf)#as_string in
    Printf.sprintf "%s: Unrecognized character '%c'" s c
}

let digit = ['0'-'9']
let space = [' ' '\t']
let alpha = ['a'-'z' 'A'-'Z']
let graph = ['!' '?' '\'']
let symbol = alpha (alpha | digit | graph)*
let comment = [^ '\n']+

let integer = digit+
let real = digit+ '.' digit*

rule token = parse
  (* Indentation: [\s\t]+ *)
  | space+ as spaces { Indent (count_spaces spaces) }

  (* Escaping delimeter. *)
  | '_' '\n' { new_line lexbuf; Lc }
  | '_' { token lexbuf }
  
  (* Strings (the contents is being unescaped). *)
  | '"' { lex_string (new strbuf) lexbuf }

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
  | '%' { Self }
  | '\'' { Quote }

  (* Unambiguous two character tokens: '\/' *)
  | '\\' '/' { Or }

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
  | "++" { lex_comment (new strbuf) lexbuf }
  | '+' { Plus }

  (* Ambiguous tokens that begin with minus character - '-', '->', '--'. *)
  | "--" { lex_comment (new strbuf) lexbuf }
  | "->" { Arrow }
  | "-" { Minus }

  (* Ambiguous tokens that begin with dot character - '.', '..'. *)
  | ".." { Ellipsis }
  | '.' { Dot }

  (* Ambiguous tokens that begin with slash character - '/', '/\'. *)
  | '/' '\\' { And }
  | '/' { By }

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
    | "add"     -> Add
    | "and"     -> And
    | "break"   -> Break
    | "by"      -> Step
    | "case"    -> HasType
    | "else"    -> Else
    | "error"   -> Error
    | "exquo"   -> Exquo
    | "for"     -> For
    | "has"     -> Has
    | "if"      -> If
    | "import"  -> Import
    | "in"      -> In
    | "iterate" -> Continue
    | "not"     -> Not
    | "or"      -> Or
    | "pretend" -> Pretend
    | "quo"     -> Quo
    | "rem"     -> Rem
    | "return"  -> Return
    | "then"    -> Then
    | "when"    -> When
    | "where"   -> Where
    | "with"    -> With
    | _ ->
        begin
          match name.[0] with
          | 'a'..'z' ->
              Name name
          | _ ->
              TypeName name
        end
  }

  (* Delimeters. *)
  | '\n' { new_line lexbuf; Eol }
  | eof { Eof }
  | _ as c { failwith (unknown_char lexbuf c) }

and lex_comment buf = parse
  | [^'\n']+ as str { Comment str }
  | '\n' { Eol }

and lex_string buf = parse
  | '"' { String buf#gets }
  | '\\' (_ as c) { buf#putc '\\'; buf#putc c; lex_string buf lexbuf }
  | _ as c { buf#putc c; lex_string buf lexbuf }
