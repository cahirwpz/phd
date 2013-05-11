{
  open Lexing
  open Lextools
  open Strbuf
  open Token
  open Wordpos
  open Num

  let count_spaces spaces =
    let l = ref 0 in
    let counter c = 
      l := !l + (if c = '\t' then 8 else 1)
    in String.iter counter spaces; !l

  exception Failure of wordpos * string
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
let comment = [^ '\n']+

let integer = digit+
let real = digit+ '.' digit+

rule token = parse
  (* Indentation: [\s\t]+ *)
  | space+ as spaces { Indent (count_spaces spaces) }

  (* Escaping delimeter. *)
  | '_' '\n' { new_line lexbuf; Lc }
  
  (* Strings (the contents will be unescaped). *)
  | '"' { lex_string (new strbuf) lexbuf }

  (* Multiline comments. *)
  | ")if false" as p { lex_mlcomment (new strbuf_with p) lexbuf }
  | ")package" { Package }

  (* One character tokens: '()[]*,;^#|' *)
  | '(' { LParen }
  | ')' { RParen }
  | '[' { LBracket }
  | ']' { RBracket }
  | '*' { Times }
  | ',' { Comma }
  | ';' { Semicolon }
  | '^' { Pow }
  | '#' { Length }
  | '|' { Bar }
  | '\'' { Quote }

  (* ':', ':=' *)
  | ":=" { Assign }
  | ':' { Colon }

  (* '=', '==', '=>', '==>' *)
  | "==>" { Macro }
  | "==" { Bind }
  | "=>" { When }
  | '=' { Eq }

  (* '+', '++' *)
  | "++" as p { lex_comment (new strbuf_with p) lexbuf }
  | '+' { Plus }

  (* '-', '--' *)
  | "--" as p { lex_comment (new strbuf_with p) lexbuf }
  | "-" { Minus }

  (* '.', '..' *)
  | ".." { Ellipsis }
  | '.' { Dot }

  (* '<', '<=' *)
  | "<=" { Le }
  | '<' { Lt }

  (* '>', '>=' *)
  | ">=" { Ge }
  | '>' { Gt }

  (* '/', '~=' *)
  | "~=" { Ne }
  | '/' { Div }

  (* number: [0-9.]+ *)
  | real as num { Float (float_of_string num) }
  | integer as num { Num (num_of_string num) }

  (* Finally identifiers: functions, variables, types, etc. *)
  | symbol as name { 
    match name with
    | "and"       -> And
    | "break"     -> Break
    | "by"        -> Step
    | "else"      -> Else
    | "for"       -> For
    | "if"        -> If
    | "in"        -> In
    | "is"        -> Is
    | "isnt"      -> IsNot
    | "local"     -> Local
    | "not"       -> Not
    | "of"        -> Of
    | "or"        -> Or
    | "repeat"    -> Repeat
    | "return"    -> Return
    | "then"      -> Then
    | "until"     -> Until
    | "where"     -> Where
    | "while"     -> While
    | _           -> Name name
  }

  (* Delimeters. *)
  | '\n' { new_line lexbuf; Eol }
  | eof { Eof }

  | _ as c { let pos = wordpos_from_lexbuf lexbuf
             and msg = Printf.sprintf "Unrecognized character '%c'" c
             in raise (Failure (pos, msg)) }

and lex_comment buf = parse
  | [^'\n']* as s { buf#puts s; Comment buf#gets }

and lex_mlcomment buf = parse
  | ")endif" as s { buf#puts s; Comment buf#gets }
  | _ as c {
    if c = '\n' then new_line lexbuf; buf#putc c; lex_mlcomment buf lexbuf }

and lex_string buf = parse
  | '"' { String buf#gets }
  | '_' (_ as c) { buf#putc '_'; buf#putc c; lex_string buf lexbuf }
  | _ as c {
    if c = '\n' then new_line lexbuf; buf#putc c; lex_string buf lexbuf }
