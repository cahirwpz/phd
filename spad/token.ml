type kind = [`space | `separator | `string | `symbol | `keyword | `operator |
             `number | `comment | `reserved | `builtin]

type token =
  (* Indentation and formatting. Lc = line continuation ('_\n'). *)
  | Indent of int | Eol | Eof | Lc

  (* Comment that begins with '--' or '++'. *)
  | Comment of string

  (* Name that begins with capital letter or is '%'. *)
  | TypeName of string

  (* Name that begins with small letter, may end with [!?']. *)
  | Name of string

  (* Basic types. *)
  | Float of float | Int of int | String of string

  (* Expression grouping, etc. *)
  | LParen | RParen

  (* Lists and list comprehensions. *)
  | LBracket | RBracket

  (* Type (':') and function ('->') specification. *)
  | OfType | Arrow

  (* -=[ operators ]=------------------------------------------------------- *)

  (* 
   * X::T = explicitly convert X to type T if possible.
   * X$T  = use the available operators for type T to compute X.
   * X@T  = choose operators to compute X so that the result is of type T.
   * X pretend T = forceful casting.
   *)
  | ToType | UsesType | ReturnsType | Pretend

  (* X case T = if X is of T type. *)
  | HasType 

  (* Assignment (':='), delayed binding ('==') and macro ('==>') operators. *)
  | Assign | Bind | Macro

  (* Lambda expression binding operator ('+->'). *)
  | Lambda

  (* Range construction operator '..'. *)
  | Ellipsis

  (* Separators ',' and ';'. *)
  | Comma | Semicolon

  (* Arithmetic operators '+', '-', '*', '/', '^' *)
  | Plus | Minus | Times | Div | Pow

  (* Extra arithmetic operators 'quo', 'exquo', 'rem' *)
  | Quo | Exquo | Rem

  (* Comparison operators: '<', '<=', '=', '~=', '>=', '>'. *)
  | Lt | Le | Eq | Ne | Ge | Gt

  (* Boolean operators: 'and', '/\', 'or', '\/', 'not', '~'. *)
  | And | Or | Not

  (* Structure accessor '.'. *)
  | Dot

  (* Container length '#'. *)
  | Length

  (* Quote ? *)
  | Quote

  (* If in list comprehensions. *)
  | Bar

  (* -=[ keywords ]=-------------------------------------------------------- *)

  (*
   * Type construction:
   * 'add', 'with', 'import', 'where'.
   *)
  | Add | With | Import | Where

  (* Control flow: 'if', 'then', 'else', '=>', 'return', 'error'. *)
  | If | Then | Else | When | Return | Error

  (* Control flow - loops: 'for', 'in', 'by', 'repeat', 'break', 'iterate'. *)
  | For | In | Repeat | Step | Break | Continue

  (* Type handling related: 'has'. *)
  | Has

let as_string = function
  | Add -> "add"
  | And -> "and"
  | Arrow -> "->"
  | Assign -> ":="
  | Bar -> "|"
  | Bind -> "=="
  | Break -> "break"
  | Comma -> ","
  | Comment s -> s
  | Continue -> "iterate"
  | Div -> "/"
  | Dot -> "."
  | Ellipsis -> ".."
  | Else -> "else"
  | Eof -> ""
  | Eol -> "\n"
  | Eq -> "="
  | Error -> "error"
  | Exquo -> "exquo"
  | Float f -> string_of_float f
  | For -> "for"
  | Ge -> ">=" 
  | Gt -> ">"
  | Has -> "has"
  | HasType -> "case"
  | If -> "if"
  | Import -> "import"
  | In -> "in"
  | Indent n -> String.make n ' '
  | Int i -> string_of_int i
  | LBracket -> "["
  | LParen -> "("
  | Lambda -> "+->"
  | Lc -> "_\n"
  | Le -> "<="
  | Length -> "#"
  | Lt -> "<"
  | Macro -> "==>"
  | Minus -> "-"
  | Name name | TypeName name -> name
  | Ne -> "~="
  | Not -> "not"
  | OfType -> ":"
  | Or -> "or"
  | Plus -> "+"
  | Pow -> "^"
  | Pretend -> "pretend"
  | Quo -> "quo"
  | Quote -> "'"
  | RBracket -> "]"
  | RParen -> ")"
  | Rem -> "rem"
  | Repeat -> "repeat"
  | Return -> "return"
  | ReturnsType -> "@"
  | Semicolon -> ";"
  | Step -> "by"
  | String str -> "\"" ^ str ^ "\""
  | Then -> "then"
  | Times -> "*"
  | ToType -> "::"
  | UsesType -> "$"
  | When -> "=>"
  | Where -> "where"
  | With -> "with"

let token_kind = function
  | Indent _ | Eol | Lc | Eof -> `space
  | Comment _ -> `comment
  | Name _ | TypeName _ -> `symbol
  | Float _ | Int _ -> `number
  | String _ -> `string
  | LParen | RParen | LBracket | RBracket | Comma | Semicolon -> `separator
  | Assign | Bind | Macro | Ellipsis
  | Plus | Minus | Times | Pow | Div | Lt | Le | Eq | Ne | Ge | Gt
  | Dot | Length | Quote | Bar | When | OfType | Arrow | ToType
  | UsesType | ReturnsType | Lambda -> `operator
  | If | Then | Else | For | In | Repeat | Break | Return | Step | Where | And
  | Or | Not | Quo | Rem | Exquo | Add | With | Import | Pretend | HasType | Has
  | Error | Continue -> `keyword
