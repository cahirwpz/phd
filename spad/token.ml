open Lextools

type token_kind = [
  `space | `separator | `string | `symbol | `keyword | `operator | `number |
  `comment | `reserved | `builtin | `typename ]

type token_type =
  (* Indentation and formatting. Lc = line continuation ('_\n'). *)
  | Indent | Eol | Eof | Lc

  (* Comment that begins with '--' or '++'. *)
  | Comment

  (* Name that begins with capital letter or is '%'. *)
  | TypeName

  (* Symbol that begins with small letter, may end with [!?']. *)
  | Symbol

  (* Directives. *)
  | Abbrev

  (* Basic types. *)
  | Float | Int | String

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

  (* Loops: 'for', 'in', 'by', 'repeat', 'break', 'iterate', 'while'. *)
  | For | In | Repeat | Step | Break | Continue | While

  (* Type handling related: 'has'. *)
  | Has

type spad_token = token_type token

let kind_of_token t =
  match t.typ with
  | Indent | Eol | Lc | Eof -> `space
  | Comment -> `comment
  | Abbrev -> `reserved
  | Symbol -> `symbol
  | TypeName -> `typename
  | Float | Int -> `number
  | String -> `string
  | LParen | RParen | LBracket | RBracket | Comma | Semicolon -> `separator
  | Assign | Bind | Macro | Ellipsis
  | Plus | Minus | Times | Pow | Div | Lt | Le | Eq | Ne | Ge | Gt
  | Dot | Length | Quote | Bar | When | OfType | Arrow | ToType
  | UsesType | ReturnsType | Lambda -> `operator
  | If | Then | Else | For | In | Repeat | Break | Return | Step | Where | And
  | Or | Not | Quo | Rem | Exquo | Add | With | Import | Pretend | HasType | Has
  | Error | Continue | While -> `keyword
