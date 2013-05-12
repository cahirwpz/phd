open Lextools

type token_kind = [
  `space | `separator | `string | `symbol | `keyword | `operator | `number |
  `comment | `reserved | `builtin]

type token_type =
  (* Indentation and formatting. Lc = line continuation ('_\n'). *)
  | Indent | Eol | Eof | Lc

  (* Comment that begins with '--' or '++'. *)
  | Comment

  (* Name that begins with small letter, may end with [!?']. *)
  | Symbol

  (* Basic types. *)
  | Float | Int | String

  (* Expression grouping, etc. *)
  | LParen | RParen

  (* Lists and list comprehensions. *)
  | LBracket | RBracket

  (* Rest of a list. *)
  | Colon

  (* Package. *)
  | Package

  (* -=[ operators ]=------------------------------------------------------- *)

  (* Assignment (':='), delayed binding ('==') and macro ('==>') operators. *)
  | Assign | Bind | Macro

  (* Range construction operator '..'. *)
  | Ellipsis

  (* Separators ',' and ';'. *)
  | Comma | Semicolon

  (* Arithmetic operators '+', '-', '*', '/', '^' *)
  | Plus | Minus | Times | Div | Pow

  (* Comparison operators: '<', '<=', '=', ~=', '>=', '>'. *)
  | Lt | Le | Eq | Ne | Ge | Gt

  (* Boolean operators: 'and', 'or', 'not'. *)
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

  (* Control flow: 'if', 'then', 'else', '=>', 'return'. *)
  | If | Then | Else | When | Return

  (* Loops: 'for', 'in', 'repeat', 'by', 'break', 'while', 'until'. *)
  | For | In | Repeat | Step | Break | While | Until

  (* Other keywords. *)
  | Case | Is | IsNot | Of | Where | Local

type boot_token = token_type Lextools.token

let kind_of_token t =
  match t.typ with
  | Indent | Eol | Lc | Eof -> `space
  | Comment -> `comment
  | Symbol -> `symbol
  | Float | Int -> `number
  | String -> `string
  | LParen | RParen | LBracket | RBracket | Comma | Semicolon -> `separator
  | Colon | Assign | Bind | Macro | Ellipsis
  | Plus | Minus | Times | Pow | Div | Lt | Le | Eq | Ne | Ge | Gt
  | Dot | Length | Quote | Bar | When -> `operator
  | Package -> `reserved
  | If | Then | Else | For | In | Repeat | Break | Return | Step | While | Local
  | Until | Case | Is | IsNot | Of | Where | And | Or | Not -> `keyword
