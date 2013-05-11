open Num

type kind = [`space | `separator | `string | `symbol | `keyword | `operator |
             `number | `comment | `reserved | `builtin]

type token =
  (* Indentation and formatting. Lc = line continuation ('_\n'). *)
  | Indent of int | Eol | Eof | Lc

  (* Comment that begins with '--' or '++'. *)
  | Comment of string

  (* Name that begins with small letter, may end with [!?']. *)
  | Name of string

  (* Basic types. *)
  | Float of float | Num of num | String of string

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

let as_string = function
  | And -> "and"
  | Assign -> ":="
  | Bar -> "|"
  | Bind -> "=="
  | Break -> "break"
  | Case -> "case"
  | Colon -> ":"
  | Comma -> ","
  | Comment text -> text
  | Div -> "/"
  | Dot -> "."
  | Ellipsis -> ".."
  | Else -> "else"
  | Eof -> ""
  | Eol -> "\n"
  | Eq -> "="
  | Float f -> string_of_float f
  | For -> "for"
  | Ge -> ">=" 
  | Gt -> ">"
  | If -> "if"
  | In -> "in"
  | Indent n -> String.make n ' '
  | Is -> "is"
  | IsNot -> "isnt"
  | LBracket -> "["
  | LParen -> "("
  | Lc -> "_\n"
  | Le -> "<="
  | Length -> "#"
  | Local -> "local"
  | Lt -> "<"
  | Macro -> "==>"
  | Minus -> "-"
  | Name name -> name
  | Ne -> "~="
  | Not -> "not"
  | Num i -> string_of_num i
  | Of -> "of"
  | Or -> "or"
  | Package -> ")package"
  | Plus -> "+"
  | Pow -> "^"
  | Quote -> "'"
  | RBracket -> "]"
  | RParen -> ")"
  | Repeat -> "repeat"
  | Return -> "return"
  | Semicolon -> ";"
  | Step -> "by"
  | String str -> "\"" ^ str ^ "\""
  | Then -> "then"
  | Times -> "*"
  | Until -> "until"
  | When -> "=>"
  | Where -> "where"
  | While -> "while"

let token_kind = function
  | Indent _ | Eol | Lc | Eof -> `space
  | Comment _ -> `comment
  | Name _ -> `symbol
  | Float _ | Num _ -> `number
  | String _ -> `string
  | LParen | RParen | LBracket | RBracket | Comma | Semicolon -> `separator
  | Colon | Assign | Bind | Macro | Ellipsis
  | Plus | Minus | Times | Pow | Div | Lt | Le | Eq | Ne | Ge | Gt
  | Dot | Length | Quote | Bar | When -> `operator
  | Package -> `reserved
  | If | Then | Else | For | In | Repeat | Break | Return | Step | While | Local
  | Until | Case | Is | IsNot | Of | Where | And | Or | Not -> `keyword
