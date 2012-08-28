type token =
  (* Indentation and formatting. Cl = continue line ('_\n'). *)
  | Indent of int | Eol | Eof | Cl

  (* Name that begins with capital letter or is '%'. *)
  | TypeName of string

  (* Name that begins with small letter, may end with [!?']. *)
  | Name of string

  (* Basic types. *)
  | Float of float | Int of int | String of string | Char of char

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
  | IfType 

  (* Assignment (':='), delayed binding ('==') and macro ('==>') operators. *)
  | Assign | Bind | Macro

  (* Lambda expression binding operator ('+->'). *)
  | Lambda

  (* Range construction operator '..'. *)
  | Ellipsis

  (* Separators ',' and ';'. *)
  | Comma | Semicolon

  (* Arithmetic operators '+', '-', '*', '/', '^' *)
  | Plus | Minus | Mul | Div | Pow

  (* Extra arithmetic operators 'quo', 'exquo', 'rem' *)
  | Quo | Exquo | Rem

  (* Comparison operators: '<', '<=', '=', '~=', '>=', '>'. *)
  | Lt | Le | Eq | Ne | Ge | Gt

  (* Boolean operators: 'and', '/\', 'or', '\/', 'not', '~'. *)
  | And | Or | Not

  (* Structure accessor '.'. *)
  | Dot

  (* -=[ keywords ]=-------------------------------------------------------- *)

  (*
   * Type construction:
   * 'add', 'with', 'Exports', 'import', 'where', 'Implementation'.
   *)
  | Add | With | Exports | Import | Where | Implementation

  (* Control flow: 'if', 'then', 'else', '=>', 'return', 'error'. *)
  | If | Then | Else | When | Return | Error

  (* Control flow - loops: 'for', 'in', 'by', 'repeat', 'break', 'iterate'. *)
  | For | In | Repeat | By | Break | Continue

  (* Type handling related: 'has'. *)
  | Has
