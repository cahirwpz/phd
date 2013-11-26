open Format
open ExtList
open ExtString
open Option
open Utils

type spadtype =
  | Array of spadtype (* to się mapuje na vector_1d_t *)
  | Cons of spadtype
  | SI | DF | Boolean | Void | Exit | Any
  | Mapping of spadtype list (* aka FunctionType *)
  | UserType of string * (spadtype list)
  | Record of (string * spadtype) list
  (*
   * Builtin types listed are in |$optimizableConstructorNames|.
   *
   * Symbol of string | String | Character
   * U32Vector | U16Vector | U8Vector 
   * Union of (string option * spadtype) list
   *)

type tree =
  | Apply of tree * tree list
  | ArrayRef of tree * tree
  | Assign of tree * tree
  | BinaryOp of string * tree * tree
  | Block of (string * spadtype) list * tree list
  | Break | Continue
  | Call of string * tree list
  | Char of char
  | Cons of tree * tree
  | Float of float
  | FunDecl of string * string list * spadtype
  | FunSymbol of string
  | Global of string * tree option
  | IfThen of tree * tree
  | IfThenElse of tree * tree * tree
  | Int of int
  | Lambda of string list * spadtype * tree
  | Loop of tree
  | Return of tree
  | String of string
  | Symbol of string
  | UnaryOp of string * tree
  | Var of string

let is_compound = function
  | Char _ | Float _ | Int _ | String _ | Symbol _ | Global _ | Var _ ->
      false
  | _ ->
      true

let list_of_group = function
  | Sexpr.Group l -> l
  | _ -> failwith "Not a group!"

(* conversion from S-Expressions *)

exception SyntaxError of string * Sexpr.sexpr;;
exception UnknownForm of string;;

let error = fun s exp -> raise (SyntaxError (s, Sexpr.Group exp))

(* all operators defined in pritimitives.lisp *)
let unary = [ "inc_SI"; "minus_SI"; "minus_DF"; "abs_DF"; "NOT" ]
let binary = [
  (* SingleInteger *)
  "add_SI"; "sub_SI"; "mul_SI"; "quo_SI"; "rem_SI";
  "eql_SI"; "less_SI"; "greater_SI";
  (* DoubleFloat *)
  "add_DF"; "sub_DF"; "mul_DF"; "div_DF";
  "eql_DF"; "less_DF"; "greater_DF";
  (* Boolean *)
  "AND"; "OR" ]

let graph = [
  '-'; '.'; ','; '&'; ':'; ';'; '*'; '%'; '}'; '{'; ']'; '['; '!'; '^'; '@';
  '~'; '('; ')'; '='; '<'; '>'; '#']

let is_simple_symbol s =
  not (List.exists (String.contains s) graph)

let translate_op = function
  | "AND" -> "and"
  | "OR" -> "or"
  | "NOT" -> "not"
  | "abs_DF" -> "abs"
  | "add_SI" | "add_DF" -> "+"
  | "minus_SI" | "minus_DF" | "sub_SI" | "sub_DF" -> "-"
  | "mul_SI" | "mul_DF" -> "*"
  | "quo_SI" | "div_DF" -> "/"
  | "rem_SI" -> "%"
  | "eql_SI" | "eql_DF" -> "=="
  | "less_SI" | "less_DF" -> "<"
  | "greater_SI" | "greater_DF" -> ">"
  | _ as x -> x

let rec convert exp =
  try
    begin
      match exp with
      | Sexpr.Float n -> Float n
      | Sexpr.Group g -> convert_group g
      | Sexpr.Int n -> Int n
      | Sexpr.Quote (Sexpr.Group g) ->
          let reduce = fun a b -> Cons (convert a, b) 
          in List.fold_right reduce g (Symbol "NIL")
      | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
      | Sexpr.String s -> String s
      | Sexpr.Symbol s -> Var s
      | e -> raise (SyntaxError ("Unknown s-expression", e))
    end
  with SyntaxError (s, exp) ->
    printf "@[<v 2>*SYNTAX ERROR* %s:@," s; Sexpr.print exp; printf "@]@.";
    Symbol "*invalid*"

and convert_group = function
  | [Sexpr.Symbol op; arg] when List.mem op unary ->
      UnaryOp (op, convert arg)
  | [Sexpr.Symbol op; arg1; arg2] when List.mem op binary ->
      BinaryOp (op, convert arg1, convert arg2)
  | (Sexpr.Symbol name)::body -> (
      try
        convert_spec_form name body
      with UnknownForm _ ->
        Call (name, List.map convert body))
  | (Sexpr.Group func)::body ->
      Apply (convert_group func, List.map convert body)
  | e -> error "Malformed group" e

and convert_spec_form = function
  | "BLOCK" -> convert_block
  | "BREAK" -> convert_break
  | "DEFPARAMETER" | "DEFVAR" -> convert_defvar
  | "DEFUN" -> convert_fun_def
  | "FUNCTION" -> convert_fun_symbol
  | "IF" -> convert_if
  | "LAMBDA" -> convert_lambda
  | "CONS" -> convert_cons
  | "LET" -> convert_let
  | "LOOP" -> convert_loop
  | "PROGN" -> convert_progn
  | "QAREF1" -> convert_aref
  | "QSETAREF1" -> convert_setaref
  | "RETURN" -> convert_return
  | "SDECLFUN" -> convert_typed_fun_decl
  | "SDEFUN" -> convert_typed_fun_def
  | "SETQ" -> convert_setq
  | "SPROG" -> convert_sprog
  | "char" -> convert_char
  | e -> raise (UnknownForm e)

and convert_block = function
  | (Sexpr.Symbol label)::rest ->
      Block ([], List.map convert rest)
  | e -> error "Malformed block s-form" e

and convert_break = function
  | [] -> Break
  | e -> error "Malformed break s-form" e

and convert_char = function
  | [Sexpr.Quote (Sexpr.Symbol c)] -> Char c.[0]
  | e -> error "Malformed char s-form" e

and convert_cons = function
  | [head; tail] -> Cons (convert head, convert tail)
  | e -> error "Malformed cons s-form" e

and convert_lambda = function
  | (Sexpr.Group args)::body ->
      let args = convert_symbol_list args in
      let types = List.make (List.length args + 1) Any in
      Lambda (args, Mapping types, convert_progn body)
  | e -> error "Malformed lambda s-form" e

and convert_loop body =
  Loop (convert_progn body)

and convert_defvar = function
  | [Sexpr.Symbol name; value] ->
      Global (name, Some (convert value))
  | [Sexpr.Symbol name] ->
      Global (name, None)
  | e -> error "Malformed defvar s-form" e

and convert_fun_def = function
  | [Sexpr.Symbol name; Sexpr.Group args; body] ->
      let args = convert_symbol_list args in
      let types = List.make (List.length args + 1) Any in
      Assign (Var name, Lambda (args, Mapping types, convert body))
  | e -> error "Malformed defun s-form" e

and convert_typed_fun_decl = function
  | [Sexpr.Symbol name; Sexpr.Group args; Sexpr.Group ret_type] ->
      let args, types = List.split @@ List.map extract_sprog_tvar args in
      FunDecl (name, args, Mapping (types @ [convert_type ret_type]))
  | e -> error "Malformed sdeclfun s-form" e

and convert_typed_fun_def = function
  | [Sexpr.Symbol name; Sexpr.Group args; body] ->
      let args, types = List.split @@ List.map convert_typed_symbol args in
      let tl1, tl2 = List.split_nth (List.length types - 1) types in
      let types = List.concat [tl1; [Any]; tl2] in
      Assign (Var name, Lambda (args, Mapping types, convert body))
  | e -> error "Malformed sdefun s-form" e

and convert_fun_symbol = function
  | [Sexpr.Symbol name] ->
      FunSymbol name
  | e -> error "Malformed function s-form" e

and convert_let = function
  | (Sexpr.Group symbols)::body ->
      let symbols = convert_symbol_list symbols in
      Block (List.map (fun (s) -> (s, Any)) symbols, List.map convert body)
  | e -> error "Malformed let s-form" e

and convert_return = function
  | [value] ->
      Return (convert value)
  | e -> error "Malformed return s-form" e

and convert_setq = function
  | [Sexpr.Symbol name; value] ->
      Assign (Var name, convert value)
  | e -> error "Malformed setq s-form" e

and convert_progn body =
  Block ([], List.map convert body)

and extract_sprog_tvar = function
  | Sexpr.Group [Sexpr.Symbol name; Sexpr.Group maybe_type] ->
      (name, convert_type maybe_type)
  | Sexpr.Group [Sexpr.Symbol name; Sexpr.Symbol "NIL"] ->
      (name, Any)
  | e -> error "Malformed sprog tvar s-from" [e]

and convert_sprog = function
  | (Sexpr.Group vars::exprs) ->
      Block (List.map extract_sprog_tvar vars, List.map convert exprs)
  | e -> error "Malformed sprog s-form" e

and convert_if = function
  | pred::if_true::[] ->
      IfThen (convert pred, convert if_true)
  | pred::if_true::if_false::[] ->
      IfThenElse (convert pred, convert if_true, convert if_false)
  | e -> error "Malformed if s-form" e

and convert_symbol_list = function
  | (Sexpr.Symbol symbol)::symbols -> symbol::(convert_symbol_list symbols)
  | [] -> []
  | e -> error "Expected a list of symbols" e

and convert_typed_symbol = function
  | Sexpr.Group (Sexpr.Symbol symbol::maybe_type) ->
      (symbol, convert_type maybe_type)
  | e -> error "Expected a type symbol" [e]

and convert_type = function
  | [Sexpr.Symbol atype] ->
      (match atype with
      | "SingleInteger" -> SI
      | "DoubleFloat" -> DF
      | "Boolean" -> Boolean
      | "Void" -> Void
      | "Exit" -> Exit
      | "List" -> Cons Any
      | "Any" | "NIL" -> Any
      | name -> UserType (name, []))
  | [Sexpr.Symbol "List"; Sexpr.Group maybe_type] ->
      Cons (convert_type maybe_type)
  | [Sexpr.Symbol "PrimitiveArray"; Sexpr.Group maybe_type] ->
      Array (convert_type maybe_type)
  | (Sexpr.Symbol "Mapping")::tys ->
      let fntype = List.map (fun (t) -> convert_type @@ list_of_group t) tys in
      Mapping (List.tl fntype @ [List.hd fntype])
  | (Sexpr.Symbol "Record")::fields ->
      let convert_field = (function
        | Sexpr.Group [Sexpr.Symbol ":"; Sexpr.Symbol name; Sexpr.Group atype] ->
            (name, convert_type atype)
        | e -> error "Cannot parse type of record field" [e]) in
      Record (List.map convert_field fields)
  | e -> error "Cannot parse type" e

and convert_aref = function
  | [Sexpr.Symbol aname; index] ->
      ArrayRef (Var aname, convert index)
  | e -> error "Malformed qaref1 s-form" e

and convert_setaref = function
  | [Sexpr.Symbol aname; index; value] ->
      Assign (ArrayRef (Var aname, convert index), convert value)
  | e -> error "Malformed qsetaref1 s-form" e

(* stringification *)
let rec print = function
  | Apply (func, args) ->
      print func; print_char '(';
      print_list print ", " args;
      print_char ')'
  | ArrayRef (name, index) ->
      print name; print_char '['; print index; print_char ']'
  | Assign (name, Lambda (args, Mapping types, body)) ->
      let targs = List.combine args (but_last types)
      and rtype = List.last types in
      printf "@[<v>fun "; print name;
      printf " (@[<hov>"; print_typed_symbols targs; printf "@]) : ";
      print_spadtype rtype; printf "@,"; print_fun_body body; printf "@]"
  | Assign (name, value) ->
      print name; print_string " := "; print value
  | Block (vars, tree) ->
      printf "@[<v>@[<v 2>begin@,"; print_block vars tree; printf "@]@,end@]"
  | Break ->
      print_string "break"
  | UnaryOp (op, x) ->
      printf "@[<hov>"; print_string (translate_op op); printf "("; print x;
      printf ")@]"
  | BinaryOp (op, x, y) ->
      printf "@[<hov>("; print x; printf "@ %s@ " (translate_op op); print y;
      printf ")@]"
  | Call (name, args) ->
      print_symbol name; print_char '(';
      print_list print ", " args;
      print_char ')'
  | Char c -> 
      printf "'%c'" c
  | Cons (a, Cons _) as lst ->
      printf "@[<1>["; print_cons lst; printf "]@]"
  | Cons (a, Var "NIL") ->
      printf "@[<1>["; print a; printf "]@]"
  | Cons (a, b) ->
      print_char '('; print a; printf " . "; print b; print_char ')'
  | Float n ->
      print_float n
  | FunDecl (name, args, Mapping types) ->
      let targs = List.combine args (but_last types)
      and rtype = List.last types in
      printf "@[<v>fun "; print_string name;
      printf " (@[<hov>"; print_typed_symbols targs; printf "@]) : ";
      print_spadtype rtype; printf "@]"
  | IfThen (pred, if_true) ->
      printf "@[<v>";
      printf "@[<v 2>if@,"; print_inline pred; printf "@]@,";
      printf "@[<v 2>then@,"; print_inline if_true; printf "@]@,endif@]"
  | IfThenElse (pred, if_true, if_false) ->
      printf "@[<v>";
      printf "@[<v 2>if@,"; print_inline pred; printf "@]@,";
      printf "@[<v 2>then@,"; print_inline if_true; printf "@]@,";
      printf "@[<v 2>else@,"; print_inline if_false; printf "@]@,endif@]"
  | Int n ->
      print_int n
  | Global (name, value) ->
      printf "global %s" name;
      if is_some value then
        (printf " := "; print (get value))
  | Lambda (args, Mapping types, body) ->
      let targs = List.combine args (but_last types)
      and rtype = List.last types in
      printf "@[<v>fn (@[<hov>"; print_typed_symbols targs; printf "@]) : ";
      print_spadtype rtype; printf " -> @,";
      print_fun_body body; printf "@] "
  | Loop body ->
      printf "@[<v>@[<v 2>loop@,"; print_inline body; printf "@]@,endloop@]"
  | Return tree ->
      printf "return "; print tree
  | String str ->
      printf "\"%s\"" (String.escaped str)
  | Symbol "NIL" -> print_string "false"
  | Symbol "T" -> print_string "true"
  | FunSymbol name | Symbol name ->
      print_char '\''; print_symbol name
  | Var name ->
      print_symbol name
  | _ ->
      failwith "not handled"

and print_inline = function
  | Block (vs, exps) -> print_block vs exps
  | x -> print x

and print_cons = function
  | Cons (a, (Cons _ as rest)) ->
      print a; printf ",@ "; print_cons rest
  | Cons (a, Var "NIL") ->
      print a
  | _ -> failwith "Not a list."

and print_fun_body = function
  | Block (_, _) as body ->
      print body
  | _ as body ->
      printf "@[<v 2>begin@,"; print body; printf "@]@,end"

and print_block vars exps =
  if List.length vars > 0 then
    (printf "var @["; print_typed_symbols vars; printf "@]@,");
  print_list print "@," exps

and string_of_spadtype = function
  | Array atype -> sprintf "PrimitiveArray(%s)" (string_of_spadtype atype)
  | Boolean -> "Boolean"
  | Cons atype -> sprintf "List(%s)" (string_of_spadtype atype)
  | DF -> "DoubleFloat"
  | Exit -> "Exit"
  | Any -> "Any"
  | Mapping ts ->
      String.join " -> " (List.map string_of_spadtype ts)
  | Record fields ->
      let string_of_field (sym, typ) =
        sprintf "%s : %s" sym (string_of_spadtype typ) in
      sprintf "{%s}" (String.join ", " (List.map string_of_field fields))
  | SI -> "SingleInteger"
  | UserType (name, []) -> name
  | UserType (name, ts) ->
      sprintf "%s(%s)" name (String.join ", " (List.map string_of_spadtype ts))
  | Void -> "Void"

and print_spadtype t = print_string @@ string_of_spadtype t

and print_symbol name = 
  if is_simple_symbol name then
    print_string name
  else
    (print_char '|'; print_string name; print_char '|')

and print_typed_symbol (name, atype) =
  print_symbol name; print_string " : "; print_spadtype atype

and print_symbols lst = print_list print_symbol ",@ " lst

and print_typed_symbols lst = print_list print_typed_symbol ",@ " lst
