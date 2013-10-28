open Format
open ExtList
open Utils

type spadtype = SI | DF | Cons | Boolean | Generic

type tree =
  | Apply of tree * tree list
  | Call of string * tree list
  | Assign of string * tree
  | UnaryOp of string * tree
  | BinaryOp of string * tree * tree
  | Block of (string * spadtype) list * tree list
  | Char of char
  | Cons of tree * tree
  | Float of float
  | IfThen of tree * tree
  | IfThenElse of tree * tree * tree
  | Global of string * tree option
  | Int of int
  | Lambda of string list * tree
  | TypedLambda of (string * spadtype) list * spadtype * tree
  | Return of tree
  | String of string
  | Symbol of string
  | Value of string
  | While of tree * tree

let is_compound = function
  | Char _ | Float _ | Int _ | String _ | Symbol _ | Global _ | Value _ ->
      false
  | _ ->
      true

(* conversion from S-Expressions *)

exception SyntaxError of string * Sexpr.sexpr;;
exception UnknownForm of string;;

let error = fun s exp -> raise (SyntaxError (s, Sexpr.Group exp))

(* all operators defined in pritimitives.lisp *)
let unary = [ "|minus_SI|"; "|minus_DF"; "|abs_DF|"; "NOT" ]
let binary = [
  (* SingleInteger *)
  "|add_SI|"; "|sub_SI|"; "|mul_SI|"; "|div_SI|";
  "|eql_SI|"; "|less_SI|"; "|greater_SI|";
  (* DoubleFloat *)
  "|add_DF|"; "|sub_DF|"; "|mul_DF|"; "|div_DF|";
  "|eql_DF|"; "|less_DF|"; "|greater_DF|";
  (* Boolean *)
  "AND"; "OR" ]

let graph = [
  '-'; '.'; ','; '&'; ':'; ';'; '*'; '%'; '}'; '{'; ']'; '['; '!'; '^'; '@';
  '~'; '('; ')'; '='; '<'; '>'; '#']

let is_simple_symbol s =
  let contains = String.contains s in
  not (List.exists contains graph)

let literal_symbol s =
  let l = String.length s in
  if l > 2 && (s.[l-1] = s.[0] && s.[0] = '|')
  then String.sub s 1 (l - 2)
  else s

let translate_op = function
  | "AND" -> "and"
  | "OR" -> "or"
  | "NOT" -> "not"
  | "abs_DF" -> "abs"
  | "add_SI" | "add_DF" -> "+"
  | "sub_SI" | "sub_DF" -> "-"
  | "mul_SI" | "mul_DF" -> "*"
  | "div_SI" | "div_DF" -> "/"
  | "eql_SI" | "eql_DF" -> "="
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
          in List.fold_right reduce g (Symbol "nil")
      | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
      | Sexpr.String s -> String s
      | Sexpr.Symbol s -> Value s
      | e -> raise (SyntaxError ("Unknown s-expression", e))
    end
  with SyntaxError (s, exp) ->
    printf "@[<v 2>*SYNTAX ERROR* %s:@," s; Sexpr.print exp; printf "@]@.";
    Symbol "*invalid*"

and convert_group = function
  | [Sexpr.Symbol op; arg] when List.mem op unary ->
      UnaryOp (literal_symbol op, convert arg)
  | [Sexpr.Symbol op; arg1; arg2] when List.mem op binary ->
      BinaryOp (literal_symbol op, convert arg1, convert arg2)
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
  | "char" -> convert_char
  | "DEFPARAMETER"
  | "DEFVAR" -> convert_defvar
  | "DEFUN" -> convert_fun_decl
  | "SDEFUN" -> convert_typed_fun_decl
  | "IF" -> convert_if
  | "LAMBDA" -> convert_lambda
  | "LET" -> convert_let
  | "LOOP" -> convert_loop
  | "SPROG" -> convert_sprog
  | "PROGN" -> convert_progn
  | "RETURN" -> convert_return
  | "SETQ" -> convert_setq
  | "WHILE" -> convert_while
  | e -> raise (UnknownForm e)

and convert_block = function
  | (Sexpr.Symbol label)::rest ->
      Block ([], List.map convert rest)
  | e -> error "Malformed block s-form" e

and convert_char = function
  | [Sexpr.Quote (Sexpr.Symbol c)] -> Char c.[0]
  | e -> error "Malformed char s-form" e

and convert_lambda = function
  | (Sexpr.Group args)::body ->
      Lambda (convert_symbol_list args, convert_progn body)
  | e -> error "Malformed lambda s-form" e

and convert_loop body =
  While (Symbol "T", convert_progn body)

and convert_defvar = function
  | [Sexpr.Symbol name; value] ->
      Global (name, Some (convert value))
  | [Sexpr.Symbol name] ->
      Global (name, None)
  | e -> error "Malformed defvar s-form" e

and convert_fun_decl = function
  | [Sexpr.Symbol name; Sexpr.Group args; body] ->
      Assign (name, Lambda (convert_symbol_list args, convert body))
  | e -> error "Malformed defun s-form" e

and convert_typed_fun_decl = function
  | [Sexpr.Symbol name; Sexpr.Group args; body] ->
      let targs = List.map convert_typed_symbol args in
      let (rsym, rtype) = List.last targs in
      Assign (name, TypedLambda ((but_last targs) @ [(rsym, Generic)], rtype, convert body))
  | e -> error "Malformed sdefun s-form" e

and convert_let = function
  | (Sexpr.Group symbols)::body ->
      let symbols = convert_symbol_list symbols in
      Block (List.map (fun (s) -> (s, Generic)) symbols, List.map convert body)
  | e -> error "Malformed let s-form" e

and convert_return = function
  | [value] ->
      Return (convert value)
  | e -> error "Malformed return s-form" e

and convert_setq = function
  | [Sexpr.Symbol name; value] ->
      Assign (name, convert value)
  | e -> error "Malformed setq s-form" e

and convert_progn body =
  Block ([], List.map convert body)

and extract_sprog_tvar = function
  | Sexpr.Group [Sexpr.Symbol name; maybe_type] ->
      (match maybe_type with 
      | Sexpr.Group (Sexpr.Symbol atype::_) ->
         (name, convert_type atype)
      | _ ->
          (name, Generic)
      )
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

and convert_while = function
  | pred::body ->
      While (convert pred, convert_progn body)
  | e -> error "Malformed while s-form" e

and convert_symbol_list = function
  | (Sexpr.Symbol symbol)::symbols -> symbol::(convert_symbol_list symbols)
  | [] -> []
  | e -> error "Expected a list of symbols" e

and convert_typed_symbol = function
  | Sexpr.Group (Sexpr.Symbol symbol::maybe_type) ->
      (match maybe_type with
      | (Sexpr.Symbol atype::_) ->
          (symbol, convert_type atype)
      | _ ->
          (symbol, Generic)
      )
  | e -> error "Expected a type symbol" [e]

and convert_type atype =
  match literal_symbol atype with
  | "SingleInteger" -> SI
  | "DoubleFloat" -> DF
  | "Boolean" -> Boolean
  | "List" -> Cons
  | _ -> Generic

(* stringification *)
let rec print = function
  | Apply (func, args) ->
      print func; print_char '(';
      iter_join print (fun () -> printf ", ") args;
      print_char ')'
  | Assign (name, Lambda (args, body)) ->
      printf "@[<v>def "; print_symbol name;
      printf "(@[<hov>"; print_symbols args; printf "@])@,";
      print_fun_body body; printf "@]"
  | Assign (name, value) ->
      print_symbol name; printf " := "; print value
  | Block (vars, tree) ->
      printf "@[<v>@[<v 2>begin@,"; print_block vars tree; printf "@]@,end@]"
  | UnaryOp (op, x) ->
      printf "@[<hov>"; print_string (translate_op op); printf "("; print x;
      printf ")@]"
  | BinaryOp (op, x, y) ->
      printf "@[<hov>("; print x; printf "@ %s@ " (translate_op op); print y;
      printf ")@]"
  | Call (name, args) ->
      print_symbol name; print_char '(';
      iter_join print (fun () -> printf ", ") args;
      print_char ')'
  | Char c -> 
      printf "'%c'" c
  | Cons (a, Cons _) as lst ->
      printf "@[<1>["; print_cons lst; printf "]@]"
  | Cons (a, b) ->
      print_char '{'; print a; printf "; "; print b; print_char '}'
  | Float n ->
      print_float n
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
      begin match value with
      | Some tree -> printf " := "; print tree
      | None -> ()
      end
  | Lambda (args, body) ->
      printf "@[<v>fn (@[<hov>"; print_symbols args; printf "@]) -> @,";
      print_fun_body body; printf "@] "
  | TypedLambda (targs, rtype, body) ->
      printf "@[<v>fn (@[<hov>"; print_typed_symbols targs; printf "@]) : ";
      print_spadtype rtype; printf " -> @,";
      print_fun_body body; printf "@] "
  | Return tree ->
      printf "return "; print tree
  | String str ->
      printf "\"%s\"" (String.escaped str)
  | Symbol "NIL" -> print_string "false"
  | Symbol "T" -> print_string "true"
  | Symbol name ->
      print_char '\''; print_symbol name
  | Value name ->
      print_symbol name
  | While (pred, body) ->
      printf "@[<v>@[<v 2>while@,"; print_inline pred;
      printf "@]@,@[<v 2>do@,"; print_inline body; printf "@]@,endwhile@]"

and print_inline = function
  | Block (vs, exps) -> print_block vs exps
  | x -> print x

and print_cons = function
  | Cons (a, (Cons (_, _) as rest)) ->
      print a; printf ";@ "; print_cons rest
  | Cons (a, Symbol "nil") ->
      print a
  | _ -> failwith "Not a list."

and print_fun_body = function
  | Block (_, _) as body ->
      print body
  | _ as body ->
      printf "@[<v 2>begin@,"; print body; printf "@]@,end"

and print_block vars exps =
  if vars != [] then
    (printf "var @["; print_typed_symbols vars; printf "@]@,");
  iter_join (fun t -> print t) (fun () -> printf "@,") exps

and print_spadtype = function
  | SI -> print_string "SingleInteger"
  | DF -> print_string "DoubleFloat"
  | Boolean -> print_string "Boolean"
  | Cons -> print_string "List"
  | Generic -> print_string "?"

and print_symbol name = 
  let name = literal_symbol name in
  if is_simple_symbol name
  then print_string name
  else printf "|%s|" name

and print_typed_symbol (name, atype) =
  print_symbol name;
  printf " : ";
  print_spadtype atype

and print_symbols lst =
  iter_join print_symbol (fun () -> printf ",@ ") lst

and print_typed_symbols lst =
  iter_join print_typed_symbol (fun () -> printf ",@ ") lst
