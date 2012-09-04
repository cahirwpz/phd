open Format
open List
open Utils

type tree =
  | Apply of tree * tree list
  | Call of string * tree list
  | Assign of string * tree
  | Block of VarSet.t * tree list
  | Char of char
  | Cons of tree * tree
  | Float of float
  | IfThen of tree * tree
  | IfThenElse of tree * tree * tree
  | Global of string * tree option
  | Int of int
  | Lambda of string list * tree
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

let unary = ["-"; "NOT"]
let binary = [
  "+"; "-"; "="; "*"; "/"; "REM"; "QUO"; 
  "<"; ">"; ">="; "<=";
  "EQUAL"; "EQ"; "EQL";
  "OR"; "AND"]

let graph = [
  '-'; '.'; ','; '&'; ':'; ';'; '*'; '%'; '}'; '{'; ']'; '['; '!'; '^'; '@';
  '~'; '('; ')'; '='; '<'; '>'; '#']

let is_simple_symbol s =
  let contains = String.contains s in
  not (exists contains graph)

let literal_symbol s =
  let l = (String.length s) - 1 in
  if l >= 3 && (s.[l] = s.[0] && s.[0] = '|')
  then String.sub s 1 (l - 1)
  else s

let translate_op = function
  | "OR" -> "||"
  | "AND" -> "&&"
  | "EQ" | "EQL" | "EQUAL" -> "="
  | "NOT" -> "!"
  | "REM" -> "%"
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
          in fold_right reduce g (Symbol "nil")
      | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
      | Sexpr.String s -> String s
      | Sexpr.Symbol s -> Value s
      | e -> raise (SyntaxError ("Unknown s-expression", e))
    end
  with SyntaxError (s, exp) ->
    printf "@[<v 2>*SYNTAX ERROR* %s:@," s; Sexpr.print exp; printf "@]@.";
    Symbol "*invalid*"

and convert_group = function
  | (Sexpr.Symbol op)::body when mem op binary && length body > 1 ->
      convert_bin_op op body
  | (Sexpr.Symbol name)::body -> (
      try
        convert_spec_form name body
      with UnknownForm _ ->
        Call (name, map convert body))
  | (Sexpr.Group func)::body ->
      Apply (convert_group func, map convert body)
  | e -> error "Malformed group" e

and convert_bin_op op = function
  | head::tail ->
      let reduce = fun a b -> Call (op, [a; convert b])
      in fold_left reduce (convert head) tail
  | e -> error "BinOp requires at least 2 args" e

and convert_spec_form = function
  | "BLOCK" -> convert_block
  | "char" -> convert_char
  | "DEFPARAMETER"
  | "DEFVAR" -> convert_defvar
  | "DEFUN" -> convert_fun_decl
  | "IF" -> convert_if
  | "LAMBDA" -> convert_lambda
  | "LET" -> convert_let
  | "LOOP" -> convert_loop
  | "PROGN" -> convert_progn
  | "RETURN" -> convert_return
  | "SETQ" -> convert_setq
  | "WHILE" -> convert_while
  | e -> raise (UnknownForm e)

and convert_block = function
  | (Sexpr.Symbol label)::rest ->
      Block (VarSet.empty, map convert rest)
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

and convert_let = function
  | (Sexpr.Group symbols)::body ->
      let symbols = convert_symbol_list symbols in
      Block (VarSet.from_list symbols, map convert body)
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
  Block (VarSet.empty, map convert body)

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
  | Call (op, [x]) when mem op unary ->
      printf "@[<hov>"; print_string (translate_op op); print x; printf "@]"
  | Call (op, [x; y]) when mem op binary ->
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
      printf "@[<v 2>if*@,"; print_inline pred; printf "@]@,";
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
  | Return tree ->
      printf "return "; print tree
  | String str ->
      printf "\"%s\"" (String.escaped str)
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
  if not (VarSet.is_empty vars) then
    (printf "var @["; print_symbols (VarSet.elements vars); printf "@]@,");
  iter_join (fun t -> print t) (fun () -> printf "@,") exps

and print_symbol name = 
  let name = literal_symbol name in
  if is_simple_symbol name
  then print_string name
  else printf "|%s|" name

and print_symbols lst =
  iter_join (fun s -> print_symbol s) (fun () -> printf ",@ ") lst
