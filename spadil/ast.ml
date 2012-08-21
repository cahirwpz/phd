open Format;;

exception SyntaxError of string * Sexpr.sexpr;;
exception UnknownForm of string;;

let error = fun s exp -> raise (SyntaxError (s, Sexpr.Group exp))

let binOps = ["+"; "-"; "*"; "/"; "<"; ">"; "or"; "and"; "equal"]

let graph = [
  '-'; '.'; ','; '&'; ':'; ';'; '*'; '%'; '}'; '{'; ']'; '['; '!'; '^'; '@';
  '~'; '('; ')']

let is_simple_symbol s =
  let contains = String.contains s in
  not (List.exists contains graph)

let string_of_symbol name = 
  if is_simple_symbol name
  then name
  else sprintf "|%s|" name

let string_of_symbol_list symbols =
  String.concat ", " (List.map string_of_symbol symbols)

let translate_op op =
  match op with
  | "or" -> "||"
  | "and" -> "&&"
  | "equal" -> "=="
  | _ as x -> x

type tree =
  | Apply of tree * tree list
  | ArrayRef of string * tree
  | Assignment of string * tree
  | BinOp of string * tree * tree
  | Block of tree list
  | Char of char
  | Cons of tree * tree
  | DefVar of string list * tree
  | Float of float
  | Function of string * string list * tree
  | IfThen of tree * tree
  | IfThenElse of tree * tree * tree
  | Int of int
  | Label of string * tree
  | Lambda of string list * tree
  | Loop of string * tree
  | String of string
  | Symbol of string

(* conversion from S-Expressions *)

let rec convert_symbol_list = function
  | (Sexpr.Symbol symbol)::symbols -> symbol::(convert_symbol_list symbols)
  | [] -> []
  | e -> error "Expected a list of symbols" e

let rec convert = function
  | Sexpr.Float n -> Float n
  | Sexpr.Group g -> convert_group g
  | Sexpr.Int n -> Int n
  | Sexpr.Quote (Sexpr.Group g) ->
      let reduce = fun a b -> Cons (convert a, b) 
      in List.fold_right reduce g (Symbol "nil")
  | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
  | Sexpr.String s -> String s
  | Sexpr.Symbol s -> Symbol s
  | e -> raise (SyntaxError ("Unknown s-expression", e))

and convert_group = function
  | (Sexpr.Symbol op)::body when List.mem op binOps && List.length body > 1 ->
      convert_bin_op (translate_op op) body
  | (Sexpr.Symbol name)::body -> (
      try
        convert_spec_form name body
      with UnknownForm _ ->
        Apply (Symbol name , List.map convert body))
  | (Sexpr.Group func)::body ->
      Apply (convert_group func, List.map convert body)
  | e -> error "Malformed group" e

and convert_bin_op op = function
  | head::tail ->
      let reduce = fun a b -> BinOp (op, a, convert b)
      in List.fold_left reduce (convert head) tail
  | e -> error "BinOp requires at least 2 args" e

and convert_spec_form = function
  | "block" -> convert_block
  | "char" -> convert_char
  | "defun" -> convert_fun_decl
  | "elt" -> convert_elt
  | "if" -> convert_if
  | "lambda" -> convert_lambda
  | "let" -> convert_let
  | "loop" -> convert_loop
  | "return" | "progn" -> convert_progn
  | "setq" -> convert_setq
  | e -> raise (UnknownForm e)

and convert_block = function
  | (Sexpr.Symbol label)::rest ->
      Label (label, convert_progn rest)
  | e -> error "Malformed block s-form" e

and convert_char = function
  | (Sexpr.Quote (Sexpr.Symbol c))::[] -> Char c.[0]
  | e -> error "Malformed char s-form" e

and convert_elt = function
  | (Sexpr.Symbol name)::index::[] -> ArrayRef (name, convert index)
  | e -> error "Malformed elt s-form" e

and convert_lambda = function
  | (Sexpr.Group args)::body ->
      Lambda (convert_symbol_list args, convert_progn body)
  | e -> error "Malformed lambda s-form" e

and convert_loop = function
  | (Sexpr.Symbol name)::body ->
      Loop (name, convert_progn body)
  | e -> error "Malformed loop s-form" e

and convert_fun_decl = function
  | (Sexpr.Symbol name)::(Sexpr.Group args)::body::[] ->
      Function (name, convert_symbol_list args, convert body)
  | e -> error "Malformed defun s-form" e

and convert_let = function
  | (Sexpr.Group symbols)::body ->
      DefVar (convert_symbol_list symbols, convert_progn body)
  | e -> error "Malformed let s-form" e

and convert_setq = function
  | (Sexpr.Symbol name)::value::[] ->
      Assignment (name, convert value)
  | e -> error "Malformed setq s-form" e

and convert_progn body = Block (List.map convert body)

and convert_if = function
  | pred::if_true::if_false::[] ->
      IfThenElse (convert pred, convert if_true, convert if_false)
  | pred::if_true::[] ->
      IfThen (convert pred, convert if_true)
  | e -> error "Malformed if s-form" e

(* stringification *)
let rec print = function
  | Apply (func, args) ->
      print func; print_char '('; print_list ", " args; print_char ')'
  | ArrayRef (name, index) ->
      printf "%s." (string_of_symbol name);
      print_char '['; print index; print_char ']'
  | Assignment (name, value) ->
      printf "%s := " (string_of_symbol name);
      (match value with
      | DefVar _
      | Block _ -> printf "(@[<v>"; print value; printf "@])"
      | _ -> printf "@[<v>"; print value; printf "@]")
  | BinOp (op, lhs, rhs) ->
      print_char '('; print lhs; printf " %s " op; print rhs; print_char ')'
  | Block tree ->
      printf "@[<v>@[<v 2>begin@,"; print_block tree; printf "@]@,end@]"
  | Char c -> 
      printf "'%c'" c
  | Cons (a, Cons _) as lst ->
      print_char '['; print_cons lst; print_char ']'
  | Cons (a, b) ->
      print_char '{'; print a; printf "; "; print b; print_char '}'
  | DefVar (names, exp) ->
      printf "var %s@," (string_of_symbol_list names);
      (match exp with
      | Block body -> print_block body
      | exp -> print exp)
  | Float n ->
      print_float n
  | Function (name, args, body) ->
      let name = (string_of_symbol name)
      and args = (string_of_symbol_list args)
      in printf "@[<v>def %s(%s)@,@[<v 2>begin@," name args;
      print body;
      printf "@]@,@]end@."
  | IfThen (pred, if_true) ->
      printf "@[<v>";
      printf "@[<v 2>if@,"; print pred; printf "@]@,";
      printf "@[<v 2>then@,"; print if_true; printf "@]@,endif@]"
  | IfThenElse (pred, if_true, if_false) ->
      printf "@[<v>";
      printf "@[<v 2>if@,"; print pred; printf "@]@,";
      printf "@[<v 2>then@,"; print if_true; printf "@]@,";
      printf "@[<v 2>else@,"; print if_false; printf "@]@,endif@]"
  | Int n ->
      print_int n
  | Lambda (args, body) ->
      let s_args = String.concat ", " (List.map string_of_symbol args) in
      printf "(%s) +-> " s_args;
      print body
  | Loop (name, tree) ->
      printf "@[<v>@[<v 2>loop %s@," name; print tree; printf "@]@,endloop@]"
  | Label (name, tree) ->
      printf "@[<v>@[<v 2>label %s@," name; print tree; printf "@]@,endlabel@]"
  | String str ->
      printf "\"%s\"" (String.escaped str)
  | Symbol name ->
      print_string (string_of_symbol name)

and print_list sep trees =
  match trees with
  | tree::[] -> print tree
  | tree::rest ->
      print tree; print_string sep; print_list sep rest
  | [] -> ()

and print_cons = function
  | Cons (a, (Cons (_, _) as rest)) ->
      print a; printf ";@ "; print_cons rest
  | Cons (a, Symbol "nil") ->
      print a
  | _ -> failwith "Not a list."

and print_block trees =
  match trees with
  | [] -> ()
  | tree::[] -> print tree
  | tree::rest -> print tree; printf "@,"; print_block rest

let print_safe expr =
  (try
    print (convert expr)
  with
  | SyntaxError (s, exp) ->
      printf "@[<v 2>*SYNTAX ERROR* %s:@," s; Sexpr.print exp; printf "@]");
  printf "@."
