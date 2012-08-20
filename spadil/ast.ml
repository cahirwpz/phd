open Format;;

exception SexprListError of Sexpr.sexpr list;;
exception SexprError of Sexpr.sexpr;;

let binOps = ["+"; "-"; "*"; "/"; "<"; ">"; "or"; "and"; "equal"]
let specForms = [
  "defun"; "let"; "progn"; "if"; "setq"; "return"; "char"; "elt"; "lambda"]

let graph = [
  '-'; '.'; ','; '&'; ':'; '*'; '%'; '}'; '{'; ']'; '['; '!'; '^'; '@'; '~'; '(';
  ')']

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
  | Block of tree
  | Char of char
  | Cons of tree list
  | DefVar of string list * tree
  | Float of float
  | Function of string * string list * tree
  | IfThen of tree * tree
  | IfThenElse of tree * tree * tree
  | Int of int
  | Lambda of string list * tree
  | Seq of tree * tree
  | String of string
  | Symbol of string

(* conversion from S-Expressions *)

let rec convert_symbol_list = function
  | (Sexpr.Symbol symbol)::symbols -> symbol::(convert_symbol_list symbols)
  | [] -> []
  | _ as e -> raise (SexprListError e)

let rec convert = function
  | Sexpr.Float n -> Float n
  | Sexpr.Group g -> convert_group g
  | Sexpr.Int n -> Int n
  | Sexpr.Quote (Sexpr.Group g) -> Cons (List.map convert g)
  | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
  | Sexpr.String s -> String s
  | Sexpr.Symbol s -> Symbol s
  | _ as e -> raise (SexprError e)

and convert_list = function
  | head::tail ->
      let reduce = fun a b -> Seq (a, convert b)
      in List.fold_left reduce (convert head) tail
  | _ -> failwith ""

and convert_group = function
  | (Sexpr.Symbol op)::body when List.mem op binOps && List.length body > 1 ->
      convert_bin_op (translate_op op) body
  | (Sexpr.Symbol form)::body when List.mem form specForms ->
      convert_spec_form form body
  | (Sexpr.Symbol funcName)::body ->
      Apply (Symbol funcName, List.map convert body)
  | (Sexpr.Group func)::body ->
      Apply (convert_group func, List.map convert body)
  | _ as e -> Sexpr.print (Sexpr.Group e); failwith "Malformed group."

and convert_bin_op op = function
  | head::tail ->
      let reduce = fun a b -> BinOp (op, a, convert b)
      in List.fold_left reduce (convert head) tail
  | _ -> failwith ""

and convert_spec_form = function
  | "char" -> convert_char
  | "defun" -> convert_fun_decl
  | "elt" -> convert_elt
  | "if" -> convert_if
  | "lambda" -> convert_lambda
  | "let" -> convert_let
  | "progn" -> convert_progn
  | "return" -> convert_return
  | "setq" -> convert_setq
  | _ as e -> failwith ("Unknown special form: " ^ e)

and convert_return tree = convert_list tree

and convert_char = function
  | (Sexpr.Quote (Sexpr.Symbol c))::[] -> Char c.[0]
  | _ as e -> raise (SexprListError e)

and convert_elt = function
  | (Sexpr.Symbol name)::index::[] -> ArrayRef (name, convert index)
  | _ as e -> raise (SexprListError e)

and convert_lambda = function
  | (Sexpr.Group args)::body ->
      Lambda (convert_symbol_list args, convert_list body)
  | _ as e -> raise (SexprListError e)

and convert_fun_decl = function
  | (Sexpr.Symbol name)::(Sexpr.Group args)::body::[] ->
      Function (name, convert_symbol_list args, convert body)
  | _ as e -> raise (SexprListError e)

and convert_let = function
  | (Sexpr.Group symbols)::body ->
      DefVar (convert_symbol_list symbols, convert_list body)
  | _ as e -> raise (SexprListError e)

and convert_setq = function
  | (Sexpr.Symbol name)::value::[] ->
      Assignment (name, convert value)
  | _ as e -> raise (SexprListError e)

and convert_progn body = convert_list body

and convert_if = function
  | pred::if_true::if_false::[] ->
      IfThenElse (convert pred, convert if_true, convert if_false)
  | pred::if_true::[] ->
      IfThen (convert pred, convert if_true)
  | _ as e -> raise (SexprListError e)

(* stringification *)
let rec print = function
  | Apply (func, args) ->
      print func; print_char '('; print_list ", " args; print_char ')'
  | ArrayRef (name, index) ->
      printf "%s." (string_of_symbol name);
      print_char '['; print index; print_char ']'
  | Assignment (name, value) ->
      printf "%s := " (string_of_symbol name); print value; print_char ';'
  | BinOp (op, lhs, rhs) ->
      print_char '('; print lhs; printf " %s " op; print rhs; print_char ')'
  | Block tree ->
      printf "@[<v>{@,"; print tree; printf "@]@,}";
  | Char c -> 
      printf "'%c'" c
  | Cons l ->
      print_char '['; print_list "; " l; print_char ']'
  | DefVar (names, tree) ->
      printf "var %s;@," (string_of_symbol_list names);
      print tree
  | Float n ->
      print_float n
  | Function (name, args, body) ->
      let name = (string_of_symbol name)
      and args = (string_of_symbol_list args)
      in printf "@[<v 1>def %s(%s) {@," name args;
      print body;
      printf "@]@.}@."
  | IfThen (pred, if_true) ->
      printf "@[<v>";
      printf "@[<v 2>if@,"; print pred; printf "@]@,";
      printf "@[<v 2>then@,"; print if_true; printf "@]@]"
  | IfThenElse (pred, if_true, if_false) ->
      printf "@[<v>";
      printf "@[<v 2>if@,"; print pred; printf "@]@,";
      printf "@[<v 2>then@,"; print if_true; printf "@]@,";
      printf "@[<v 2>else@,"; print if_false; printf "@]@]"
  | Int n ->
      print_int n
  | Lambda (args, body) ->
      let s_args = String.concat ", " (List.map string_of_symbol args) in
      printf "(%s) +-> " s_args;
      print body
  | Seq (left, right) ->
      print left; print_cut (); print right
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

let print_safe expr =
  try
    let tree = convert expr in
    print tree; print_char '\n'
  with
  | SexprListError exprs ->
      print_string "Conversion error:\n";
      Sexpr.print (Sexpr.Group exprs)
  | SexprError expr ->
      print_string "Conversion error:\n";
      Sexpr.print expr
