open Format;;

exception SexprListError of Sexpr.sexpr list;;
exception SexprError of Sexpr.sexpr;;

let binOps = ["+"; "-"; "*"; "/"; "<"; ">"; "or"; "and"; "equal"]
let specForms = [
  "defun"; "prog"; "progn"; "cond"; "setq"; "return"; "char"; "elt"; "lambda"]

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
  | Case of tree * tree
  | Char of char
  | DefVar of string list * tree
  | Function of string * string list * tree
  | Lambda of string list * tree
  | Cons of tree list
  | Number of float
  | Seq of tree * tree
  | String of string
  | Symbol of string

(* conversion from S-Expressions *)

let rec convert_symbol_list = function
  | (Sexpr.Symbol symbol)::symbols -> symbol::(convert_symbol_list symbols)
  | [] -> []
  | _ as e -> raise (SexprListError e)

let rec convert = function
  | Sexpr.String s -> String s
  | Sexpr.Symbol s -> Symbol s
  | Sexpr.Group g -> convert_group g
  | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
  | Sexpr.Quote (Sexpr.Group g) -> Cons (List.map convert g)
  | Sexpr.Number n -> Number n
  | _ as e -> raise (SexprError e)

and convert_list lst =
  match lst with head::tail ->
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

and convert_bin_op op lst =
  match lst with head::tail ->
    let reduce = fun a b -> BinOp (op, a, convert b)
    in List.fold_left reduce (convert head) tail
  | _ -> failwith ""

and convert_spec_form = function
  | "defun" -> convert_fun_decl
  | "prog" -> convert_prog
  | "progn" -> convert_progn
  | "cond" -> convert_cond
  | "setq" -> convert_setq
  | "return" -> convert_return
  | "char" -> convert_char
  | "elt" -> convert_elt
  | "lambda" -> convert_lambda
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

and convert_prog = function
  | (Sexpr.Group symbols)::body ->
      DefVar (convert_symbol_list symbols, convert_list body)
  | _ as e -> raise (SexprListError e)

and convert_setq = function
  | (Sexpr.Symbol name)::value::[] ->
      Assignment (name, convert value)
  | _ as e -> raise (SexprListError e)

and convert_progn body = convert_list body

and convert_cond clauses =
  match clauses with head::tail ->
    let reduce = fun a b -> Seq (a, convert_clause b)
    in Block (List.fold_left reduce (convert_clause head) tail)
  | _ -> failwith ""

and convert_clause = function
  | Sexpr.Group (head::tail) ->
      Case (convert head, convert_list tail)
  | _ as e -> raise (SexprError e)

(* stringification *)
let rec print = function
  | Apply (func, args) ->
      print func; print_char '('; print_list ", " args; print_char ')'
  | Block tree ->
      printf "@[<v 2>{@,"; print tree; printf "@]@,}"
  | Case (Symbol "t", body) ->
      print_string "_ => "; print body
  | Case (cond, body) ->
      print cond; print_string " => "; print body
  | DefVar (names, tree) ->
      printf "var %s;@," (string_of_symbol_list names);
      print tree
  | Function (name, args, body) ->
      let name = (string_of_symbol name)
      and args = (string_of_symbol_list args)
      in printf "@[<v 1>def %s(%s) {@," name args;
      print body;
      printf "@]@.}@."
  | Symbol name ->
      print_string (string_of_symbol name)
  | String str ->
      printf "\"%s\"" (String.escaped str)
  | Seq (left, right) ->
      print left; print_cut (); print right
  | Number n ->
      print_float n
  | Assignment (name, value) ->
      printf "%s := " (string_of_symbol name); print value; print_char ';'
  | BinOp (op, lhs, rhs) ->
      print_char '('; print lhs; printf " %s " op; print rhs; print_char ')'
  | Char c -> 
      printf "'%c'" c
  | Cons l ->
      print_char '['; print_list ", " l; print_char ']'
  | ArrayRef (name, index) ->
      printf "%s." (string_of_symbol name);
      print_char '['; print index; print_char ']'
  | Lambda (args, body) ->
      let s_args = String.concat ", " (List.map string_of_symbol args) in
      printf "(%s) +-> " s_args;
      print body

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
