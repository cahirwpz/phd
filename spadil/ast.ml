open Printf

exception SexprListError of Sexpr.sexpr list;;
exception SexprError of Sexpr.sexpr;;

let binOps = ["+"; "-"; "*"; "/"; "<"; ">"; "or"; "equal"]
let specForms = [
  "defun"; "prog"; "progn"; "cond"; "setq"; "return"; "char"; "elt"]

let graph = [
  '-'; '.'; '&'; ':'; '*'; '%'; '}'; '{'; ']'; '['; '!'; '^'; '@'; '~'; '(';
  ')']

let isSimpleSymbol s =
  let contains = String.contains s in
  not (List.exists contains graph)

let translate_op op =
  match op with
  | "or" -> "||"
  | "equal" -> "=="
  | _ as x -> x

type tree =
  | Assignment of string * tree
  | BinOp of string * tree * tree
  | Block of tree
  | Case of tree * tree
  | DefVar of string * tree
  | FunCall of string * tree list
  | Function of string * tree list * tree
  | Number of float
  | Seq of tree * tree
  | Char of char
  | String of string
  | Symbol of string
  | List of tree list
  | ArrayRef of string * tree

(* conversion from S-Expressions *)

let rec convert_symbol_list = function
  | (Sexpr.Symbol s)::symbols -> (Symbol s)::(convert_symbol_list symbols)
  | [] -> []
  | _ as e -> raise (SexprListError e)

let rec convert = function
  | Sexpr.String s -> String s
  | Sexpr.Symbol s -> Symbol s
  | Sexpr.Group g -> convert_group g
  | Sexpr.Quote (Sexpr.Symbol s) -> Symbol s
  | Sexpr.Quote (Sexpr.Group g) -> List (List.map convert g)
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
  | (Sexpr.Symbol funName)::body ->
      FunCall (funName, List.map convert body)
  | _ as e -> failwith ("Group: " ^ (Sexpr.stringify (Sexpr.Group e)))

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
  | _ as e -> failwith ("Unknown special form: " ^ e)

and convert_return tree = convert_list tree

and convert_char = function
  | (Sexpr.Quote (Sexpr.Symbol c))::[] -> Char c.[0]
  | _ as e -> raise (SexprListError e)

and convert_elt = function
  | (Sexpr.Symbol name)::index::[] -> ArrayRef (name, convert index)
  | _ as e -> raise (SexprListError e)

and convert_fun_decl = function
  | (Sexpr.Symbol name)::(Sexpr.Group args)::body::[] ->
      Function (name, convert_symbol_list args, convert body)
  | _ as e -> raise (SexprListError e)

and convert_prog = function
  | (Sexpr.Group (Sexpr.Symbol name::symbols))::body ->
      DefVar (name, convert_prog (Sexpr.Group symbols::body))
  | (Sexpr.Group [])::body ->
      convert_list body
  | _ as e -> raise (SexprListError e)

and convert_setq = function
  | (Sexpr.Symbol name)::value::[] ->
      Assignment (name, convert value)
  | _ as e -> raise (SexprListError e)

and convert_progn body = convert_list body

and convert_cond clauses =
  let reduce = fun a b -> Seq (a, convert_clause b)
  and head = List.hd clauses
  and tail = List.tl clauses
  in Block (List.fold_left reduce (convert_clause head) tail)

and convert_clause = function
  | Sexpr.Group g ->
      let head = List.hd g
      and tail = List.tl g
      in Case (convert head, convert_list tail)
  | _ as e -> raise (SexprError e)

(* stringification *)
let rec stringify tree =
  match tree with
  | Function (name, args, body) ->
      let s_args = stringify_list ", " args
      and s_body = stringify body in
      sprintf "def %s(%s) {%s}" name s_args s_body;
  | FunCall (name, args) ->
      let s_args = stringify_list ", " args in
      sprintf "%s(%s)" name s_args
  | Symbol name ->
      stringify_symbol name
  | String str ->
      sprintf "\"%s\"" (String.escaped str)
  | Seq (left, right) ->
      sprintf "%s; %s" (stringify left) (stringify right)
  | Case (Symbol "t", body) ->
      sprintf "_ => %s" (stringify body)
  | Case (cond, body) ->
      sprintf "%s => %s" (stringify cond) (stringify body)
  | Block tree ->
      sprintf "{%s}" (stringify tree)
  | DefVar (name, tree) ->
      sprintf "var %s;%s" (stringify_symbol name) (stringify tree)
  | Number n ->
      string_of_float n
  | Assignment (name, value) ->
      sprintf "%s := %s" (stringify_symbol name) (stringify value)
  | BinOp (op, lhs, rhs) ->
      sprintf "(%s %s %s)" (stringify lhs) op (stringify rhs)
  | Char c -> 
      sprintf "'%c'" c
  | List l ->
      sprintf "[%s]" (stringify_list ", " l)
  | ArrayRef (name, index) ->
      sprintf "%s.[%s]" (stringify_symbol name) (stringify index)

and stringify_list sep trees = 
  let s_trees = List.map stringify trees in
  String.concat sep s_trees

and stringify_symbol name = 
  if isSimpleSymbol name then name else sprintf "|%s|" name

let as_string expr =
  try
    let tree = convert expr in
    stringify tree
  with
  | SexprListError exprs ->
      sprintf "Conversion error: %s\n" (Sexpr.stringify (Sexpr.Group exprs))
  | SexprError expr ->
      sprintf "Conversion error: %s\n" (Sexpr.stringify expr)
