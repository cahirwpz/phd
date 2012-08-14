open Printf

type sexpr =
  | Number of float
  | Symbol of string 
  | SExpr of sexpr list 

let rec stringify expr =
  match expr with
    | Number n -> string_of_float n
    | Symbol s -> sprintf "|%s|" s
    | SExpr el ->
        let sl = List.map stringify el in
        sprintf "(%s)" (String.concat " " sl)

let print expr_list =
  let sl = List.map stringify expr_list in
  printf "%s\n" (String.concat "\n" sl)
