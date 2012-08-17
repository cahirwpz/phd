open Printf

type sexpr =
  | Number of float
  | Symbol of string 
  | Group of sexpr list 
  | Quote of sexpr
  | String of string
  | TreeDecl of int * sexpr
  | TreeRef of int
  | Label of string

let rec stringify expr =
  match expr with
    | Number n -> string_of_float n
    | Symbol s -> "|" ^ s ^ "|"
    | Group el ->
        let sl = List.map stringify el in
        let s = (String.concat " " sl) in
        sprintf "(%s)" s
    | Quote e -> "'" ^ (stringify e)
    | String s -> sprintf "\"%s\"" s
    | Label s -> "#:" ^ s
    | TreeDecl (n, e) -> sprintf "#%d=%s" n (stringify e)
    | TreeRef n -> sprintf "#%d#" n

let print expr_list =
  let sl = List.map stringify expr_list in
  let s = String.concat "\n" sl in
  print_string (s ^ "\n")
