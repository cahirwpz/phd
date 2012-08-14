{
  open Printf
  open Parser
}

let digit = ['0' - '9']
let space = [' ' '\t' '\n']
let id = ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*
let arith = ("+" | "-" | "*" | "/")
let cmp = ("=" | "<" | "<=" | ">" | ">=")

rule token = parse
  | space+ { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | digit+ as inum { NUM (float_of_string inum) }
  | (cmp | arith | id) as symbol { SYMBOL symbol }
  | _ as c
  	{
      printf "Unrecognized character: %c\n" c;
	    token lexbuf
    }
  | eof	{ EOF }
