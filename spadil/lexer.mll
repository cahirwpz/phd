{
  open Printf
  open Parser
}

let digit = ['0' - '9']
let space = [' ' '\t' '\n']
let id = ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*
let arith = ("+" | "-" | "*" | "/")
let cmp = ("=" | "<" | "<=" | ">" | ">=")

let int_re = digit+
let float_re = digit+ '.' digit*

rule token = parse
  | space+ { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '\'' { QUOTE }
  | '"' { parseString "" lexbuf }
  | float_re as num { NUM (float_of_string num) }
  | int_re as num { NUM (float_of_string num) }
  | (cmp | arith | id) as symbol { SYMBOL symbol }
  | _ as c
  	{
      printf "Unrecognized character: %c\n" c;
	    token lexbuf
    }
  | eof	{ EOF }
and parseString acc = parse
  | ("\\" _) as c {
      let s = Scanf.unescaped c in
      parseString (acc ^ s) lexbuf
    }
  | '"' { STRING acc }
  | _ as c {
      let s = String.make 1 c in
      parseString (acc ^ s) lexbuf
    }
