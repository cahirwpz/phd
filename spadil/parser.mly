%token <float> NUM
%token <string> SYMBOL
%token <string> STRING
%token QUOTE
%token LPAREN RPAREN
%token EOF

%start program
%type <Sexpr.sexpr list> program
%%

program:
  sexpr_list EOF { $1 }
;;

sexpr:
    NUM { Sexpr.Number $1 }
  | STRING { Sexpr.String $1 }
  | QUOTE sexpr { Sexpr.Quote $2 }
  | SYMBOL { Sexpr.Symbol $1 }
  | LPAREN sexpr_list RPAREN { Sexpr.Group $2 }
;;

sexpr_list:
    sexpr sexpr_list { $1 :: $2 }
  | {[]}
;;
