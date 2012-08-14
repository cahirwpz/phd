%token <float> NUM
%token <string> SYMBOL
%token LPAREN RPAREN
%token EOF

%start program
%type <Ast.sexpr list> program
%%

program:
  sexpr_list EOF { $1 }
;;

sexpr:
    NUM { Ast.Number $1 }
  | SYMBOL { Ast.Symbol $1 }
  | LPAREN sexpr_list RPAREN { Ast.SExpr $2 }
;;

sexpr_list:
  sexpr sexpr_list { $1 :: $2 }
| { [] }
;;
