%token <float> NUM
%token <string> SYMBOL
%token <string> STRING
%token <string> FUNCTION
%token <string> LABEL
%token <int> TREE_DECL
%token <int> TREE_REF
%token QUOTE
%token VECTOR
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
  | LABEL { Sexpr.Symbol ("#:" ^ $1) }
  | FUNCTION { Sexpr.Group [Sexpr.Symbol "function"; Sexpr.Symbol $1] }
  | TREE_REF { Sexpr.TreeRef $1 }
  | TREE_DECL sexpr { Sexpr.TreeDecl ($1, $2) }
  | VECTOR sexpr_list RPAREN { Sexpr.Group ((Sexpr.Symbol "vector") :: $2) }
  | LPAREN sexpr_list RPAREN { Sexpr.Group $2 }
;;

sexpr_list:
    sexpr sexpr_list { $1 :: $2 }
  | {[]}
;;
