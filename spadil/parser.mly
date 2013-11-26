%token <float> FNUM
%token <int> INUM
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
    FNUM { Sexpr.Float $1 }
  | INUM { Sexpr.Int $1 }
  | STRING { Sexpr.String $1 }
  | QUOTE VECTOR sexpr_list RPAREN { Sexpr.Group ((Sexpr.Symbol "vector") :: $3) }
  | QUOTE sexpr { Sexpr.Quote $2 }
  | SYMBOL { Sexpr.Symbol $1 }
  | LABEL { Sexpr.Symbol ("#:" ^ $1) }
  | FUNCTION { Sexpr.Group [Sexpr.Symbol "FUNCTION"; Sexpr.Symbol $1] }
  | TREE_REF { Sexpr.TreeRef $1 }
  | TREE_DECL sexpr { Sexpr.TreeDecl ($1, $2) }
  | LPAREN sexpr_list RPAREN { Sexpr.Group $2 }
;;

sexpr_list:
    sexpr sexpr_list { $1 :: $2 }
  | {[]}
;;
