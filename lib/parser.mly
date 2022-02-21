%{
  open Ast
%}

%token <Ast.var> SYMBOL
%token <int> INT
%token <string> STRING
%token TRUE
%token FALSE
%token DOT
%token QUOTE
%token DEFINE
%token QUIT
%token USE
%token LPAREN
%token RPAREN
%token EOF

%start toplevel expr
%type <Ast.command list> toplevel
%type <Ast.expr> expr

%%

toplevel:
  | EOF { [] }
  | e = command ; e1 = toplevel { e :: e1 } 

command:
  | LPAREN ; DEFINE ; e = SYMBOL ; e1 = expr ; RPAREN { Define (e, e1) }
  | QUIT { Quit }
  | USE ; e = STRING { Use e } 
  | e = expr { Expr e }

expr:
  | e = SYMBOL { Symbol e }
  | e = INT { Int e }
  | TRUE { True }
  | FALSE { False }
  | LPAREN ; RPAREN { Null }
  | LPAREN ; e = list ; RPAREN { e }
  | QUOTE ; e = expr { Pair (Symbol "quote", Pair (e, Null)) }

list:
  | e = expr ; DOT ; e1 = expr { Pair (e, e1) }
  | e = expr { Pair (e, Null) }
  | e = expr ; e1 = list { Pair (e, e1) }

%%