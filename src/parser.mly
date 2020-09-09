%{
  open Ast
%}

%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMICOLON
%token ASSIGN
%token PLUS MINUS
%token TRUE FALSE
%token AND OR
%token NOT
%token LESSEQ
%token IF ELSE
%token ASSERTEQ
%token RETURN
%token TEST

%token EOF

%token <int> NUMBER
%token <string> IDENT

%left SEMICOLON
%nonassoc NOT
%left AND
%left OR
%left PLUS MINUS

%start <Ast.prog> prog

%%

prog:
| defs = list (fundef) t = test EOF { (defs, t) }

fundef:
| id = IDENT c = param_code
    { (id, c) }

test:
| TEST c = param_code
    { c }

param_code:
| LPAREN args = separated_list (COMMA, IDENT) RPAREN
  LBRACE c = command RBRACE
    { (args, c) }

command:
| id = IDENT ASSIGN e = expr  { Assign (id, e) }
| IF LPAREN b = bexpr RPAREN
  LBRACE c1 = command RBRACE
  ELSE LBRACE c2 = command RBRACE  { IfThenElse (b, c1, c2) }
| ASSERTEQ LPAREN e1 = expr COMMA e2 = expr RPAREN  { AssertEq (e1, e2) }
| c1 = command SEMICOLON c2 = command  { Seq (c1, c2) }
| RETURN e = expr  { Return e }

bexpr:
| TRUE  { True }
| FALSE  { False }
| b1 = bexpr AND b2 = bexpr  { And (b1, b2) }
| b1 = bexpr OR b2 = bexpr  { Or (b1, b2) }
| NOT b = bexpr  { Not b }
| e1 = expr r = rel e2 = expr  { Rel (r, e1, e2) }

expr:
| n = NUMBER  { Number n }
| id = IDENT  { Var id }
| id = IDENT LPAREN args = separated_list (COMMA, expr) RPAREN
  { FunApply (id, args) }
| e1 = expr op = binop e2 = expr  { Binop (op, e1, e2) }

%inline rel:
| LESSEQ  { LessEq }

%inline binop:
| PLUS  { Plus }
| MINUS  { Minus }
