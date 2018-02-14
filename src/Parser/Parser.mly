%{
  open Lexing
  open Position
  open Error
  open LangAST

%}

%token LET IN
%token FUN TO
%token EQ LPAREN RPAREN COMMA EOF
%token<int> INT
%token<bool> BOOL	
%token<string> ID FCT

%start<LangAST.t> program

%%

program: e = expression EOF
{
  e
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}

expression: FUN x=located(identifier) TO e=located(expression)
{
  Abs(x, e)
}
| LET x=located(identifier) EQ e1=located(expression) IN e2=located(expression)
{
  Let(x, e1, e2)
}
| e1=located(simple_expression) COMMA e2=located(simple_expression)
{
 Pair(e1, e2)
}
| v = located(simple_expression)
{
  v
}

simple_expression :
| e1=located(simple_expression) e2=located(very_simple_expression)
{
 App (e1, e2)
}
| e = located(very_simple_expression)
{
  e
}
very_simple_expression :
| x=identifier
{
 Var(x)
}
| f=func
{
 Const ( Fct f)
}
| x=INT
{
 Const (Int x)
}
| x = BOOL
{
 Const (Bool x)
}
| LPAREN e=expression RPAREN
{
 e
}

%inline func: f=FCT {
 f
}

%inline identifier: x=ID {
  x
}

%inline located(X): x=X {
  (*Position.with_poss $startpos $endpos*) x
}
