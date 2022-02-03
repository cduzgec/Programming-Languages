%{
#include <stdio.h>
void yyerror(const char *s)
{
 return;
}
%}
%token tINTTYPE tINTVECTORTYPE tINTMATRIXTYPE tREALTYPE tREALVECTORTYPE tREALMATRIXTYPE 
%token tIF tENDIF tTRANSPOSE
%token tEQ tLT tGT tDOTPROD tNE tLTE tGTE tOR tAND
%token tIDENT tINTNUM tREALNUM
%token '+' '-' '*' '/' '=' '[' ']' ',' ';'
%left EQ tLT tGT tNE tLTE tGTE
%left '+' '-'
%left '*' '/' tDOTPROD
%left tAND tOR
%%
prog: stmtlst
	   ;
stmtlst : stmtlst stmt
        | stmt
			  ;
stmt : decl | if | asgn 
            ;
decl : type vars '=' expr ';'
      ;
asgn : tIDENT '=' expr ';' 
      ;
if : tIF '(' bool ')' stmt tENDIF 
    ;     
transpose : tTRANSPOSE '(' expr ')'
          ;
vectorLit : '[' row ']'
          ;
matrixLit : '[' rows ']'

rows : rows ';' row | row ';' row

row : row ',' value | value
    ;    
bool : comp | value tAND value | value tOR value
      ;
comp : value relation value
      ;
relation : tEQ | tLT | tGT | tNE | tLTE | tGTE      
          ;     
type : tINTTYPE | tINTVECTORTYPE | tINTMATRIXTYPE 
      | tREALTYPE | tREALVECTORTYPE | tREALMATRIXTYPE    
      ;
expr : | value | value '+' value | value '-' value | value '/' value | value '*' value | value tDOTPROD value  
       |transpose | vectorLit | matrixLit 
     ;       
vars : vars ',' tIDENT | tIDENT
      ;
value : tIDENT | tINTNUM | tREALNUM
      ;
%%
int main() 
{
  if(yyparse())
  {
    printf("ERROR\n");
    return 1;
  }
  else
  {
    printf("OK\n");
    return 0;
  }
}