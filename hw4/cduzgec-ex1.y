%{
#define YYSTYPE char *
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
extern FILE *yyin;
int yylex();

void yyerror (const char *s) 
{
	printf ("%s\n", s); 
}

int global_index = 0;
char global_array[200][200];

int local_index = 0;
char local_array[200][200];

bool isItGlobal = true;

%}

%token EOE tINTVAL tINT tSTRING tRETURN tPRINT tLPAR tRPAR tCOMMA 
%token tMOD tASSIGNM tMINUS tPLUS tDIV tSTAR tSEMI tLBRAC tRBRAC tSTRINGVAL tIDENT 


%left tASSIGNM
%left tSEMI
%left tMINUS tPLUS 
%left tDIV tSTAR tMOD
%left tLPAR tRPAR
%start program

%%

program:	stmtlist
		;

stmtlist:	stmtlist stmt 
		| stmt
		;

stmt:       decl
            | func
            | funccall
 	    | assign
	    | print 
            ;

expr:	    value   
            | expr	tDIV expr
            | expr	tSTAR expr
            | expr	tMINUS expr
            | expr	tPLUS expr
	    | expr 	tMOD expr
	    | funccall    
	    ;   

decl:	type variable tASSIGNM expr tSEMI 
	;


funccall : tIDENT tLPAR var tRPAR
	;

assign : var tASSIGNM expr tSEMI
	;

func : type tIDENT tLPAR parameters tRPAR  curlyL body return curlyR
	;

parameters : parameters ',' type tIDENT
	| type tIDENT
	;

var:	expr ',' var
	| expr
	;

variable : tIDENT ',' variable
	| tIDENT {    /* check local, check global -> if exists then exit redefinition else check global if true put global array else local array */  

char ident_array[100];
strcpy(ident_array , $1);  

int i=0;
while (i < local_index+1)
{
	if(strcmp(ident_array, local_array[i]) == 0)  
	{
		extern int noOfLine;
		printf("%d Redefinition of variable \n", noOfLine);
		exit(1);

	}
	i++;
}

int j=0;
while(j < global_index+1)   
{
	if(strcmp(ident_array, global_array[j]) == 0)
	{
		extern int noOfLine;
		printf("%d Redefinition of variable  \n", noOfLine );
		exit(2);
	}
	j++;
}


if(!isItGlobal)
{
	strcpy(local_array[local_index], ident_array);
	local_index++;
}

else
{
	strcpy(global_array[global_index] , ident_array); 
	global_index++;
}

}
;


value : tINTVAL
	| tSTRINGVAL
	| tIDENT { /* check local global array if doesnt exist then exit unndefined */

char ident_array[100];
strcpy(ident_array , $1);
bool flag = false;

int i=0;
while(i < global_index+1)       
{
	if(strcmp(ident_array, global_array[i]) == 0)
	{
		flag = true;
	}
	i++;
}

int j=0;
while(i < local_index+1)
{
	if(strcmp(ident_array, local_array[j]) == 0)
	{
		flag = true;
	}
	j++;
}


if(!flag)
{
extern int noOfLine;
printf("%d Undefined variable  \n" ,noOfLine);
exit(3);
}

}
;

type : tINT
	| tSTRING
	;

curlyL : tLBRAC { isItGlobal = false;}
	;

curlyR :  tRBRAC 
{ 
isItGlobal = true; 
local_index = 0;

int j=0;
while(j < 200)
	{
		int i=0;
   		while (i < 200)
    		{  
        		local_array[j][i] = '\0';
			i++;
    		}
	j++;
	}
}
;
	
body  : decl body
	| assign body
	| print body
	| decl
	| assign
	| print
	; 

return : tRETURN expr tSEMI
	;

print :	tPRINT tLPAR expr tRPAR tSEMI
	;

%%
int main ()
{
   if (yyparse()) {
       printf("ERROR\n");
       return 1;
   }
   else {
      printf("OK\n");
      return 0;
   }
}