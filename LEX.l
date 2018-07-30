

%{
	
	
#include "v1.tab.h" 
#include <stdio.h>
#include <stdlib.h>  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
using namespace std;
#include <string>
#include <iostream>

int yylex();
char *p;
%}
%option yylineno
DIGIT   [0-9]
NZDIGIT [1-9]
ALPHA   [a-zA-Z_]
%%

	/*data types */
"char"									{ return CHAR; }
"int"									{ return INT; }
"double"			                	{return DOUBLE; }
"bool"				                 	{  return BOOL; }
"string"                                {return STRING; }

	/* assignment */
"="					                    { return '='; }
	/* values */
'{ALPHA}'                               {yylval.CHAR=yytext[1]; return CHARVAL;}
{DIGIT}{DIGIT}*                         {  yylval.INTEGER = (int)atof(yytext); return INTVAL; }
({DIGIT}{DIGIT}*)"."({DIGIT})+       	{ yylval.DOUBLE = atof(yytext);   ;return DOUBLEVAL; }
\"{ALPHA}+\"                      { p=(char *)calloc(strlen(yytext)+1,sizeof(char));strcpy(p,yytext); yylval.STRING = p;
												return STRINGVAL;}
true                                   {  return TRUE1; }
false                                   { return FALSE1; }

	/* reserved words */
break 							    	{return (BREAK); }
case 									{ return (CASE); }
continue 								{ return(CONTINUE); }
default 								{ return(DEFAULT); }
do 										{ return(DO); }
else									{ return(ELSE); }
for										{ return FOR; }
if										{ return(IF); }
return									{ return(RETURN); }
switch									{ return(SWITCH); }
while									{ return(WHILE); }
cout									{ return COUT;    }
const									{ return CONST;    }
"<<"									{ return COUTOP;   }
\"({ALPHA}|{DIGIT}|[ ])*\" {  return COUTSTR; }
endl                                    {return ENDL;}
cin										{ return CIN;}
">>"									{return CINOP;}

"&&"									{ return AND_OP; }
"||"									{ return OR_OP; }
"!"										{ return NE_OP; }

"<="									{ return LE_OP; }
">="									{ return GE_OP; }
"=="									{ return EQ_OP; }
"<"										{ return '<'; }
">"										{ return '>'; }
"!="                                    { return NG_EQ;}


";"										{ return ';'; }
("{")									{ return '{'; }
("}")									{ return '}'; }
","										{ return ','; }
":"										{ return ':'; }
"("										{ return '('; }
")"										{ return ')'; }
("[")									{ return '['; }
("]")									{ return ']'; }
"&"										{ return '&'; }
"~"										{ return '~'; }
"-"										{ return '-'; }
"+"										{ return '+'; }
"*"										{ return '*'; }
"/"										{ return '/'; }
[\n]        		  {yylineno++;}
"printSymbol"							{return PrintSymbol;}

"^"										{ return '^'; }

{ALPHA}({ALPHA}|{DIGIT})*	            {p=(char *)calloc(strlen(yytext)+1,sizeof(char));strcpy(p,yytext); yylval.STRING = p;
												
					
															  return IDENTIFIER;}
(" "|\t)*          					    { ;}
.										{ /* discard bad characters */ }

%%

int yywrap() {}