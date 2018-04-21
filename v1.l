%{  
#include "v1.tab.h" 
#include <stdlib.h>  
#define YYSTYPE int
extern YYSTYPE yylval;
%}

DIGIT   [0-9]
NZDIGIT [1-0]
ALPHA   [a-zA-Z_]


%%

"break"					{ return(BREAK); }
"case"					{ return(CASE); }
"char"					{ RET_TYPE(CHAR); }
"const"					{ return(CONST); }
"continue"				{ return(CONTINUE); }
"default"				{ return(DEFAULT); }
"do"					{ return(DO); }
"double"				{ return(DOUBLE); }
"else"					{ return(ELSE); }
"for"					{ return(FOR); }
"if"					{ return(IF); }
"int"					{ RET_TYPE(INT); }
"return"				{ return(RETURN); }
"switch"				{ return(SWITCH); }
"while"					{ return(WHILE); }
"Bool"					{ RET_TYPE(BOOL); }

"+="					{ return ADD_ASSIGN; }
"-="					{ return SUB_ASSIGN; }
"*="					{ return MUL_ASSIGN; }
"/="					{ return DIV_ASSIGN; }
"%="					{ return MOD_ASSIGN; }
"&="					{ return AND_ASSIGN; }
"^="					{ return XOR_ASSIGN; }
"|="					{ return OR_ASSIGN; }
"++"					{ return INC_OP; }
"--"					{ return DEC_OP; }
"&&"					{ return AND_OP; }
"||"					{ return OR_OP; }
"<="					{ return LE_OP; }
">="					{ return GE_OP; }
"=="					{ return EQ_OP; }
"!"					{ return NE_OP; }
";"					{ return ';'; }
("{")				{ return '{'; }
("}")				{ return '}'; }
","					{ return ','; }
":"					{ return ':'; }
"="					{ return '='; }
"("					{ return '('; }
")"					{ return ')'; }
("[")				{ return '['; }
("]")				{ return ']'; }
"&"					{ return '&'; }
"~"					{ return '~'; }
"-"					{ return '-'; }
"+"					{ return '+'; }
"*"					{ return '*'; }
"/"					{ return '/'; }
"<"					{ return '<'; }
">"					{ return '>'; }
"^"					{ return '^'; }

(" "|\t)*             ;
.					{ /* discard bad characters */ }

%%