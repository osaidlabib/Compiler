%{
#include <iostream>
#include <string>
using namespace std;
#include <map>
void yyerror (const char *s);
int yylex();
extern int yylineno;
extern char *yytext;
#include <stdio.h>
#include<math.h>
#include <stdlib.h>
extern FILE *yyin;
#include <sstream>
#include <queue>
#include <stack>
#include <vector>
vector<int> labelsIf, labelsLoop;
int labelmax=0;
int blocknumber =1;
map <int,queue<string> >BLOCKPRINTS;
bool printnow=true;
string conv(int a);
#define LOGIC_OP 2550

int printStore = 0;
string for1[3];

int switchRegister;
bool switchStore = 0;
string caseValue;

int checkOperation(int type1,char OP,int type2);
struct IDInfo
{
	int type;									//0 int,1 double,2 char,3 bool, 4 string
	int isInit;
	int isConst;
};

map <int,map <string,IDInfo> >symbolTable;
int scope_level=1;
int Re =0;
void print(string s);
IDInfo GetIdentifier(string s);
void newIdentifier(string s,int identfierType,int assignType,int isConst);
void updateIdentifier(string s,int assignType);
int GetIdLevel(string name);
void printEndfor ();
void print_identifrs();

%}

%union {
		int INTEGER;
		double DOUBLE;
		char CHAR;
		bool BOOL;
		char* STRING;
	 struct info1
		{
			int type;									
			int Rnum;
			}info ;

		}


%token CONST 	 '{' '}' '<' '>'
%token  INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token 	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN  ADD_ASSIGN
%token 	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token 	XOR_ASSIGN OR_ASSIGN	 
%token 	BOOL CHAR   INT DOUBLE STRING    
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR  CONTINUE BREAK RETURN 
%token COUT COUTOP  COUTSTR  ENDL CIN CINOP  
%token ')' '('   
%token PrintSymbol



%type <info > Expr expressions MathExpr LogicExpr VALUES type   ConstDecleration  Decleration 
%token <STRING> IDENTIFIER
%token <INTEGER> INTVAL
%token <DOUBLE> DOUBLEVAL
%token <CHAR> CHARVAL
%token <STRING> STRINGVAL
%token <BOOL> BOOLVAL  TRUE1 FALSE1
%right '='

%left '+' '-'
%left '*' '/' 
%left  EQ_OP LE_OP
%left GE_OP NE_OP 
%left AND_OP OR_OP
%left  NG_EQ
%right '!' 

%%

program : Stmt 	
		| program Stmt
		
		
	;

/* Decleration block */	
Decleration : 	 type IDENTIFIER '=' expressions 		{ newIdentifier($2,$1.type,$4.type,0); string temp($2);
																		string s= "MOV "+temp+",R"+conv($4.Rnum);
																		print(s);
															}	
					| type IDENTIFIER 	    								 {newIdentifier($2,$1.type,-1,0);}

ConstDecleration 		: CONST type IDENTIFIER '=' expressions 	    	 { newIdentifier($3,$2.type,$5.type,1); 
																				string temp($3);	
																				string s="MOV "+temp+",R"+conv($5.Rnum);
																				print(s);	
																					}	

Assignment				: IDENTIFIER '=' expressions 		    		     {updateIdentifier($1,$3.type); 
																				string temp($1);
																				string s="MOV "+temp+",R"+conv($3.Rnum);
																				print(s);																																								
																				}
						;

type					: INT		{$$.type=INTVAL;}
						| DOUBLE   {$$.type=DOUBLEVAL;}
						| BOOL     {$$.type=BOOLVAL;}
						| CHAR     {$$.type=CHARVAL;}
						|STRING    {$$.type=STRINGVAL;}
						
						;	


/* MathExpr block */
MathExpr:  
	MathExpr '+' MathExpr			   {  $$.type=checkOperation($1.type,'+',$3.type); $$.Rnum=Re;							
										  string s="ADD R"+conv(Re++)+",R"+conv($1.Rnum)+", R"+conv($3.Rnum);
										  print(s);  
										        }   
	| MathExpr '-' MathExpr  		   {   $$.type=checkOperation($1.type,'-',$3.type); $$.Rnum=Re;
											string s = "SUB R"+ conv(Re++)+",R"+conv($1.Rnum)+", R"+conv($3.Rnum); 
											print(s);
											}  
	| MathExpr '*' MathExpr  		    {  $$.type=checkOperation($1.type,'*',$3.type); $$.Rnum=Re;
											string s = "MUL R"+ conv(Re++)+",R"+conv($1.Rnum)+", R"+conv($3.Rnum); 
											print(s);  
											}  
	| MathExpr '/' MathExpr 	 {$$.type=checkOperation($1.type,'/',$3.type); $$.Rnum=Re;
											string s = "DIV R"+ conv(Re++)+",R"+conv($1.Rnum)+", R"+conv($3.Rnum); 
											print(s);
											}  
	| VALUES		{$$.Rnum=$1.Rnum; $$.type=$1.type; }
	;


expressions				: Expr 					 {   $$.Rnum=$1.Rnum;          $$.type=$1.type;}
						| MathExpr			 {$$.Rnum=$1.Rnum;  $$.type=$1.type;}	
						|LogicExpr             {$$.Rnum=$1.Rnum; $$.type=$1.type;}	
							
						;	
Expr:
    MathExpr LE_OP MathExpr          {string str = "CMPLE R" + conv($1.Rnum) + ", R" +conv($3.Rnum); print(str);}
    | MathExpr GE_OP MathExpr          {string str = "CMPGE R" + conv($1.Rnum) + ", R"+ conv($3.Rnum);print(str); }
    | MathExpr EQ_OP MathExpr         {string str = "CMPE R" + conv($1.Rnum) + ", R" +conv($3.Rnum);print(str);}
    | MathExpr NG_EQ MathExpr            {string str = "CMPNE R" + conv($1.Rnum) + ", R"+ conv($3.Rnum);print(str);}
    | MathExpr '>' MathExpr             {string str = "CMPG R" + conv($1.Rnum) + ", R" +conv($3.Rnum);print(str);}
    | MathExpr '<' MathExpr           {string str = "CMPL R" + conv($1.Rnum) + ", R" +conv($3.Rnum);print(str);}
    ;


LogicExpr:VALUES AND_OP VALUES    { $$.type=checkOperation($1.type,'l',$3.type); string str = "AND R"+conv(Re++)+" ,R"+ conv($1.Rnum) + ", R" +conv($3.Rnum); print(str);}  
	| VALUES OR_OP VALUES    {$$.type=checkOperation($1.type,'l',$3.type);string str = "OR R"+conv(Re++)+" ,R"+ conv($1.Rnum) + ", R" +conv($3.Rnum); print(str);}  
	| NE_OP	VALUES      {$$.type=checkOperation(-1,'l',$2.type);string str = "NOT R"+conv(Re++)+" ,R"+ conv($2.Rnum); print(str); }
	| Bstart LogicExpr Bend  {printf("brackets first");}	
	;
	

VALUES: IDENTIFIER    { string s= "MOV R"+conv(Re++)+" , "+string($1);print(s); $$.Rnum=Re-1; $$.type=GetIdentifier($1).type;}
        |INTVAL        {string s= "MOV R"+conv(Re++)+" , "+conv($1);print(s); $$.Rnum=Re-1; $$.type=INTVAL;
							
						
							
							}
        |DOUBLEVAL    {string s= "MOV R"+conv(Re++)+" , "+conv($1);print(s); $$.Rnum=Re-1;$$.type=DOUBLEVAL;}
        |CHARVAL        {
                            stringstream ss;
                            string x;
                            ss << $1;
                            ss >> x;
                            string s= "MOV R"+conv(Re++)+" , "+x;
							
                            print(s);  $$.Rnum=Re-1; $$.type=CHARVAL;
                            }

        |TRUE1        {string s= "MOV R"+conv(Re++)+" , "+"1";print(s); $$.Rnum=Re-1; $$.type=BOOLVAL;}
        |FALSE1        {string s= "MOV R"+conv(Re++)+" , "+"0";print(s);  $$.Rnum=Re-1; $$.type=BOOLVAL;}
        |STRINGVAL        {string s="MOV R"+conv(Re++)+" , "+$1;print(s); $$.Rnum=Re-1;  $$.type=STRINGVAL;}
        ;

error1  : Decleration {cout<<"insert  semi semicolon at line"<<int(ceil(yylineno/2.00))<<endl; exit(1);}
		|Assignment {cout<<"insert semi semicolon at line "<<int(ceil(yylineno/2.00))<<endl; exit(1);} 
		|ConstDecleration 	{cout<<"insert semi semicolon at line "<<int(ceil(yylineno/2.00))<<endl; exit(1);} 
						
/* statement and block statements */
Stmt: Decleration ';'    {Re=0;}
		|error1
		|Assignment ';'  {Re=0;}
		|ConstDecleration ';' {Re=0;}
		| IfStmt			{Re=0;}
		| coutstatement		{Re=0;}
		| cinstatement	{Re=0;}
		| expressions	{Re=0;}
		|WhileStmt	{Re=0;}
		|SwitchStatement	{Re=0;}
		|DoWhileStmt		{Re=0;}
		|ForStmt		{Re=0;}
		| contStat		{Re=0;}
		| breakStat		{Re=0;}	
		|pSymbol
		;
BLOCKStmt:	Kstart StmtList Kend
		| Stmt
	;
contStat : CONTINUE ';'{ cout << "JMP Label" << labelsLoop[(int)labelsLoop.size() - 1] << endl;}
breakStat: BREAK ';' { cout << "JMP Label" << labelsLoop[(int)labelsLoop.size() - 2] << endl; }


Kstart : '{' { scope_level++;}

ifStart :'{' {scope_level++; cout<<"JF Label"<<labelmax<<endl;labelsIf.push_back(labelmax++);}
ifEnd1 : '}' { 
				symbolTable[scope_level].clear();
				scope_level--;
                cout << "Label" << *labelsIf.rbegin()<<" :" << endl; 
                                labelsIf.pop_back();
                }
ifEnd : '}'  {
				symbolTable[scope_level].clear();
				scope_level--;
                cout << "JMP Label" << labelmax << endl;
                cout << "Label" << *labelsIf.rbegin()<<" :" << endl;
                labelsIf.pop_back();
                labelsIf.push_back(labelmax++);
}

ELSEend : '}' {
				symbolTable[scope_level].clear();
				scope_level--;
				int x=*labelsIf.rbegin();
				labelsIf.pop_back();
				cout<<"Label"<<x<<" : "<<endl;
				}
Kend : '}'	{symbolTable[scope_level].clear(); scope_level--;}
Bstart : '('  { ;}
Bend :')'    {;}	


StmtList:	StmtList Stmt
	|
	;	
	
	
/* IfStmt Block lazm ben { } */

		
	expressionsif : expressions {

			if($1.type==BOOLVAL)
			{
			string s="CMPE R"+conv($1.Rnum)+" ,1";
			print(s);
			}
	}	
                  
IfStmt : IF Bstart expressionsif Bend ifStart StmtList ifEnd1   { } 
  		| IF Bstart expressionsif Bend ifStart StmtList ifEnd ELSE Kstart StmtList ELSEend{ 	}
;
		
/* switch Block lazm ben { } */	

SwitchStatement     :   SWITCH Bstart MathExpr switchEndExpr  Kstart   CaseBlock Kend                  {
								labelsLoop.pop_back();
								cout << "Label" << *labelsLoop.rbegin() << " :\n";
								labelsLoop.pop_back();
								}
                    ;
CaseBlock :              CaseStatement CaseBlock

				|
				;
			
cv  : 			INTVAL  {if(switchStore)caseValue = conv($1); }
				|CHARVAL {if(switchStore) 
							{
							stringstream ss;
                            string x;
                            ss << $1;
                            ss >> x;
							caseValue = x;
							}
					}

CaseStatement       :  case cv CmpCase StmtList        { cout << "Label" << *labelsIf.rbegin() << " :\n"; labelsIf.pop_back(); } 
                    
					| default ':' StmtList            { } 
                    ;		
		
switchEndExpr : ')' {
						switchRegister = Re - 1;
						labelsLoop.push_back(labelmax++);
						labelsLoop.push_back(labelmax++);
						}
		
case : CASE { switchStore = true;}		
default : DEFAULT {	}
CmpCase : ':' { cout << "CMPE R" << switchRegister << " , " << caseValue << endl;
				switchStore = false;
				cout << "JF Label " << labelmax << endl;
				labelsIf.push_back(labelmax++);
				}

	
/* WHILE STATEMENT */
While      : WHILE {printStore=1;}
ENDbWhile   : ')'  {		
						cout << "JF Label" << labelmax << endl;
						labelsLoop.push_back(labelmax++);
						cout << "Label" << labelmax << " :\n";
						labelsLoop.push_back(labelmax++);
						printStore=0;	
					}
WhileStmt:  While Bstart Expr ENDbWhile BLOCKStmt  {printEndfor();}
	;	
	
/* do while statement */


do          :DO   {
						labelsLoop.push_back(labelmax++);
					cout << "Label" << labelmax << " :\n";
						labelsLoop.push_back(labelmax++);
					}

DoWhileStmt : do BLOCKStmt  WHILE Bstart Expr Bend  {printEndfor();}

/* FOR LOOP STATEMENT */



ENDexpr1   : ';'  {		printStore = 1;			}
ENDexpr2   : ';'  {		
						cout << "JF Label" << labelmax << endl;
						labelsLoop.push_back(labelmax++);
						cout << "Label" << labelmax << " :\n";
						labelsLoop.push_back(labelmax++);
						printStore = 2;			
					}
ENDFOR    : ')' 	{   printStore = 0;         }

ForStmt: FOR Bstart Assignment ENDexpr1 Expr ENDexpr2 Assignment ENDFOR BLOCKStmt   {
								printEndfor();	}

            |FOR Bstart Decleration ENDexpr1 Expr ENDexpr2 Assignment ENDFOR BLOCKStmt   {
								printEndfor();	
}
 


pSymbol  :PrintSymbol {  print_identifrs(); }
/* cin & cout statement */
cinstatement:
	CIN cinstatement2  ';'{printf("cin match statement\n");}
cinstatement2 : CINOP IDENTIFIER cinstatement3
cinstatement3 : cinstatement2
				| 

	
coutstatement:
	COUT  coutstatement2 ';' {printf("match coutstatement \n");}	
coutstatement2 :COUTOP  COUTSTR endstatement
				
endstatement: 	 COUTOP ENDL 
				| coutstatement2
				|
%%



int checkOperation(int type1, char oper,int type2)
{


	if(oper=='+'||oper=='-'||oper=='*'||oper=='/')
	{
		if(type1==CHARVAL||type2==CHARVAL)
		{
			cout<<"Error at line number "<<int(ceil(yylineno/2.00))<<": no Arithmetic operators for (char) type";
			exit(1);
		}
		if(type1==BOOLVAL||type2==BOOLVAL)
		{
			cout<<"Error at line number "<<int(ceil(yylineno/2.00))<<": no Arithmetic operators for (bool) type";
			exit(1);
		}
		if(type1==STRINGVAL||type2==STRINGVAL)
		{
			cout<<"Error at line number "<<int(ceil(yylineno/2.00))<<": no Arithmetic operators for (STRING) type";
			exit(1);
		}
		if(type1==type2)
		return type1;
		if((type1==INTVAL&&type2==DOUBLEVAL)||((type2==INTVAL&&type1==DOUBLEVAL)))
				return DOUBLEVAL;
		
	}
	
	if(oper=='l')
	{
		if(type1==-1)
		{
			if(type2!=BOOLVAL)
			{
				cout<<"Error at line number "<<int(ceil(yylineno/2.00))<<": expect bool variables";
				exit(1);
			}
		}
		else
		if(!(type1==BOOLVAL&&type2==BOOLVAL))
		{
			cout<<"error at line number : "<<int(ceil(yylineno/2.00))<<": expect bool variables";
			exit(1);
		}
		return BOOLVAL;
	}
	
	
	
}



int GetIdLevel(string name)
{
	for(int i=1;i<=scope_level;i++)
	{
		if (symbolTable[i].find(name) != symbolTable[i].end())
			return i;
	}
	return -1;  //  not found
}

IDInfo GetIdentifier(string name)
{
	
	int IdentifierScopeLevel=GetIdLevel(name);
	
	if ( IdentifierScopeLevel==-1 ) {
		cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": Identifier ("<<name<<") not declared";
		exit(1);
	} else {
		if(symbolTable[IdentifierScopeLevel][name].isInit==0)
		{
			cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": Identifier ("<<name<<") not Initialized";
			exit(1);
		}
	  return symbolTable[IdentifierScopeLevel][name];
	}
}


void newIdentifier(string name,int identfierType,int assignType,int isConst)
{
	
	map<int,string> types;
	types[INTVAL]="Integer";
	types[DOUBLEVAL]="Double";
	types[CHARVAL]="Char";
	types[BOOLVAL]="Bool";
	int IdentifierScopeLevel=GetIdLevel(name);
	if ( IdentifierScopeLevel==scope_level ) {
			cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": Identifier "<<name<<" redefinition";
			exit(1);
	}
	
	if(assignType!=-1)
	{
		if((identfierType==INTVAL&&assignType==DOUBLEVAL)||((assignType==INTVAL&&identfierType==DOUBLEVAL))||identfierType==assignType)
		{	
			IDInfo x;
			x.type=identfierType;
			x.isInit=1;
			x.isConst=isConst;
			symbolTable[scope_level][name]=x;
			return;
		}
		cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": no type conversion from "<<types[assignType]<<" to "<<types[identfierType];
		exit(1);
	}
	
			IDInfo x;
			x.type=identfierType;
			x.isInit=0;
			x.isConst=isConst;

	symbolTable[scope_level][name]=x;
}

void updateIdentifier(string name,int assignType)
{
	
	map<int,string> types;
	types[INTVAL]="Integer";
	types[DOUBLEVAL]="Double";
	types[CHARVAL]="Char";
	types[BOOLVAL]="Bool";
	types[STRINGVAL]="String";
	
	int IdentifierScopeLevel=GetIdLevel(name);
	if ( IdentifierScopeLevel==-1 ) {
		cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": Identifier ("<<name<<") not declared";
		exit(1);
	}
	
	if(symbolTable[IdentifierScopeLevel][name].isConst==1)
	{
		cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": Identifier ("<<name<<") is const";
		exit(1);
	}
	int identfierType=symbolTable[IdentifierScopeLevel][name].type;
	if((identfierType==INTVAL&&assignType==DOUBLEVAL)||((assignType==INTVAL&&identfierType==DOUBLEVAL))||identfierType==assignType)
	{	
		return;
	}
	cout<<"Error at Line "<<int(ceil(yylineno/2.00))<<": no type conversion from "<<types[assignType]<<" to "<<types[identfierType];
	exit(1);
	
	
}
string conv (int a)
{
																				stringstream ss;
																				ss << a;
																				string str = ss.str();																			
																				return str;
}
void print(string s)
{
	
	if (printStore) for1[printStore] += s + "\n";
	if (printStore < 2)
			cout<<s<<endl;



}

void printEndfor(){
	cout << for1[2] << for1[1];
	cout << "JT Label" << *labelsLoop.rbegin() << endl;
	labelsLoop.pop_back();
	cout <<"Label"<< *labelsLoop.rbegin()<<" :" << endl;
	labelsLoop.pop_back();
	for1[1]="";
	for1[2]="";
}
void print_identifrs(){
    map<int,string> types;
    types[INTVAL]="Integer";
    types[DOUBLEVAL]="Double";
    types[CHARVAL]="Char";
    types[BOOLVAL]="Bool";
    types[STRINGVAL]="String";
    map <int,map <string,IDInfo> >::iterator it;
    map<string,IDInfo>::iterator it2;
    it = symbolTable.begin();
    while(it != symbolTable.end()){
        cout<<"------------------------------------------------------------------------------------------"<<endl<<endl;
        cout <<"level : "<<it->first<<" ";
        it2 = it->second.begin();
        while(it2 != it->second.end()){
            cout<<"identifer : "<<it2->first<<" type: "<<types[it2->second.type]<<" isInit: "<<it2->second.isInit<<" isConst: "<<it2->second.isConst<<endl;
            it2++;
        }
      it++;
    }

        cout<<"------------------------------------------------------------------------------------------"<<endl<<endl;
}
int main() {

	freopen("output.txt","w",stdout);

printf("Start program \n");
yyin = fopen("scope.txt", "r");
yyparse();
return 1;
}

void yyerror( const char*  mes) 
{
	int line=int(ceil(yylineno/2.00));
	cout<<"unexpected "<<yytext<<" at line"<<line;
}		/* prints an error message */
