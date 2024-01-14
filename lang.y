%{
#include <stdio.h>
#include <stdlib.h>

int yyerror(char *s);

int yydebug = 1;
%}
%token PROGRAM;
%token IDENTIFIER;
%token INTCONSTANT;
%token STRINGCONSTANT;
%token CHAR;
%token INT;
%token STRING;
%token IF;
%token ELSE;
%token WHILE;
%token CONST;
%token DO;
%token OF;
%token READ;
%token READNUMBER;
%token THEN;
%token VAR;
%token WRITE;
%token REPEAT;
%token UNTIL;
%token DECIDE;
%token OR;
%token THIS;
%token IS;
%token GOOD;
%token THAT;
%token TELLS;
%token YOU;
%token TO;
%token STOP;
%token START;
%token WORKING;
%token WITH;
%token FROM;
%token GOING;
%token UP;
%token DOWN;
%token GO;
%token HALT;
%token DECLARE;

%token PLUS;
%token MINUS;
%token TIMES;
%token DIV;

%token LESS;
%token LESSEQUAL;
%token EQUAL;
%token BIGGEREQUAL;
%token DOUBLEEQUAL;
%token NOTEQUAL;
%token BIGGER;
%token OPENSQUARE;
%token CLOSESQUARE;
%token OPENROUND;
%token CLOSEROUND;

%start Program

%%
Program: PROGRAM CompoundStatement {printf("Program ->  program CompoundStatement \n"); };

CompoundStatement : GO CompoundStatement {printf("CompoundStatement -> go CompoundStatement\n"); }
		  | HALT {printf("CompoundStatement -> halt ;\n");}
		  | Statement CompoundStatement {printf("CompoundStatement -> Statement CompoundStatement ;\n");}
		  ;

Statement : DeclarationStatement {printf("Statement -> DeclarationStatement \n");}
	|   AssignmentStatement {printf("Statement -> AssignmentStatement \n");}
	|   IfStatement {printf("Statement -> IfStatement \n");}
	|   WhileStatement {printf("Statement -> WhileStatement \n");}
	|   ForStatement {printf("Statement -> ForStatement \n");}
	|   RepeatStatement {printf("Statement -> ForStatement \n");}
	|   IOStatement {printf("Statement -> IOStatement \n");}
	;

DeclarationStatement: DECLARE Type IDENTIFIER {printf("DeclarationStatement -> TYPE  IDENTIFIER\n");} 
		      | DECLARE Type IDENTIFIER EQUAL Expression {printf("DeclarationStatement -> TYPE IDENTIFIER = Expression");}
			;

AssignmentStatement : IDENTIFIER EQUAL Expression {printf("AssignmentStatement -> IDENTIFIER = Expression\n");};

Expression : Term {printf("Expression -> Term\n");}
	|    Expression MathSymbol Term {printf("Expression -> Expression MathSymbol Term\n");}
	|	 Expression MathSymbol Expression {printf("Expression -> Expression MathSymbol Expression");}
	;

Term : INTCONSTANT {printf("Term -> INTCONSTANT\n");}
       |	IDENTIFIER {printf("Term -> IDENTIFIER\n");}
       |	OPENROUND Expression CLOSEROUND {printf("Term -> ( Expression ) \n");}
       ;
	   
MathSymbol : PLUS {printf("MathSymbol -> PLUS\n");}
	|	MINUS {printf("MathSymbol -> MINUS\n");}
	|	DIV {printf("MathSymbol -> MINUS\n");}
	|	TIMES {printf("MathSymbol -> MINUS\n");}
	;

Type :    INT {printf("Type -> int\n");}
	|	STRING {printf("Type -> string\n");}
	|	CHAR {printf("Type -> char\n");}
      ;

IfStatement : DECIDE Relation THEN DO Statement {printf("IfStatement -> decide Relation then do CompoundStatement\n");}
	| DECIDE Relation THEN DO Statement ELSE DO Statement {printf("IfStatement -> decide Relation then do CompoundStatement else do CompoundStatement");}
	;

WhileStatement : WHILE THIS IS GOOD Relation DO THAT Statement {printf("WhileStatement -> while this is good Relation do that CompoundStatement\n");};

RepeatStatement : DO THIS Statement UNTIL THIS TELLS YOU TO STOP Relation {printf("RepeatStatement -> do this until this tells you to stop CompoundStatement\n");};

ForStatement : START WORKING WITH THIS IDENTIFIER FROM THIS WorkingVariable TO THIS WorkingVariable GOING Direction Statement {printf("ForStatement -> Start working with this IDENTIFIER from this WorkingVariable to this WorkingVariable going Direction CompoundStatement\n");};

WorkingVariable : IDENTIFIER {printf("WorkingVariable -> IDENTIFIER\n");}
	|	INTCONSTANT {printf("WorkingVariable -> INTCONSTANT\n");}
	;
	
Direction : UP {printf("Direction -> up\n");}
	|	DOWN {printf("Direction -> down\n");}
	;


IOStatement : OutputStatement {printf("IOStatement -> OutputStatement\n");}
	|	InputStatement {printf("IOStatement -> InputStatement\n");}
	|	InputNumberStatement {printf("IOStatement -> InputNumberStatement\n");}
	;
	
InputStatement : READ IDENTIFIER {printf("InputStatement -> read IDENTIFIER\n");};

InputNumberStatement : READNUMBER IDENTIFIER {printf("InputNumberStatement -> readNumber IDENTIFIER\n");};

OutputStatement : WRITE IDENTIFIER {printf("OutputStatement -> write IDENTIFIER\n");}




Relation: Expression RelationSymbol Expression {printf("Condition -> Expression Relation Expression\n");};

RelationSymbol: LESS {printf("Relation -> Less\n");}
	| LESSEQUAL {printf("Relation -> LESS OR EQUAL\n");}
	| DOUBLEEQUAL {printf("Relation -> EQUAL\n");}
	| NOTEQUAL {printf("Relation -> NOT EQUAL\n");}
	| BIGGEREQUAL {printf("Relation -> BIGGER OR EQUAL\n");}
	| BIGGER {printf("Relation -> BIGGER\n");}
	;
	

%%
yyerror(char *s)
{	
	printf("%s\n",s);
}

extern FILE *yyin;

main(int argc, char **argv)
{
	if(argc>1) yyin =  fopen(argv[1],"r");
	if(!yyparse()) fprintf(stderr, "\tOK\n");
} 