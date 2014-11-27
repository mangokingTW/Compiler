%{
#include <stdio.h>
#include <stdlib.h>

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
%}

%token TMPDEFLIST


/* IDENTIFIER */
%token ID           /* identifier */

/* TYPES */
%token INT          /* keyword */
%token DOUBLE       /* keyword */
%token FLOAT        /* keyword */
%token STRING       /* keyword */
%token BOOLEAN         /* keyword */
%token VOID
%token CONST            /* keyword */

/* SYMBOL */
%token LEFTS
%token RIGHTS
%token LEFTM
%token RIGHTM
%token LEFTB
%token RIGHTB
%token COMMA
%token SEMICOLON    /* ; */

/* OPERATORS */
%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD
%token LT
%token LE
%token EQ
%token GT
%token GE
%token NE
%token SIGN
%token AND
%token OR
%token NOT

/* CONST VALUES */
%token CONST_INT
%token CONST_FLOAT
%token CONST_DOUBLE
%token CONST_STRING
%token CONST_BOOLEAN

%token RETURN
%token CONTINUE
%token BREAK

%token IF
%token ELSE
%token WHILE
%token DO
%token FOR

%token PRINT
%token READ

%%

program : decl_chain definition_list decl_and_def_list
    	;

decl_chain : decl_chain declaration_list
           |
           ;

decl_and_def_list : definition_list decl_and_def_list
                  | declaration_list decl_and_def_list
                  |
                  ;


declaration_list : const_decl
                 | var_decl
                 | funct_decl
				 ;

definition_list : funct_def
                | void_funct_def
                ;
                
void_funct_def : VOID ID LEFTS funct_val_decl RIGHTS LEFTB statements RIGHTB
               ;

funct_def : type ID LEFTS funct_val_decl RIGHTS LEFTB statements RIGHTB 
          ;

var_decl : type id_list SEMICOLON
         ;

const_decl : CONST type const_id_list SEMICOLON
           ;

funct_decl : type ID LEFTS funct_val_decl RIGHTS SEMICOLON
           | VOID ID LEFTS funct_val_decl RIGHTS SEMICOLON
           ;

funct_val_decl :    type identifier COMMA funct_val_decl
               |    type identifier
               |
               ;

statements : decl_local_val statements
           | funct_call statements
           | if_condition statements
           | var_assign statements
           | while_loop statements
           | for_loop statements
           | simple_state statements
           | jump_state statements
           |
           ;

simple_state : PRINT exp SEMICOLON
             | READ identifier SEMICOLON
             ;

decl_local_val : type decl_id_list SEMICOLON
               | CONST type const_id_list SEMICOLON
                ;

funct_call : ID LEFTS funct_val_assign RIGHTS SEMICOLON
           ;

funct_val_assign : identifier COMMA funct_val_decl
                 | const_val COMMA funct_val_decl
                 | identifier
                 | const_val
                 |
                 ;

if_condition : IF LEFTS exp RIGHTS LEFTB statements RIGHTB
             | IF LEFTS exp RIGHTS LEFTB statements RIGHTB 
             ELSE LEFTB statements RIGHTB
             ;

var_assign : id_list SEMICOLON 
           ;

while_loop : WHILE LEFTS exp RIGHTS LEFTB statements RIGHTB
           | DO LEFTB statements RIGHTB WHILE LEFTS exp RIGHTS SEMICOLON
           ;

for_loop : FOR LEFTS var_assign_f SEMICOLON exp_f SEMICOLON var_assign_f RIGHTS
           LEFTB statements RIGHTB

exp_f : exp
      |
      ;
      
var_assign_f : id_list
             |
             ;

jump_state : RETURN exp SEMICOLON
           | BREAK SEMICOLON
           | CONTINUE SEMICOLON
           ;

type : INT
     | DOUBLE
     | FLOAT
     | STRING
     | BOOLEAN
     ; 

id_list : identifier id_list_a
        | identifier SIGN exp id_list_a
        | identifier SIGN LEFTB sign_array RIGHTB id_list_a
        ;

id_list_a :  COMMA identifier id_list_a
        | COMMA identifier SIGN exp id_list_a
        | COMMA identifier SIGN LEFTB sign_array RIGHTB id_list_a
        |
        ;
        
decl_id_list : decl_id decl_id_list_a
        | decl_id SIGN exp decl_id_list_a
        | decl_id SIGN LEFTB sign_array RIGHTB decl_id_list_a
        ;

decl_id_list_a :  COMMA decl_id decl_id_list_a
        | COMMA decl_id SIGN exp decl_id_list_a
        | COMMA decl_id SIGN LEFTB sign_array RIGHTB decl_id_list_a
        |
        ;

sign_array : exp COMMA sign_array
           | exp
           ;

const_id_list : decl_id SIGN const_val const_id_list_a
              ;

const_id_list_a : COMMA decl_id SIGN const_val const_id_list_a
                |
                ;
                
const_val : CONST_INT
          | CONST_FLOAT
          | CONST_DOUBLE
          | CONST_STRING
          | CONST_BOOLEAN
          ;

exp : exp PLUS term 
    | exp MINUS term
    | exp AND term
    | exp OR term
    | exp LT term
    | exp GT term
    | exp LE term
    | exp GE term
    | exp NE term
    | exp EQ term
    | term
    ;

term : term MUL fac
     | term DIV fac
     | term MOD fac
     | fac
     ;
     
fac : LEFTS exp RIGHTS
    | identifier
    | MINUS identifier
    | NOT identifier
    | const_val
    | MINUS const_val
    | NOT const_val
    | funct_id
    | MINUS funct_id
    | NOT funct_id
    ;

funct_id : ID LEFTS funct_val_assign RIGHTS
        ;

identifier : ID array_chain
	        ;

array_chain : LEFTM exp RIGHTM array_chain
            |
            ;

decl_id : ID integer_const_chain
        ;
        
integer_const_chain : LEFTM CONST_INT RIGHTM integer_const_chain
                    |
                    ;

%%

int yyerror( char *msg )
{
  fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
  fprintf( stderr, "|--------------------------------------------------------------------------\n" );
  exit(-1);
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}

