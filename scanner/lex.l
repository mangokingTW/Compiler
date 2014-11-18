%{
#include <string.h>

#define MAX_LINE_LENGTH 257
#define MAX_ID_LENGTH 257
#define MAX_STRING_LENGTH 257
#define LIST                strlcat(buf, yytext, MAX_LINE_LENGTH)
#define token(t)            {LIST; if (Opt_Token) printf("<%s>\n", #t);}
#define tokenInteger(t, i)  {LIST; if (Opt_Token) printf("<%s:%d>\n", t, i);}
#define tokenString(t, s)   {LIST; if (Opt_Token) printf("<%s:%s>\n", t, s);}
int Opt_Source = 1;
int Opt_Token = 1;
int Opt_Statistic =1;
int linenum = 1;
int Last_State = 0;
char buf[MAX_LINE_LENGTH];
char strContent[MAX_STRING_LENGTH];
typedef struct ids{
	char id_name[MAX_ID_LENGTH];
	int freq;
	struct ids *next;
} id;
id *id_head;
void set(id **, char *);
id *new_id();
%}

%x PRAGMA
%x SOURCE
%x TOKEN
%x STATISTIC
%x OTHER
%x STR
%x STR_ESCAPE
%x COMMENT

%%
<INITIAL>[\t ]+	{
		LIST;
	}
^#pragma	{
		LIST;
		BEGIN PRAGMA;
	}
<PRAGMA>"source"	{
		LIST;
		BEGIN SOURCE;
	}
<PRAGMA>"token"	{
		LIST;
		BEGIN TOKEN;
	}
<PRAGMA>"statistic"	{
		LIST;
		BEGIN STATISTIC;
	}

<SOURCE>"on"	{
		LIST;
		Opt_Source = 1;
		BEGIN OTHER;
	}
<SOURCE>"off"	{
		LIST;
		Opt_Source = 0;
		BEGIN OTHER;
	}
<SOURCE>[\t ]+	{
		LIST;
	}
<SOURCE>[^\t ]	{
		//printf("Error at line %d: %s\n",linenum, yytext);
                  fprintf(stderr,"Error at line %d: %s\n",linenum,yytext);
        exit(1);
	}
<TOKEN>"on"	{
		LIST;
		Opt_Token = 1;
		BEGIN OTHER;
	}
<TOKEN>"off"	{
		LIST;
		Opt_Token = 0;
		BEGIN OTHER;
	}
<TOKEN>[\t ]+	{
		LIST;
	}
<TOKEN>[^\t ]	{
		//printf("Error at token line %d: %s\n",linenum, yytext);
                 fprintf(stderr,"Error at line %d: %s\n",linenum,yytext);
        exit(1);
	}
<STATISTIC>"on"	{
		LIST;
		Opt_Statistic = 1;
		BEGIN OTHER;
	}
<STATISTIC>"off"	{
		LIST;
		Opt_Statistic = 0;
		BEGIN OTHER;
	}
<STATISTIC>[\t ]+	{
		LIST;
	}
<STATISTIC>[^\t ]	{
		//printf("Error at line %d: %s\n",linenum, yytext);
                 fprintf(stderr,"Error at line %d: %s\n",linenum,yytext);
        exit(1);
	}
<OTHER>\/\/.*	{
		LIST;
		BEGIN INITIAL;
	}
<OTHER>"/*"	{
		LIST;
        Last_State = OTHER;
		BEGIN COMMENT;
	}

<OTHER>\n		{
		LIST;
  	      if (Opt_Source)
  	        printf("%d:%s\n", linenum, buf);
  	      linenum++;
  	      buf[0] = '\0';
		  BEGIN INITIAL;
	}
<OTHER>[^\n]		{
		//printf("Error at line %d: %s\n",linenum, yytext);
                  fprintf(stderr,"Error at line %d: %s\n",linenum,yytext);
                exit(1);

	}
<PRAGMA>[\t ]+		{
		LIST;
	}
<PRAGMA>[^\t ]		{
		//printf("Error at pragma line %d: %s\n",linenum, yytext);
                  fprintf(stderr,"Error at line %d: %s\n",linenum,yytext);
                exit(1);

	}
"("	{
		tokenString("delim", "(");
	}
")"	{
		tokenString("delim", ")");
	}
","	{
		tokenString("delim", ",");
	}
";"	{
		tokenString("delim", ";");
	}
"["	{
		tokenString("delim", "[");
	}
"]"	{
		tokenString("delim", "]");
	}
"{"	{
		tokenString("delim", "{");
	}
"}"	{
		tokenString("delim", "}");
	}
"+"	{
		token("+");
	}
"-"	{
		token("-");
	}
"*"	{
		token("*");
	}
"/"	{
		token("/");
	}
"%"	{
		token("%");
	}
"="	{
		token("=");
	}
"<"	{
		token("<");
	}
"<="	{
		token("<=");
	}
"!="	{
		token("!=");
	}
">="	{
		token(">=");
	}
">"	{
		token(">");
	}
"=="	{
		token("==");
	}
"&&"	{
		token("&&");
	}
"||"	{
		token("||");
	}
"!"	{
		token("!");
	}
read |
boolean |
while |
do |
if |
else |
true | 
false | 
for | 
int | 
print |
bool | 
void | 
float | 
double | 
string |
continue | 
break | 
return	{
		tokenString("KW", yytext);
	}

0 |
[1-9][0-9]* {
		tokenString("integer", yytext);
	}
[0-9]+\.[0-9]+	{
		tokenString("float", yytext);
	}

([0-9]+|([0-9]*\.[0-9]+))([eE][-+]?[0-9]+)	{
		tokenString("scientific", yytext);
	}
"\""	{ 
		LIST;
                strContent[0] = '\0';
		BEGIN STR;
	}
<STR>"\\"	{
		LIST;
		BEGIN STR_ESCAPE;
	}
<STR_ESCAPE>\"	{
		LIST;
		strlcat (strContent, "\"", MAX_STRING_LENGTH);
		BEGIN STR;
	}
<STR_ESCAPE>n	{
		LIST;
		strlcat( strContent, "\n", MAX_STRING_LENGTH);
		BEGIN STR;
	}
<STR_ESCAPE>t	{
		LIST;
		strlcat( strContent, "\t", MAX_STRING_LENGTH);
		BEGIN STR;
	}
<STR_ESCAPE>\\	{
		LIST;
		strlcat( strContent, "\\", MAX_STRING_LENGTH);
		BEGIN STR;
	}
<STR_ESCAPE>\n  {
    fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
    exit(1);
}
<STR_ESCAPE>[^\"nt\\]	{
		LIST;
		strlcat( strContent, "\\", MAX_STRING_LENGTH);
		strlcat( strContent, yytext, MAX_STRING_LENGTH);
		BEGIN STR;
	}
<STR>"\n"	{
		//printf("Error at line %d: newline\n", linenum);
                 fprintf(stderr,"Error at line %d: %s\n",linenum,yytext);
		exit(1);
	}

<STR>\"	{
		//LIST;
		//strlcat( buf, "\"");
		//if( Opt_Token)
		tokenString("string", strContent);
		BEGIN INITIAL;
		
	}
<STR>[^\\\n\"]	{
		LIST;
		strlcat( strContent, yytext, MAX_STRING_LENGTH);
	}

"/*"	{
		LIST;
        Last_State = INITIAL;
		BEGIN COMMENT;
	}
<COMMENT>"\n"	{
		LIST;
		if (Opt_Source)
  	        printf("%d:%s\n", linenum, buf);
		linenum++;
                buf[0] = '\0';
	}

			
<COMMENT>.	{
		LIST;
	}
<COMMENT>"*/"	{
		LIST;
		BEGIN Last_State;
	}
\/\/.*	{
		LIST;
	}
 /*<<EOF>> {
          if(Opt_Source)
            printf("EOF%d:%s\n",linenum,buf);
		  buf[0] = '\0';
		  yyterminate();
        }*/
\n	{
  	      LIST;
  	      if (Opt_Source)
  	        printf("%d:%s\n", linenum, buf);
  	      linenum++;
  	      buf[0] = '\0';
        }

[a-zA-Z_][a-zA-Z0-9_]{0,255}	{
		if(Opt_Statistic){
			set(&id_head,yytext);
		}
		tokenString("id",yytext);
	}
.	{
		//printf("Error at line %d: %s\n",linenum, yytext);
                 fprintf(stderr,"Error at line %d: %s\n",linenum, yytext);
		exit(1);
	}
%%

int main( int argc, char **argv )
{

	if( argc == 1 )
	{
		yyin = stdin;
	}
	else if( argc == 2 )
	{
		FILE *fp = fopen( argv[1], "r" );
		if( fp == NULL ) {
				fprintf( stderr, "Open file error\n" );
				exit(-1);
		}
		yyin = fp;
	}
	else
	{
	  	fprintf( stderr, "Usage: ./scanner [filename]\n" );
   		exit(0);
 	} 


  	id_head = NULL;
	yylex();
	
  	if(Opt_Statistic){
		printf("frequencies of identifiers:\n");
		id *offset = id_head;
  		while(offset){
				printf("%s\t%d\n",offset->id_name, offset->freq);
    			offset = offset->next;
  		}
	}

  	exit(0);
}
void set(id **head, char *yytext)
{
	id *offset = *head;
	id *prev = NULL;	
	if(offset == NULL){
		*head = new_id();
		strncpy((*head)->id_name, yytext,strlen(yytext));
		(*head)->freq = 1;
	}else{
		while(offset){
			if(!strncmp(offset->id_name, yytext, (strlen(yytext)>=strlen(offset->id_name)?strlen(yytext):strlen(offset->id_name)))){
				offset->freq+=1;
				break;
			}
			prev = offset;
			offset = offset -> next;
		}
		if(!offset){
			prev->next = new_id();
			strncpy(prev->next->id_name, yytext, strlen(yytext));
			prev->next->freq+=1;
		}
	}
}
id *new_id()
{
	id *temp = (id *)malloc(sizeof(id));
	temp->next = NULL;
	memset(temp->id_name, 0, 256);
	temp->freq = 0;
	return temp;
}
			
