%{
#define LIST                { strncat(buf, yytext, sizeof(buf) - strlen(buf) - 1); }
#define token(t)            { LIST; if (Opt_Token&&Opt_sl&&Opt_st) printf("<\"%s\">\n", t); }
#define tokenString(t, s)   { LIST; if (Opt_Token&&Opt_sl&&Opt_st) printf("<%s:%s>\n", t, s); }
#define MAX_LINE_LENGTH 257
#define MAX_ID_LENGTH 257
int Opt_Source = 1;
int Opt_Token = 1;
int Opt_sl = 1;
int Opt_st = 1;
int Opt_Statistic = 1;
int linenum = 1;
char buf[MAX_LINE_LENGTH];
        char out[65535];
struct frequen{
	char ID[256];
	int freq;
};
int idlength = 0; 
struct frequen idbuff[1024];
%}

%s PRAGSTATE PRAGSOURCESTATE PRAGTOKENSTATE PRAGSTATISTICSTATE PRAGENDSTATE STRINGSTATE
ccomment (\/\/)
digit [0-9]

%%

<STRINGSTATE>{
	
	([^\"\\\n]|\\.)* {


        char* st=yytext;

        int j,i;
        for( j = 0,i = 0 ; st[j] ; j++ ,i++){
                if( st[j] == '\\' ){
                        j++;
                        switch(st[j]){
                        case 'n': 
                                out[i] = '\n';
                                break;
                        case 't':
                                out[i] = '\t';
                                break;
                        case '\\': 
                                out[i] = '\\';
                                break;
                        case '\"': 
                                out[i] = '\"';
                                break;
                        default:
                                out[i++]='\\';
                                out[i]=st[j];
                        }
                }
                else out[i]=st[j];
        }
        out[i]=0;

        LIST;
	}
	
	\" {
		BEGIN INITIAL;
		LIST;
		if( Opt_Token&&Opt_sl&&Opt_st){
        printf("<string:");
        printf("%s",out);
        printf(">\n");
        }
        }
	
	\n { // Unrecognized character
    if(Opt_Token&&Opt_sl&&Opt_st)
    {
        fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
        exit(1);
    }
    else LIST;
	}
}

<PRAGSTATE>{
	
	"source" {
	BEGIN(PRAGSOURCESTATE);
	LIST;
	}

	"token" {
        BEGIN PRAGTOKENSTATE;
        LIST;
	}

	"statistic" {
        BEGIN PRAGSTATISTICSTATE;
        LIST;
	}
	
	" "|"\t" {
		LIST;
	}

	\n { // Unrecognized character
    if(Opt_Token&&Opt_sl&&Opt_st)
    {
        fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
        exit(1);
    }
    else LIST;
}

}

<PRAGSOURCESTATE>{
	
	"on" {
	BEGIN PRAGENDSTATE;
	Opt_Source=1;
	LIST;
	}
	
	"off" {
        BEGIN PRAGENDSTATE;
        Opt_Source=0;
        LIST;
	}

	" "|"\t" {
                LIST;
        }

	\n { // Unrecognized character
    if(Opt_Token&&Opt_sl&&Opt_st)
    {
        fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
        exit(1);
    }
    else LIST;
}
	
}

<PRAGTOKENSTATE>{
        
	"on" {
	BEGIN PRAGENDSTATE;
        Opt_Token=1;
        LIST;
        }

        "off" {
        BEGIN PRAGENDSTATE;
        Opt_Token=0;
        LIST;
        }

        " "|"\t" {
                LIST;
        }

        \n { // Unrecognized character
    if(Opt_Token&&Opt_sl&&Opt_st)
    {
        fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
        exit(1);
    }
    else LIST;
}

}

<PRAGSTATISTICSTATE>{
        
	"on" {
        BEGIN PRAGENDSTATE;
        Opt_Statistic=1;
        LIST;
        }

        "off" {
        BEGIN PRAGENDSTATE;
        Opt_Statistic=0;
        LIST;
        }

        " "|"\t" {
                LIST;
        }

        \n { // Unrecognized character
    if(Opt_Token&&Opt_sl&&Opt_st)
    {
        fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
        exit(1);
    }
    else LIST;
}

}

<PRAGENDSTATE>{ 

	\/\*[^(\*\/|\n)]* {
	Opt_st = 0;
        LIST;
	}
	\*\/ {
        if(Opt_Token&&Opt_sl&&Opt_st){
        	fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
        	exit(1);
	}
        else {
                Opt_st=1;
                LIST;
        }
	}	
	{ccomment} {
	BEGIN INITIAL;
        if(Opt_st)
        {
                Opt_sl = 0;
        }
        LIST;
	}

	" "|"\t" {
		LIST;
	}

	\n {
	BEGIN INITIAL;
    LIST;
   Opt_sl = 1;
   if (Opt_Source)
        printf("%d:%s\n", linenum, buf);
    ++linenum;
    buf[0] = '\0';
}	
	
}

<INITIAL>{

^\#pragma {
        BEGIN(PRAGSTATE);
        LIST;
}

\/\*[^(\*\/|\n)]* {
        Opt_st = 0;
        LIST;
}

\*\/ {
        if(Opt_Token&&Opt_sl&&Opt_st){
                printf("<\"*\">\n");
                printf("<\"/\">\n");
                LIST;
        }
        else {
                Opt_st=1;
                LIST;
        }
}

{ccomment} {
	if(Opt_st)
        {
		Opt_sl = 0;
	}
	LIST;
}

"{"|"}"|"("|")"|"["|"]"|","|";" {
    tokenString("delim", yytext);
}

"+"|"-"|"*"|"/"|"%"|"="|"<"|"<="|"!="|">="|">"|"=="|"&&"|"||"|"!" {
        token(yytext);
}

[0-9]+\.[0-9]+ {
        tokenString("float",yytext);
}

([0-9]+|[0-9]*\.[0-9]+)[eE]("+"|"-"|"")[0-9]+ {
        tokenString("scientific",yytext);
}

[1-9]{digit}*|0 {
        tokenString("integer",yytext);
}

"while"|"do"|"if"|"else"|"true"|"false"|"for"|"int"|"print"|"const"|"read"|"boolean"|"bool"|"void"|"float"|"double"|"string"|"continue"|"break"|"return" {
        tokenString("KW",yytext);
}

[_a-zA-Z][_a-zA-Z0-9]* {
        tokenString("id",yytext);
if(Opt_Token&&Opt_sl&&Opt_st){
        char* inputid = yytext;
        int j;
        int booling = 0;
        for( j = 0 ; j < idlength ; j++ ){
                if( !strcmp(idbuff[j].ID, inputid) ){
                        booling = 1;
                        idbuff[j].freq++;
                        break;
                }
        }
        if( booling==0 ){
                strcpy( idbuff[idlength].ID , inputid );
                idbuff[idlength].freq++;
                idlength++;
        }
}
}
\" {
	BEGIN STRINGSTATE;
	LIST;
}
	/*
\"([^\"\\\n]|\\.)*\" {


	char* st=yytext;
	char out[65535];
	int j,i;
	for( j = 0,i = 0 ; st[j] ; j++ ,i++){
		if( st[j] == '\\' ){
			j++;
			switch(st[j]){
			case 'n': 
				out[i] = '\n';
                                break;
			case 't':
				out[i] = '\t';
                                break;
			case '\\': 
				out[i] = '\\';
                                break;
			case '\"': 
				out[i] = '\"';
                                break;
			default:
				out[i++]='\\';
				out[i]=st[j];
			}
		}
		else out[i]=st[j];
	}
	out[i-1]=0;

	LIST;
	if( Opt_Token&&Opt_sl&&Opt_st){
	printf("<string:");
	printf("%s",&out[1]);
	printf(">\n");
	}
}*/


"\t"|" " {
	LIST;
}

\n {
    LIST;
   Opt_sl = 1;
   if (Opt_Source)
        printf("%d:%s\n", linenum, buf);
    ++linenum;
    buf[0] = '\0';
}

}

. { // Unrecognized character
    if(Opt_Token&&Opt_sl&&Opt_st)
    {
    	fprintf(stderr, "Error at line %d: %s\n", linenum, yytext);
    	exit(1);
    }
    else LIST;
}

%%

int main( int argc, char **argv )
{
    if (argc >= 2)
    {
        yyin = fopen( argv[1], "r" );
        if ( NULL == yyin ) {
            fprintf( stderr, "Cannot open: %s\n", argv[1] );
            exit(-1);
        }
    }
    else
    {
        yyin = stdin;
    }

    yylex();

    if (Opt_Statistic)
    {
        // Print frequency table
	printf("frequencies of identifiers:\n");
	int j ;
	for( j = 0 ; j < idlength ; j++ )
		printf("%s\t%d\n",idbuff[j].ID,idbuff[j].freq);
    }

    exit(0);
}
