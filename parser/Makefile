all :
	flex lex.l
	yacc -d -v yacctemplate.y
	gcc lex.yy.c y.tab.c -I . -lfl -ly
