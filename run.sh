#!/bin/bash
yacc -d lang.y
flex -i lex2.lxi
gcc -o a.exe y.tab.c lex.yy.c