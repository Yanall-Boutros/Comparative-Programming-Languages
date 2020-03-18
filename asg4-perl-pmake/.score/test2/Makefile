# $Id: Makefile,v 1.1 2020-02-03 15:27:37-08 - - $

all : hello

hello : main.o hello.o
	gcc main.o hello.o -o hello

main.o : main.c hello.h
	gcc -c main.c

hello.o : hello.c hello.h
	gcc -c hello.c

ci : Makefile main.c hello.c hello.h
	cid Makefile main.c hello.c hello.h

test : hello
	./hello

clean : 
	- rm hello.o main.o

spotless : clean
	- rm hello

