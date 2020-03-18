# $Id: Makefile,v 1.1 2020-02-03 15:28:10-08 - - $

SOURCES = main.c hello.c
HEADERS = hello.h
OBJECTS = main.o hello.o
CHECKIN = Makefile ${SOURCES} ${HEADERS}
EXECBIN = hello

all : ${EXECBIN}

${EXECBIN} : ${OBJECTS}
	gcc ${OBJECTS} -o ${EXECBIN}

main.o : main.c hello.h
	gcc -c main.c

hello.o : hello.c hello.h
	gcc -c hello.c

ci : ${CHECKIN}
	cid ${CHECKIN}

test : ${EXECBIN}
	./${EXECBIN}

clean : 
	- rm ${OBJECTS}

spotless : clean
	- rm ${EXECBIN}

