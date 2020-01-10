# $Id: Makefile,v 1.2 2015-11-03 13:44:07-08 - - $

MKFILE   = Makefile
CCOPTS   = -g -O0 -Wall -Wextra
SOURCES  = main.c hello.c
HEADERS  = hello.h
OBJECTS  = main.o hello.o
CHECKIN  = ${MKFILE} ${SOURCES} ${HEADERS}
EXECBIN  = hello

all : ${EXECBIN}

${EXECBIN} : ${OBJECTS}
	cc ${CCOPTS} ${OBJECTS} -o ${EXECBIN}

%.o : %.c
	cc ${CCOPTS} -c $<

ci : ${CHECKIN}
	cid + ${CHECKIN}

test : ${EXECBIN}
	./${EXECBIN} ; echo status = $$?

clean : 
	- rm ${OBJECTS}

spotless : clean
	- rm ${EXECBIN}

hello.o: hello.c hello.h
main.o: main.c hello.h

