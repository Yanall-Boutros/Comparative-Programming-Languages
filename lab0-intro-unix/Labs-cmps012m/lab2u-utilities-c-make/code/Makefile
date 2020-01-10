# $Id: Makefile,v 1.1 2015-01-13 15:48:01-08 - - $

SOURCES   = hello.c
OBJECTS   = ${SOURCES:.c=.o}
ALLSRCS   = ${SOURCES} Makefile
EXECBIN   = hello

GCC       = gcc -g -O0 -Wall -Wextra -std=gnu11

all: ${EXECBIN}

${EXECBIN}: ${OBJECTS}
	${GCC} ${OBJECTS} -o ${EXECBIN}

%.o : %.c
	- checksource $<
	cid + $<
	${GCC} -c $< -o $@

test: ${EXECBIN}

