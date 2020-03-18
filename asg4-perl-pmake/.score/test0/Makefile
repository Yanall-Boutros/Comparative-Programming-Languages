# $Id: Makefile,v 1.1 2020-02-03 15:26:36-08 - - $

# Note: "echo all" is echoed, but echo of "foo" and "bar" are not.

all : foo bar
	echo all - command 'IS' echoed

foo :
	@ echo foo - command not echoed

bar :
	@ echo bar - command not echoed

