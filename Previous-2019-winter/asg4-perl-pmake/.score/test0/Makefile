# $Id: Makefile,v 1.3 2018-09-28 15:41:48-07 - - $

# Note: "echo all" is echoed, but echo of "foo" and "bar" are not.

all : foo bar
	echo all - command '*IS*' echoed

foo :
	@ echo foo - command not echoed

bar :
	@ echo bar - command not echoed

