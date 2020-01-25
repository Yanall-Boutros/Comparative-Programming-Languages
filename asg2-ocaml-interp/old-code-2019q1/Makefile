# $Id: Makefile,v 1.12 2020-01-17 14:21:56-08 - - $

#
# General useful macros
#

MKFILE     = Makefile
MAKEFLAGS += --no-builtin-rules
DEPSFILE   = ${MKFILE}.deps
NOINCLUDE  = ci clean spotless
NEEDINCL   = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
GMAKE      = ${MAKE} --no-print-directory

#
# File macros
#

EXECBIN	   = sbinterp
OBJCMX	   = etc.cmx parser.cmx scanner.cmx tables.cmx \
             dumper.cmx interp.cmx main.cmx
OBJCMI	   = ${OBJCMX:.cmx=.cmi} absyn.cmi
OBJBIN	   = ${OBJCMX:.cmx=.o}
MLSOURCE   = absyn.mli etc.mli etc.ml tables.mli tables.ml \
             dumper.mli dumper.ml interp.mli interp.ml main.ml
GENSOURCE  = dumper.mli tables.mli parser.mli parser.ml scanner.ml
GENFILES   = ${GENSOURCE} parser.output ${DEPSFILE}
OTHERFILES = ${MKFILE} ${DEPSFILE} using .ocamlinit
ALLSOURCES = ${MLSOURCE} parser.mly scanner.mll ${OTHERFILES}
LISTING    = Listing.ps

#
# General targets
#

all : ${EXECBIN}

${EXECBIN} : ${OBJCMX}
	ocamlopt str.cmxa ${OBJCMX} -o ${EXECBIN}

%.cmi : %.mli
	ocamlopt -c $<

%.cmx : %.ml
	ocamlopt -c $<

%.ml : %.mll
	ocamllex $<

%.mli %.ml : %.mly
	ocamlyacc -v $<


MAKEMLI    = (echo "(* Generated: $$(date) *)"; ocamlopt -i $<) >$@

tables.mli : tables.ml absyn.cmi
	${call MAKEMLI}

dumper.mli : dumper.ml absyn.cmi
	${call MAKEMLI}

#
# Misc targets
#

clean :
	- rm ${OBJCMI} ${OBJCMX} ${OBJBIN} ${GENSOURCE}

spotless : clean
	- rm ${EXECBIN} ${GENFILES} ${LISTING} ${LISTING:.ps=.pdf} 

ci : ${ALLSOURCES}
	- checksource ${ALLSOURCES}
	cid + ${ALLSOURCES}

deps : ${MLSOURCE} ${GENSOURCE}
	@ echo "# Generated: $$(date)" >${DEPSFILE}
	ocamldep ${MLSOURCE} ${GENSOURCE} >>${DEPSFILE}

${DEPSFILE} : tables.mli
	@touch ${DEPSFILE}
	${GMAKE} deps

lis : ${ALLSOURCES}
	mkpspdf ${LISTING} ${ALLSOURCES}

again :
	${GMAKE} spotless
	${GMAKE} deps
	${GMAKE} ci
	${GMAKE} all
	${GMAKE} lis

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif

