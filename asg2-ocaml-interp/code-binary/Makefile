# $Id: Makefile,v 1.15 2020-01-24 12:54:40-08 - - $

#
# General useful macros
#

MKFILE     = Makefile
MAKEFLAGS += --no-builtin-rules
DEPSFILE   = ${MKFILE}.deps
NOINCLUDE  = ci clean spotless
NEEDINCL   = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
GMAKE      = ${MAKE} --no-print-directory
OCAMLOPT   = ocamlopt -g

#
# File macros
#

EXECBIN    = sbinterp
OBJCMX     = etc.cmx parser.cmx scanner.cmx tables.cmx \
             dumper.cmx interp.cmx main.cmx
OBJCMI     = ${OBJCMX:.cmx=.cmi} absyn.cmi
OBJBIN     = ${OBJCMX:.cmx=.o}
MLSOURCE   = absyn.mli etc.mli etc.ml tables.mli tables.ml \
             dumper.mli dumper.ml interp.mli interp.ml main.ml
GENLEXYACC = parser.mli parser.ml scanner.ml
GENSOURCE  = dumper.mli tables.mli ${GENLEXYACC}
GENFILES   = ${GENSOURCE} parser.output ${DEPSFILE}
OTHERFILES = ${MKFILE} ${DEPSFILE} using .ocamlinit
ALLSOURCES = ${MLSOURCE} parser.mly scanner.mll ${OTHERFILES}
LISTING    = Listing.ps

#
# General targets
#

all : ${EXECBIN}

${EXECBIN} : ${OBJCMX}
	${OCAMLOPT} str.cmxa ${OBJCMX} -o ${EXECBIN}

%.cmi : %.mli
	${OCAMLOPT} -c $<

%.cmx : %.ml
	${OCAMLOPT} -c $<

%.ml : %.mll
	ocamllex $<

%.mli %.ml : %.mly
	ocamlyacc -v $<


MAKEMLI    = (echo "(* Generated: $$(date) *)"; ${OCAMLOPT} -i $<) >$@

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

