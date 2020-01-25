.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE CSE-112 Winter\~2020 Program\~2 "Interpreter in Ocaml"
.tm title done
.RCS "$Id: asg2-ocaml-interp.mm,v 1.24 2020-01-17 14:33:17-08 - - $"
.PWD
.URL
.de V=BLI
.   LI "\f[CB]\\$[1]\f[P]\\$[2]\f[CB]\\$[3]\f[P]"
.   br
..
.ds CSE-112-root /afs/cats.ucsc.edu/courses/cse112-wm
.ds PROFILE \[Do]HOME/.bash_profile
.H 1 "Overview"
This project will repeat the Silly Basic interpreter,
except this time the program will be written in Ocaml
but with Silly Basic programs untranslated from the original.
See the
.V= \&.score/
directory for sample input files.
Output should be the same as for the Scheme version of the program,
except for minor variations in output due to differences between
the Scheme and Ocaml languages.
Any results which would produce a complex value in Scheme 
should produce
.V= nan
in this project.
.H 1 "Running \f[CB]ocaml\f[P] interactively"
Ocaml may be run interactively from the command line or as
a compiled program.
The compiled program version,
created using
.V= make
is required for all submitted programs.
.P
To run 
.V= ocaml
interactively,
add the following to your
.V= \*[PROFILE] \(::
.VTCODE* 1 "export PATH=$PATH:\*[CSE-112-root]/usr/ocaml/bin"
When running
.V= ocaml
interactively, use the command
.V= rlwrap
to gain access to the readline arrow keys so that you
can recover earlier typed lines.
Example\(::
.TVCODE* 1 "-bash-$ " "rlwrap ocaml"
.TVCODE* 1 "        OCaml version 4.02.1"
.TVCODE* 1 "# " "let f x y = x +. y;;"
.TVCODE* 1 "val f : float -> float -> float = <fun>"
.TVCODE* 1 "# " "f 3.;;"
.TVCODE* 1 "- : float -> float = <fun>"
.TVCODE* 1 "# " "f 3. 4.;;"
.TVCODE* 1 "- : float = 7."
.TVCODE* 1 "# " "\[ha]D"
To simplify typing, the following line might be added to your
.V= \*[PROFILE] \(::
.VTCODE* 1 "alias ocaml=\[Dq]rlwrap ocaml\[Dq]"
The suggestions above assume you are using
.V= bash
as your login shell.
If not, use the syntax appropriate for whatever shell you are using.
.P
Some files that are useful when running interactively are\(::
.BVL \n[Pi]
.V=BLI using
A set of 
.V= #use
directives which can be used for interactive testing of the
functions.
This file is not used in compilation.
After starting Ocaml, 
type in the following command to load your source code
interactively\(::
.VTCODE* 1 "#use \[Dq]using\[Dq];;"
.V=BLI \&.ocamlinit
As an alternative to the
.V= using
file, create the file
.V= \&.ocamlinit
containing the same information.
The file
.V= \&.ocamlinit
in the current directory is automatically sourced when
.V= ocaml
starts.
.LE
.P
As an alternative, start up
.V= ocaml
with the line
.VTCODE* 1 "rlwrap ocaml -init using"
which will start up the init file when needed,
but avoid the automatic startup when you don't want it.
If you have a
.V= \&.ocamlinit
and want to ocassionally suppress it,
you can use
.VTCODE* 1 "rlwrap ocaml -init /dev/null"
.H 1 "Source code"
The following files and modules are provided in the
.V= code/
subdirectory\(::
.BVL \n[Pi]
.V=BLI etc.mli ", " etc.ml
Interface and implementation of the
.V= Etc
module,
which contains miscellaneous functions not specifically tied to
other purposes.
.V=BLI absyn.mli
Definition of the abstract syntax used by the interpreter.
No implementation file is needed.
.V=BLI tables.mli ", " tables.ml
Module for maintaining the five tables needed by the program.
The interface file is automatically generated from the implemenation,
not entered manually.
The required tables and their types are\(::
.BVL \n[Pi] "" 1
.V=BLI label_table
Labels with pointers to the list of program statements.
.VTCODE* 0 "type label_table_t = (string, Absyn.program) Hashtbl.t"
.V=BLI unary_fn_table
The unary functions.
.VTCODE* 0 "type unary_fn_table_t = (string, float -> float) Hashtbl.t"
.V=BLI binary_fn_table
The binary functions.
.VTCODE* 0 \
"type binary_fn_table_t = (string, float -> float -> float) Hashtbl.t"
Because Ocaml is strongly typed,
the unary and binary functions need to be be in separate tables.
.V=BLI variable_table
The simple variables used by the program.	
.VTCODE* 0 "type variable_table_t = (string, float) Hashtbl.t"
.V=BLI array_table
The arrays used by the program.
.VTCODE* 0 "type array_table_t = (string, float array) Hashtbl.t"
.LE
.V=BLI interp.mli ", " interp.ml
The interface and implementation of the interpreter.
This is the major project of this program and must be 
extensively modified.
.V=BLI main.ml
The main function which behaves differently,
depending on whether the program is run interactively or 
from the command line.
Does the parsing to create the abstract syntax structure,
then calls the interpreter.
.V=BLI parser.mly
The parser reads a Silly Basic program,
verify syntax, and create the abstract syntax.
Specifies the exact syntax of the language.
.V=BLI scanner.mll
The lexical specification for the language,
and reads tokens from the source file.
.V=BLI Makefile
Since the Ocaml
project is compiled into a binary executable,
as is C++,
a 
.V= Makefile
is needed, as is required in any C, C++, or Java project.
.LE
.H 1 "What to submit"
Submit all of the necessary source files so that the grader
may perform the build.
That means submit
.V= Makefile ,
.V= parser.mly ,
.V= scanner.mll ,
and all
.V= *.mli
and
.V= *.ml
files.
If you are doing pair programming,
also submit the files required by the
.V= pair-programming
document.
Verify the grading criteria from the
.V= \&.score/
subdirectory.
.FINISH
