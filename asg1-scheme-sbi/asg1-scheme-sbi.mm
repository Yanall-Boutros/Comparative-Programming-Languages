.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE CSE-112 Winter\~2020 Program\~1 "Functionally Scheme"
.tm title done
.RCS "$Id: asg1-scheme-sbi.mm,v 1.30 2020-01-09 14:09:20-08 - - $"
.PWD
.URL
.de RULE
.   IR \\$1 \|\|\[->]\|
.   shift
.   while \\n[.$] \{\
.      ds Prefix \f[I]
.      ds Suffix \f[R]
.      if '\\$1'{' .ds Prefix \f[R]
.      if '\\$1'}...' .ds Prefix \f[R]
.      if '\\$1'[' .ds Prefix \f[R]
.      if '\\$1']' .ds Prefix \f[R]
.      if '\\$1'|' .ds Prefix \f[R]
.      if '\\$1'@' \{\
.         ds Prefix `\f[CB]
.         ds Suffix \f[R]'
.         shift
.      \}
\|\\*[Prefix]\\$1\\*[Suffix]\&
.      shift
.   \}
.   br
..
.EQ
delim $$
.EN
.ds CSE-112-root /afs/cats.ucsc.edu/courses/cse112-wm
.ds Scheme-root \*[CSE-112-root]/usr/racket
.H 1 "Overview"
Scheme is a dynamically typed (mostly) functional language
with a very simple syntax.
In this assignment, you will write a Silly Basic language
interpreter in Scheme.
The interpreter will read in an intermediate language program,
parse it, and then interpret it.
No looping constructs may be used,
so it is critical that certain parts use proper tail-recursion
to avoid function call stack overflow.
.H 1 "A Silly Basic Interpreter"
.SH=BVL
.MANPAGE=LI "NAME"
sbi.scm \[em] a Silly Basic Interpreter
.MANPAGE=LI "SYNOPSIS"
.V= sbi.scm
.IR \|filename
.MANPAGE=LI "DESCRIPTION"
The SB interpreter reads in an SBIR program from the file whose
name is specified in the argument list,
stores it in a list,
and then interprets that intermediate representation.
During interpretation, numbers are read from the standard input
and results written to the standard output.
.P
Error messages are printed to the standard error.
The first error, whether during compilation or interpretation,
causes a message to be printed and the program to exit with
an exit code of 1.
.MANPAGE=LI "OPTIONS"
None.
.MANPAGE=LI "OPERANDS"
The single filename argument
specifies an SBIR program to be run.
.MANPAGE=LI "EXIT STATUS"
If the program completes without error, 0 is returned.
If not, 1 is returned.
.MANPAGE=LI "HISTORY"
BASIC (Beginner's All-purpose Symbolic Instruction Code)
was designed at Dartmouth College, NH,
by John Kemeny and Thomas Kurtz in 1965.
A variation of that language was ROM BASIC,
distributed by IBM on their original PC in 1980.
.P
Silly Basic is somewhat related.
This description of the Silly Basic programming language,
assumes that certain things are intuitively obvious.
There are only two data types in the language\(::
strings and numbers.
Strings are used only in
.V= print
statements.
There are no string variables.
Variables are floating point numbers.
.MANPAGE=LI "EWD498"
Edsger W.\& Dijkstra\(::
.ft I
``It is practically impossible to teach good programming to students
that have had a prior exposure to BASIC\(::
as potential programmers they are mentally mutilated beyond hope of
regeneration.''
.ft P
\[em]
EWD498.
.P
EWD manuscripts are archived at
.V= http://www.cs.utexas.edu/\[ti]EWD/ .
.MANPAGE=LI "THE SBIR LANGUAGE"
This is a top-down definition of the SBIR language,
specified using a variation of Backus-Naur Form (BNF),
the format used to specify Algol-60, yet another one of
the ancient languages.
In the metanotation, 
brackets indicate that what they enclose is optional,
braces indicate that what they enclose is repeated zero or more times,
and a bar indicates alternation.
Italics indicate nonterminal symbols and token classes,
while quoted courier bold indicates literal tokens.
.P
.ALX a ()
.LI
.RULE Program @ ( { @ ( Linenr [ Label ] [ Statement ] @ ) }... @ )
.P
A program consists of zero or more statements,
each of which
might be identified by a label.
Labels are kept in a name\%space separate from the
.IR Variable
namespace and do not conflict with each other.
The program terminates when control flows off the last statement.
A statement with neither a label nor a statement is considered
just a comment and not put into the statement list.
.LE
.MANPAGE=LI "STATEMENTS"
Statements are the only organizational structure in the language
and are executed one by one in sequence, 
except when a control transfer occurs.
There is no block structure or nesting.
.ALX a ()
.LI
.RULE Statement @ ( @ dim Arrayref @ )
.RULE Arrayref @ ( @ asub Variable Expression @ )
.P
The
.V= dim
statement creates an array given by
the variable name and inserts it into the array table,
replacing any previous array already in the array table.
The dimension of the array is given by the expression.
All values in the array are initialized to 0.
The expression is rounded to the nearest integer before being
used as the bound, which must be positive.
.LI
.RULE Statement @ ( @ let Memory Expression @ )
.RULE Memory Arrayref | Variable
.P
A
.V= let
statement makes an assignment to a variable.
The expression is first evaluated.
For a
.IR Variable ,
its value is stored into the Symbol table,
replacing whatever was there previously.
For an
.IR Arrayref ,
the store message is sent to the vector representing the array.
If the Symbol table entry is not an array,
an error occurs.
.LI
.RULE Statement @ ( @ goto Label @ )
.P
Control transfers to the statement referred to by the
.IR Label .
An error occurs if the
.IR Label
is not defined.
.LI
.RULE Statement @ ( @ if @ ( Relop Expression Expression @ ) Label @ )
.RULE Relop @ = | @ < | @ > | @ != | @ >= | @ <=
.P
The two
.IR Expression s
are compared according to the given
.IR Relop ,
and if the comparison is true,
control transfers to the statement, as for the
.V= goto
statement.
.LI
.RULE Statement @ ( @ print { Printable }... @ )
.RULE Printable String | Expression
.P
Each of the operands is printed in sequence, with a space
before
.IR Expression
values.
A newline is output at the end of the
.V= print
statement.
.V= print
statements are the only place
.IR String s
may
occur in SBIR.
.LI
.RULE Statement @ ( @ input Memory { Memory }... @ )
.P
Numeric values are read in and assigned to the input variables
in sequence.
Arguments might be elements of an array.
For each value read into a
.IR Memory ,
the value is
inserted into the Symbol table under that variable's key.
For arrays, the array must already exist and the subscript not
be out of bounds.
.P
If an invalid value (anything that is not a
.V= number? )
is read,
the value returned is
.V= nan .
If end of file is encountered,
the value returned is
.V= nan
and the variable
.V= eof
is entered into the symbol table with the value 1.
The value of
.V= nan
can be computed using the expression
.V= "(/ 0.0 0.0)" .
Counterintuitively, the expression
.V= "(= nan nan)"
is false.
.LE
.MANPAGE=LI "EXPRESSIONS"
Expressions consistitute the computational part of the language.
All values dealt with at the expression level are real numbers.
Invalid computations, such as division by zero and infinite results
do not cause computation to stop.
The value just propagates according to the rules of real
or complex arithmetic.
.ALX a ()
.LI
.RULE Expression @ ( Binop Expression Expression @ )
.RULE Expression @ ( Unop Expression @ )
.RULE Expression @ ( Function Expression @ )
.RULE Expression Constant
.RULE Expression Memory
.RULE Binop @ + | @ \- | @ * | @ / | @ \[ha]
.RULE Unop @ + | @ \-
.P
.IR Constant s
are numbers.
Names of
.IR Function s,
.IR Arrayref s,
and
.IR Variable s
all look like identifiers and their meaning is given by context.
.V= "(\[ha] a b)"
is exponentiation ($ a sup b $)
.LE
.MANPAGE=LI "LEXICAL SYNTAX"
.IR Comment s
being with a semi-colon and end at the end of a line.
.IR String s
are delimited by double-quote marks
.RB ( \[Dq] ).
.IR Number s
consist of digits, an optional decimal point, and an optional exponent.
Keywords and
.IR Variable
names are atoms.
All of this is taken care of by Scheme's builtin
.V= read .
.MANPAGE=LI "BUILTIN SYMBOLS"
In addition to the operators that are part of the language,
the following functions are part of the function table\(::
.V= abs ,
.V= acos ,
.V= asin ,
.V= atan ,
.V= ceiling ,
.V= cos ,
.V= exp ,
.V= floor ,
.V= log ,
.V= round ,
.V= sin ,
.V= sqrt ,
.V= tan ,
.V= truncate .
There is no facility for the user to add functions to the
function table.
The following are part of the initial variable table\(::
.V= nan \~=\~ "(/ 0.0 0.0)" \(;;
.V= eof \~=\~ 0.0 \(;;
.V= pi \~=\~ "(acos -1.0)" \(;;
.V= e \~=\~ "(exp 1.0)" .
.H 1 "Program Structure"
The program will be read in by Scheme's
.V= read
function,
and represented internally as a list of statements,
each statement having its own structure.
After reading in the program,
all labels must be put into a hash table,
the key being the label itself and the value being the
particular statement it refers to.
.P
Interpretation will then proceed down the list from the
first statement to the last.
The interpreter stops when it runs off the end of the list.
A control transfer is executed by fetching the address of
a statement from the label table.
.P
All variables are either real numbers or vectors of real numbers.
Another hash table is used whose keys are variable names
and whose values are real numbers, vectors of real numbers,
or single parameter functions.
An array subscript operation and a function call are
syntactically ambiguous,
but are disambiguated at run time by checking the symbol table.
An uninitialized variable should be treated as 0.
.P
Your program should not crash,
no matter what the input.
If a detectable unforseen condition happens due to user error,
a message should be printed,
giving the name of the file and the statement number.
.P
The usual arithmetic results for infinities are printed
by the runtime system, and these should be generated wherever
possible.
Division by zero, for example, should produce one of these
quantities
.=V ( +inf.0 ,
.V= -inf.0 ,
.V= +nan.0 ).
Add 0.0 to all input numbers to ensure that they 
are converted to real numbers.
Also look at the functions to see which ones need special treatment.
While there is no way to directly input a complex number,
some computations, such as
.V= (sqrt\~-1) ,
will produce them.
.P
You may ignore the directory
.V= src-sb ,
which contains source code and a translator from Basic to SBIR.
You may also ignore the directory
.V= sbtran ,
which contains the SB to SBIR translator itself,
written in Ocaml.
.H 1 "Functional Style"
Programming should be done in entirely functional style,
except for maintenance of the symbol tables.
That means do not use any imperative functions except as outlined
below.
In Scheme, imperative functions end with a bang
.=V ( ! )
to indicate that an assignment is being made.
Symbol tables are created with
.V= make-hash
and updated with
.V= hash-set! .
The symbol tables are as follows\(::
.ALX a ()
.LI
.V= *function-table*
is used to hold all of the functions, which include the operators.
This is initialized when the program begins using a
.V= for-each
loop containing a
.V= lambda .
(See the example
.V= symbols.scm ).
.LI
.V= *variable-table*
holds the value of all variables,
and is updated as needed during interpretation of the program.
Whenever a variable in the symbol table is not found,
the value 0 is returned.
The variable table is initialized with the variables described in
the section ``builtin symbols''.
.LI
.V= *array-table*
is used to hold all arrays defined in the program.
Arrays and variables are in separate namespaces.
Arrays are created with
.V= make-vector
and updated with
.V= vector-set! .
.LI
.V= *label-table*
is used to hold addresses of each line, one level up from statements.
This is initialized by scanning the list returned by
.V= (read)
when the program begins.
.LE
.P
Except for
.V= hash-set!
and
.V= vector-set!
as outlined above,
no imperative functions are permitted.
Use functional style only.
.H 1 "Pseudocode Outline"
The data structure consists of a recursively nested list\(::
.ALX a ()
.LI
The top level list consists of a sequence of lines.
Each line is pointed at by the
.V= car
of a cell in the top level list.
.LI
Each line consists of a line number,
an optional label, which is always a
.V= symbol? ,
and an optional statement, which is always a
.V= pair? .
Use
.V= null?
to determine whether not something exists.
Do not use
.V= list? .
.LI
A statement consists of a keyword followed by operands,
mostly expressions.
.LI
An expression uses prefix notation in standard Scheme format.
.LE
.P
A suggested outline and description of some of the functions follows\(::
.ALX a ()
.LI
After reading in the program,
make one pass over the top level,
checking for a label in each line.
Each label should be inserted into the label hash
with a pointer to the top level node (not the line).
.LI
Write a function
.V= interpret-program
takes the top level list as an argument and
checks to see if there is a statement.
.ALX i ()
.LI
If there is no statement,
call
.V= interpret-program
recursively with the
.V= cdr
of the top level node.
.LI
If there is a statement, look up the keyword in the
statement hash and call
.VI interpret- statement ,
where
.IR statement
is the keyword found in the statement.
.LI
This funcion should return null
for a statement that is not a control transfer,
or for a statement that is a control transfer that is not taken.
.LI
If this function returns a null then call
.V= interpret-program
recursively with the
.V= cdr ,
as explained above.
.LI
If this function is a successful control transfer,
it should return the label to which to transfer,
and then
.V= interpret-program
calls itself recursively with the associated line.
.LE
.LI
Write separate functions
.VI interpret- statement
for each one of the keyword in the language.
.LI
The function
.V= evaluate-expression 
is called by a statement interpreter.
.ALX i ()
.LI
It looks up the function in the function table.
.LI
It uses
.V= map 
to call
.V= evaluate-expression
for each of the arguments to the function.
.LI
Then use
.V= apply
to apply the function to the list of results obtained.
.LI
Subscripting arrays will require a special case.
.LE
.LE
.H 1 "Examples Directory"
.VTCODE* 0 \*[CSE-112-root]/Languages/scheme/Examples/
.H 1 "Running \f[CB]mzscheme\fP Interactively"
It will be very convenient for you to run
.V= mzscheme 
interactively for testing purposes simply by invoking it from
the command line, as in\(::
.TVCODE* 1 "-bash-1\[Do] " "mzscheme"
.TVCODE* 1 "Welcome to Racket v7.4."
.TVCODE* 1 "> " "(expt 2 128)
.TVCODE* 1 "340282366920938463463374607431768211456"
.TVCODE* 1 "> " "\[ha]D"
To do this, be sure to put it in your 
.V= \[Do]PATH .
This can be done by putting the following lines in your
.V= \&.bash_profile\(::
.VTCODE* 1 "export PATH=\[Do]PATH:\*[CSE-112-root]/usr/racket/bin
Of course, you may prefer to collapse these multiple shell
commands into a single line.
If you use a different shell,
then setting your
.V= \[Do]PATH
will be done differently.
.P
To use the arrow keys on the keyboard to edit previous lines
in interactive mode,
put the following line in a file
.V= \[Do]HOME/.racketrc \(::
.VTCODE* 1 "(require readline)"
Or, after starting
.V= mzscheme ,
enter this command before any other interaction.
.H 1 "What to Submit"
Submit two files\(::
.V= README
and
.V= sbi.scm .
It must be runnable by using it as the command word of any
shell command, and hence the execute bit must be turned on
.=V ( "chmod +x" ).
It will be run as a shell script,
and hence the first line must be the following hashbang\(::
.VTCODE* 1 "#!\*[Scheme-root]/bin/mzscheme -qr"
.P
Make sure that the Unix command
.V= "which mzscheme"
responds with the same executable.
Important note\(::
This must be the
.E= first
line in your script, and your id should be after it.
Be sure there are no carriage return characters in the file.
.P
If you are doing pair programming, one partner should submit
.V= sbi.scm ,
but both should submit the
.V= README
and
.V= PARTNER
files, as specified in the pair programming guidelines.
If you are
.E= not
pair programming,
read the section ``Solo programming''.
.P
Be sure to use
.V= checksource
to verify basic formatting.
This script, and other scripts,
such as
.V= cid
and
.V= elimcr ,
are in
.VTCODE* 1 \*[CSE-112-root]/bin
which should also be put in your
.V= \[Do]PATH
environment variable.
.P
The
.V= .score/
subdirectory contains instructions to the graders.
Be sure your program runs with the test script.
If your program runs when typed in manually from the command line,
but not using the script,
you will receive no points for execution and testing.
