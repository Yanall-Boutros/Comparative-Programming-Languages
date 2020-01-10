.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE CMPS-012M Winter\~2016 Lab\~2 "Utilities, C, make"
.RCS \
"$Id: lab2u-utilities-c-make.mm,v 1.70 2016-01-06 16:29:20-08 - - $"
.PWD
.URL
.ds SUBQUARTER cmps012b-wm.w15
.ds COURSEDIR /afs/cats.ucsc.edu/courses/cmps012b-wm
.ds COURSEURL http://www2.ucsc.edu/courses/cmps012b-wm/:/
.ds BINDIR \*[COURSEDIR]/bin
.nr HISTORY 0 1
.af HISTORY 01
.de PROMPT
.   TVCODE* 1 "bash-\\n+[HISTORY]\[Do] " "\\$*"
..
.de OUTPUT
.   TVCODE* 1 "\\$*"
..
.de SUBMIT
.   P
.   nop Submit\(::
.   V= "\\$*"
..
.EQ
delim $$
.EN
.H 1 "Overview"
In this lab you will continue being familiarized with Unix,
looking at some utilities\(::
.V= find (1),
.V= grep (1),
.V= man (1),
.V= du (1),
etc.
We will also introduce the C language and the
.V= make 
utility.
.H 1 "The \f[CB]man\fP(1) command"
The
.V= man (1)
command can be used to read about any Unix command,
C language function,
or other Unix utility.
When referenceing a man-page,
following the name is a number in parentheses which identifies
the chapter in question.
Following are the sections and what they describe\(::
.nr ALX*Li \n[Li]-1
.LB 0 0 0 0 \0 0 0
.LI
(1) General commands
.LI
(2) System calls
.LI
(3) Library functions, covering in particular the C standard library
.LI
(4) Special files (usually devices found in \f[CB]/dev\fP) and drivers
.LI
(5) File formats and conventions
.LI
(6) Games and screensavers
.LI
(7) Miscellanea
.LI
(8) System administration commands and daemons
.LE
.H 1 "The \f[CB]find\fP(1) and \f[CB]grep\fP(1) commands"
The
.V= find (1)
command can be used to find files in directory hierarchies
according to various options.
The general syntax is
.br
.nr VCODEN \n[VCODENWIDTH]u*1
\h'\n[VCODEN]u'\f[CB]find\fP [\fIpath\fP\|.\|.\|.]
[\fIoperand\fP\|.\|.\|.]
.br
The pathnames specified identify one or more directories.
There are many operands, some of which are\(::
.DS
.TS
allbox tab(|); l lw(349p).
\f[CB]-name\fP \fIwildcard\fP|T{
.fi
Finds files according to the standard shell wildcard expressions,
with case being significant.
T}
\f[CB]-iname\fP \fIwildcard\fP|T{
.fi
Finds files according to the standard shell wildcard expressions,
with case not being significant.
T}
\f[CB]-mtime\fP \fIdays\fP|T{
.fi
Finds files according to the time stamp, measuread in days.
For example 
.V= +3
means more than 3 days,
.V= -3
means less than 3 days,
and 
.V= 3
means exactly 3 days.
T}
\f[CB]-size\fP \fIunits\fP|T{
.fi
Finds files according to their size.
For example if the units are specified as
.V= +200k ,
any files larger than that will be found.
T}
.TE
.DE
.P
The
.V= grep (1)
command is used to search a sequence of files,
given a regular expression.
As long as metacharaters are not used,
it can be used to search for strings.
Regular expressions are somewhat more complicated.
.H 1 "Pipes"
A pipe is a connection between the output of one process and the
input of another process and can be used to connect processes
together into a pipeline.
For example,
.V= ls (1)
lists information about files,
and
.V= wc (1)
prints the number of lines, words, and characters in its input.
Therefore\(::
.PROMPT ls | wc -l
will count the number of (non-dot) files in your current directory.
.PROMPT ls -la | grep 'Jan 13'
will list the output of
.V= ls
for all files modified on January 13.
.H 1 "Lab exercises"
Each of the following items will require something to be
submitted for credit in this lab.
It is assumed that the previous lab has been completed.
.ALX 1 ()
.LI
Read the man page for 
.V= find (1)\(::
.PROMPT man -s 1 find
Then redirect the output into a file called
.V= man.1.find ,
and note that if you view it with an editor,
you will see a lot of backspace characters represented as
.=V `` \[ha]H ''
characters.
On a text terminal, this would overstrike to make some characters
bold.
Clean this up by using\(::
.PROMPT vim man.1.find
Then use the
.V= vim
line-mode command
.VTCODE* 1 ":g/.\[ha]V\[ha]H/s///g"
which will clean it up and make it readable as text.
Note that
.=V `` \[ha]V\[ha]H ''
means type
.=V `` vh ''
while holding down the Control key.
.SUBMIT man.1.find
.LI
Repeat this exercise for the
.V= grep (1)
command.
.SUBMIT man.1.grep
.LI
Use the
.V= find
command to locate all PDF files in my 
.V= Assignments/
and
.V= Labs-cmps012m/
directories.
.PROMPT find \[ti]/12b/[AL]* -name '*.pdf'
Note that the first few operands are directories
(expanded via wildcards)
which specify directories,
and the
.V= -name
operand specifies wildcards for the names of files to match.
Redirect this into a file called
.V= asgs-labs-found
.SUBMIT asgs-labs-found
.LI
Use the
.V= grep (1)
command to search for the string
.=V `` Submit: ''
in all of the lab directories for all files with the suffix
.V= \&.tt \(::
.PROMPT grep Submit: lab*/*.tt
Note that you have to
.V= cd
into the directory
.V= Labs-cmps012m/
for this to work.
Redirect the output of this command into a file called
.V= grepped-submits .
Note that you can not create this file in the course directory,
so you will have to specify a pathname which
deposits this file in one of your directories.
.SUBMIT grepped-submits
.LI
The
.V= du (1)
(disk usage)
command performs a recursive traversal over all directories and
prints out the amount of disk space taken by each directory.
This is useful in finding out if you are using up too much disk space.
The command
.V= "fs lq"
lists your disk quota.
.PROMPT fs lq \[ti]
.PROMPT du -k \[ti]/private/cmps012b
Print out your current disk quota and disk usage for your 12B and 12M
directories.
(If you have named your 12B and 12M working directories something else,
use the actual name if different from above.)
The
.V= -k
option specifies that disk usage should be measured in K-bytes
($ 1 K = 2 sup 10 = 1024 $).
Redirect the output of both of these commands into a file called
.V= disk-usage .
.SUBMIT disk-usage
.P
Note that to append to the end of a file you use
.V= >>
instead of
.V= > .
For example,
the following will create a file called
.V= foo
with your current working directory followed by the date\(::
.br
.ne 2
.PROMPT pwd >foo
.PROMPT date >>foo
.LE
.H 1 "Introduction to C"
Copy the program
.V= code/hello.c
into your directory.
Modify the program to conform to the specifications of
.V= hello.java
in the previous lab.
.ALX 1 ()
.LI
The program argument
.V= argc
counts the number of command line arguments given to the program.
.LI
The program argument
.V= argv
is a pointer to an array of pointers to character string,
where
.V= argv[0]
is the name of the program itself.
.LI
At the top of
.V= main ,
add an
.V= int
variable called
.V= exit_status
which is initialized to the value
.V= EXIT_SUCCESS .
.LI
If there are no command line arguments, 
.V= argc
should have the value
.V= 1 .
If that is the case,
print the message
.VTCODE* 1 "Hello, World!"
as is done in the starter code.
.LI
If not,
print the message
.VTCODE* 1 "Usage: hello"
to
.V= stderr .
.V= fprintf (3)
works like
.V= printf (1),
except that its first argument should be
.V= stderr ,
followed by the format.
.LI
The program name should not be hard-coded.
instead use the expression
.VTCODE* 1 "basename (argv[0])"
which obtains the name of the program without the directory
information.
.LI
In the case of failure,
set
.V= exit_status
to 
.V= EXIT_FAILURE ,
and return that value as the value of the program.
.LE
.SUBMIT hello.c
.H 1 "Introduction to \f[CB]make\fP"
A
.V= Makefile
is used to build software from specifications.
Copy
.V= code/Makefile
into your lab2 project directory and rename it to
.V= Makefile .
(Do not copy the file
.V= Makefile
in this directory.
It builds the PDF from the groff sources.)
.ALX 1 ()
.LI
The first line contains an RCS
.V= \[Do]Id\[Do]
header in a comment,
which starts with
.V= # ,
not a double slash.
.LI
Then some variables are defined,
with
.V= OBJECTS
being the same as
.V= SOURCES ,
except for the suffix.
.LI
Note the options given to
.V= gcc ,
which is used to compile C code.
.LI
Then there are dependencies which compile the source into object files,
and link the object files into an executable binary.
.LI
By analogy we might consider object files to be similar to class files,
and the executable image similar to a jar.
Analogies only work so far then fail.
.LI
Add a target
.V= test
to the make file and put commands after it.
Each command must be indented by one tab.
The test should do the following\(::
.ALX i ()
.LI
Run
.V= hello
and redirect its output to
.V= test1.out ,
its error to
.V= test1.err ,
then
.E= "on the same line" ,
redirect its exit status to
.V= test1.status .
Note that the
.V= hello
command and the
.V= echo 
command must be on the same line separated by a semi-colon.
.LI
Run
.V= hello
with an argument of
.V= world
and redirect its
.V= stdout
and
.V= stderr
as before but call the files
.V= test2.out ,
.V= test2.err ,
and
.V= test2.status .
.LE
.LI
In a
.V= Makefile ,
the dollar sign always introduces a variable whose value is
substituted.
This can be done either with a single character variable name
or multiple characters in braces.
So, for example, 
.V= \[Do]<
.V= \[Do]@
and
.V= \[Do]{FOOBAR}
are all variables to be substituted.
So
.V= \[Do]?
substitues the value of the variable
.V= ? ,
which, unless defined, is replaced by nothing.
The variable
.V= \[Do]\[Do]
has the value of 
.=V ` \[Do] ',
so to send
.V= \[Do]?
to the shell,
write
.V= \[Do]\[Do]?
in the
.V= Makefile .
.LE
.SUBMIT Makefile
.H 1 "Example \f[CB].bashrc\f[P]"
An example
.V= \&.bashrc
file is also shown.
This is just an example and there is nothing to submit
for this paragraph.
.DS
\&
.B1
.SP
.ft CR
.nf
.eo
.pso cat -n code/hello.c | expand
.ec
.fi
.ft R
.SP
.B2
.FG "\f[CB]code/hello.c\fP" "" 0 ELIMCR_BASH
.DE
.DS
\&
.B1
.SP
.ft CR
.nf
.eo
.pso cat -n code/Makefile| expand
.ec
.fi
.ft R
.SP
.B2
.FG "\f[CB]code/Makefile\fP" "" 0 ELIMCR_BASH
.DE
.DS
\&
.B1
.SP
.ft CR
.nf
.eo
.pso cat -n example.bashrc | expand
.ec
.fi
.ft R
.SP
.B2
.FG "\f[CB]example.bashrc\fP" "" 0 ELIMCR_BASH
.DE
.FINISH
