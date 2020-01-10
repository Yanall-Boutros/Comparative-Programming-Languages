.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE CMPS-012M Winter\~2016 Lab\~0 "Introduction to Unix"
.RCS "$Id: lab0u-intro-unix.mm,v 1.79 2015-12-07 14:37:46-08 - - $"
.PWD
.URL
.ds SUBQUARTER cmps012b-wm.w16
.ds SUBMITDIR /afs/cats.ucsc.edu/class/\*[SUBQUARTER]/lab0/\[Do]USER
.ds COURSEDIR /afs/cats.ucsc.edu/courses/cmps012b-wm
.ds COURSEURL http://www2.ucsc.edu/courses/cmps012b-wm/:/
.ds BINDIR \*[COURSEDIR]/bin
.nr HISTORY 0 1
.af HISTORY 01
.ds UNIXHOST unix.ucsc.edu
.de LI=file
.   LI
.   ds file \\n[li*cnt!\\n[li*lvl]]_\\$1
..
.de PROMPT
.   TVCODE* 1 "bash-\\n+[HISTORY]\[Do] " "\\$*"
..
.de OUTPUT
.   TVCODE* 1 "\\$*"
..
.de SUBMIT
.   ie \\n[.$]=0 .PROMPT submit \*[SUBQUARTER] lab0 \\*[file]
.   el           .PROMPT submit \*[SUBQUARTER] lab0 \\$*
..
.H 1 "Overview"
This lab will introduce you to Unix (Linux)
and basic commands used to navigate around the file system.
You will learn how to submit assignments using the
.V= submit 
command on Unix.
File and directory security and organization will also be
covered, 
specifically for the AFS file system.
Interacting with Unix is done by typing commands at the shell
prompt.
We will discuss only the
.V= bash 
shell.
.H 1 "Directories and ACLs"
Organize your work by creating a separate directory for each 
course you take,
and a separate subdirectory under that for each assignment or lab that
you work on.
In the following commentary,
what the computer types out is shown in Courier plain type,
and what the user types in is shown in Courier bold type.
.DS
.PROMPT cd
.PROMPT mkdir private
.DE
The command
.V= cd
without arguments sets the current directory to
.V= \[Do]HOME ,
which can also be referred to by a tilde 
.=V ( \[ti] ).
The
.V= mkdir
command creates a new directory.
An error will be printed if you already have the directory.
.DS
.PROMPT fs setacl private $USER all -clear
.PROMPT fs listacl private
.OUTPUT Access list for private is
.OUTPUT Normal rights:
.OUTPUT "  foobar rlidwka"
.DE
This has set an access control list (ACL) on the private directory,
which limits access to the user
.V= foobar .
The environment variable
.V= \[Do]USER
always contains your user name.
Any directories created under this directory will inherit
the ACL from its parent.
An ACL may be placed only on a directory,
never on an individual file.
The letters
.V= rlidwka
show which access rights are available to which user\(::
.nr Txtindent \n[Pi]*7/2
.de acl_code
.   LI "\f[CB]\\$1\0\f[P](\\$2)"
..
.VL \n[Txtindent] \n[Pi] 1
.acl_code r read
allows read access to the files in the directory.
.acl_code l list
allows
.V= ls (1)
to list the names of the files in the directory.
.acl_code i insert
allows new files to be inserted into the directory.
.acl_code d delete
allows files to be deleted from the directory.
.acl_code w write
allows writing (updating) to files in the directory.
.acl_code k lock
allows files in the directory to be locked.
.acl_code a admin
allows administrative
.=V ( fs\~setacl )
access to the directory.
.LE
.P
If an access right is given as
.V= "system:authuser rl" ,
it means that any authorized user may read files and list directories.
This is usually what is placed on course directories,
such as the one you are looking at now.
.P
If you are working from off campus,
you will need to connect to the servers
.V= unix.ucsc.edu .
Read 
.IR "``Unix Timeshare\(::"
.IR "How to Connect''" \(::
.VTCODE* 1 \
"http://its.ucsc.edu/unix-timeshare/tutorials/how-to-connect.html"
When you are in the Unix lab,
you do not need to use the servers.
.H 1 "Lab exercises"
For this lab,
you will be asked to create files and submit them using the
.V= submit
command.
.ALX 01 ()
.LI=file private-lab0
You should have a separate directory for each course and a subdirectory
of that for each lab or programming project.
Create one for lab0.
The shell variable
.V= \[Do]HOME
and the tilde
.=V ( \[ti] )
both refer to your home directory.
.PROMPT cd \[ti]/private
.PROMPT mkdir -p cmps012b/lab0
.PROMPT cd cmps012b/lab0
The
.V= pwd (1)
command shows you which directory is your current directory.
.PROMPT pwd
The 
.V= ls (1)
command shows you the contents of a directory.
The
.V= -l
option produces output in long format and
.V= -a
shows hidden files as well.
.PROMPT ls -la
.PROMPT pwd >\*[file]
.PROMPT ls -la >>\*[file]
.PROMPT cat \*[file]
.SUBMIT
.LI=file date
Create a file called \*[file] by redirecting the output of the
.V= date (1)
command.
Then submit it.
.PROMPT date
.PROMPT date >\*[file]
.PROMPT cat \*[file]
.SUBMIT
Note that the first command prints to the terminal,
while the second one redirects the output to a file.
.LI=file acl
Verify that you have properly set security on your private file
hierarchy.
Also check your disk quota.
.PROMPT fs listacl \[ti]/private
.PROMPT fs listquota
.PROMPT fs listacl \[ti]/private >\*[file]
.PROMPT fs listquota >>\*[file]
.PROMPT cat \\*[file]
.SUBMIT
Note that
.V= >
redirects output to a file, but
.V= >>
appends output to an existing file.
You can abbreviate the first operand of
.V= fs \(::
.V= fs\~sa ,
.V= fs\~la ,
and
.V= fs\~lq .
.LI=file which_shell
Verify that you are using 
.V= bash
and not
.V= tcsh .
.PROMPT echo $0
.OUTPUT tcsh
.PROMPT bash
.PROMPT echo $0
.OUTPUT -bash
.PROMPT echo $0 >\*[file]
.SUBMIT
So far, there have been no differences between the two shells,
but later on in the course, there will be some little difficulties
with
.V= tcsh .
If the first 
.V= echo
command displays
.V= bash ,
then you are already using
.V= bash .
If you wish to change your shell to 
.V= bash
permanently,
type the command
.V= chsh
and follow instructions.
Whether or not you change your shell is up to you,
but there are some advantages to using
.V= bash
as opposed to
.V= tcsh .
.LI=file getent
The
.V= getent (1)
command shows you information about your own username.
.PROMPT getent passwd \[Do]USER
.PROMPT getent passwd \[Do]USER >\*[file]
.SUBMIT
Its seven fields show the username,
what used to be the password field (but is no longer used),
your numeric userid,
your numeric group number (which is the same for everyone),
your actual name,
your home directory,
and
your default shell.
.LI=file .bashrc
The file
.V= \[ti]/.bashrc
in your home directory controls how 
.V= bash
starts up.
It is not created by default,
so using your favorite editor, create a file called
\&.bashrc
in your home directory with the following line\(::
.VTCODE* 1 "alias sub12b=\[Dq]submit \*[SUBQUARTER]\[Dq]"
Every time you start bash from the command line,
all commands in this file are executed.
To source it the first time
.PROMPT source \[ti]/.bashrc
This is done automatically every time you start
.V= bash .
.SUBMIT \[ti]/.bashrc
You do not have to source this file the next time you log in.
.LI=file .bash_profile
Also create a file
.V= \[ti]/.bash_profile
with the line
.VTCODE* 1 "source \[Do]HOME/.bashrc
This will cause your
.V= \&.bashrc
to be sourced whether you start it as a login shell or subshell.
.PROMPT "sub12b lab0 .bashrc .bash_profile
You can now abbreviate the 
.V= submit
command.
.LI=file verifying
To verify what you have submitted, use the command
.PROMPT ls -la \*[SUBMITDIR]
You may submit files as many times as you like before the due date.
When you do submit files, 
they are prefixed with sequence numbers.
So, for example,
if you submit
.V= foo
four times, you will see
.V= 1_foo ,
.V= 2_foo ,
.V= 3_foo ,
.V= 4_foo ,
in the submit directory.
Always verify what you have submitted.
If you are not sure of what you submitted, submit again.
Type this
.V= ls
command again and redirect output into a file called
.V= \*[file] .
.SUBMIT
.LI=file goodbye.java
Write a program in Java which prints the message
.V= "Goodbye, Java"
to the standard error
.=V ( System.err )
and then uses
.V= System.exit
to exit with exit status 1.
.PROMPT javac goodbye.java
.PROMPT java goodbye
.OUTPUT "Goodbye, Java."
.PROMPT echo \[Do]?
.OUTPUT 1
.SUBMIT goodbye.java
.LI=file mk.goodbye
Create a shell script that compiles, runs, and prints the
exit status of this program\(::
.PROMPT cat mk.goodbye
.OUTPUT #!/bin/bash
.OUTPUT javac goodbye.java
.OUTPUT java goodbye
.OUTPUT echo \[Do]?
.PROMPT chmod +x mk.goodbye
.SUBMIT mk.goodbye
This final program is a shell script which automates a small task.
The
.V= x -bit
must be turned on for it to work.
.LE
.P
This lab has asked you to submit 10 files.
Verify that they are all submitted.
.H 1 "Naming files"
.ALX 1 ()
.LI 
Filenames should be spelled using only lower case letters,
digits, periods, and underscores or minus signs.
Upper case letters in filenames are generally to be avoided,
except for special names such as 
.V= Makefile
and 
.V= README ,
which must be spelled exactly that way.
Commands like
.V= ls (1)
sort filenames lexicographically and thus list capitalized names before
lower case names.
The following non-alphanumeric characters generally to not cause
problems when used in filenames\(::
.VTCODE* 1  "% + , - . : = @ _"
.LI 
Shell metacharacters are prohibited in filenames.
A slash 
.=V ( / )
is a directory separator,
so Unix will not let you use it in a filename, even if you quote it.
Following is a list of shell metacharacters\(::
.VTCODE* 1  \
"! \[Dq] # \[Do] & ' ( ) * / ; < > ? [ \[rs] ] \[ha] ` { | } \[ti]"
.LI
The tilde
.=V ( \[ti] ) 
only has special meaning when it is the first character in a filename,
in which case it causes username interpolation.
.LI
The plus
.=V ( + )
and minus
.=V ( - )
characters
should never appear as the first character of a filename,
since they also typically introduce command-line options.
.LI
Be careful about using dot
.=V ( . )
as the first character because that makes files ``hidden''.
The term ``dotfile'' is often used to refer to such a file.
Control files such as
.V= \&.bashrc
usually begin with a dot.
.LI
And never use space or tabs in filenames\(!!
.LE
.H 1 "Learning an editor"
If you are already familiar with editing files on Unix,
you may ignore this part.
If not, try one of the following\(::
.ALX 1 ()
.LI
The following command will give you a tour of
.V= vim \(::
.PROMPT vimtutor
.LI
The following command will give you an introduction to
.V= emacs \(::
.PROMPT emacs &
Then select from the menu
.V= "Help \[->] Emacs tutorial" .
.LI
A beginner might prefer to use
.V= pico
or
.V= nano ,
which are extremely simple editors,
but are not used by professionals.
.LI
Never under any circumstances use
M*cr*\[Do]*ft W*rd
to create a program file.
.LI
Never cut and paste anything from a PDF into a text file.
If you cut and paste, 
do so from a text file.
.LE
.Cato.maior
.H 1 "Grading guidelines"
The subdirectory
.V= \&.score
under this directory contains instructions to the graders.
This directory will not be shown on the web,
and with
.V= ls (1),
only with the
.V= -a
option.
.H 1 "Submit checklist"
Carefully review the submit checklist\(::
.VTCODE* 0 \*[COURSEDIR]/Syllabus/submit-checklist/
.VTCODE* 0 \*[COURSEURL]/Syllabus/submit-checklist/
.FINISH
