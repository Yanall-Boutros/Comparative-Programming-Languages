.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.GETST* ELIMCR_BASH Figure_ELIMCR_BASH
.GETHN* HELLO_JAVA Header_HELLO_JAVA
.GETHN* MK_HELLO Header_MK_HELLO
.TITLE CMPS-012M Winter\~2016 Lab\~1 "Shells, scripts, etc."
.RCS "$Id: lab1u-shells-etc.mm,v 1.51 2015-12-07 14:38:24-08 - - $"
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
.H 1 "Overview"
This lab will continue your introduction to Unix.
We will look at environment variables,
controlling the shell with
.V= \&.bashrc ,
and backing up files using the
.V= RCS
utilities.
.H 1 "The \f[CB]\[Do]PATH\f[P] variable"
There are some commands given in this lab which are not generally
available Unix commands.
These are
.V= cid 
and
.V= checksource .
You will notice that you get an error message when you use them\(::
.DS
.PROMPT cid hello.java
.OUTPUT "bash: cid: command not found"
.DE
This is because they are not in your path.
These commands, among many others, are in the directory
.V= \*[BINDIR]/ .
You should add this directory to your path.
How this is done depends on which shell you are using.
We will discuss only
.V= bash
in this document and ignore the others. 
.P
The path to the course
.V= bin
directory can be added to your path with the command\(::
.PROMPT export PATH=\[Do]PATH:\*[BINDIR]
This command should be added to your
.V= \&.bashrc
file.
.H 1 "Standard output, standard error, and exit status"
Normal output is written to the standard output,
which is 
.V= stdout
in C,
.V= System.out
in Java,
and file descriptor 1 in the shell.
Error messages and other out of band notes are written to the
standard error,
which is
.V= stderr
in C,
.V= System.err
in Java,
and file descriptor 2 in the shell.
When a program returns to its caller,
it provides an exit status integer between 0 and 255.
Success is indicated with 0,
and any other number indicates failure.
.P
.SETR HELLO_JAVA
Write a Java program called
.V= hello.java
with the following specifications\(::
.ALX a ()
.LI
If it is given no command line arguments (in
.V= args )
it prints the message
.VTCODE* 1 "Hello, World!"
to the standard output and exits with status 0.
.LI
If it is given any command line arguments,
it prints the message
.VTCODE* 1 "Usage: hello"
to the standard error and exits with status code 1.
.LE
.H 1 "Putting Java in a jar"
Java programs can be run from the command line using the
.V= java
command directly,
but the usual practice is to put class files in a jar file
so that they can be executed like any other Unix command.
.P
This is done via the 
.V= jar
command, which requires a
.V= Manifest
file to tell the jar file exec which class is the main class.
.SETR MK_HELLO
.DS
.PROMPT echo Main-class: hello >Manifest
.PROMPT jar cvfm hello Manifest hello.class
.PROMPT chmod +x hello
.DE
The 
.V= echo
command redirects output to the 
.V= Manifest
file (note the capital M).
Then the
.V= hello
jar is created and the 
.V= x -bit
is turned on.
.H 1 "Running a jar file"
A jar file can just be run using the name of the file,
without having to specify that it is a Java program.
It can be specified by explicit path name, as here,
or by the name of the program being looked up in the
.V= \[Do]PATH
variable.
.DS
.PROMPT ./hello
.OUTPUT Hello, World!
.PROMPT echo \[Do]?
.OUTPUT 0
.PROMPT ./hello world
.OUTPUT Usage: hello
.PROMPT echo \[Do]?
.OUTPUT 1
.DE
.H 1 "The script \f[CB]checksource\f[P]"
Use the script
.V= checksource
to check on some basic formatting items.
Edit your files so that it does not complain.
If you run
.V= checksource
without filename operands,
it will print out a text-format manual page.
To check up on
.V= hello.java ,
use the command\(::
.DS
.PROMPT "checksource hello.java"
.DE
Tab characters should
.E= only
be used in a
.V= Makefile .
Otherwise indentation should be done with a few spaces.
If you edit on a M*cr*\[Do]*ft system,
make sure to eliminate the carriage turn charcters from the
file when copying to Unix.
And do not make lines longer than 72 characters.
.P
Carriage return characters can be eliminated with the following
.V= vim
line mode command\(::
.VTCODE* 1 ":g/\[ha]V\[ha]M/s///g"
Do not actually type the circumflex
.=V ( \[ha] )
here.
The
.V= \[ha] -notation
means hold down the Control key.
So
.V= \[ha]V\[ha]M
means type the letters ``vm''
while holding down the Control key.
Or use the
.V= bash
script
.V= elimcr
(see Figure \*[Figure_ELIMCR_BASH])
in the course
.V= bin
directory.
.H 1 "\f[CB]RCS\f[P]"
It is a good idea to keep many backup copies of your work.
.V= RCS
is a good utility to keep track of backup copies.
If you don't have backups,
you will have to depend on the IT department
to recover yesterday's copy.
Murphy's law says that the most important changes won't be in
that copy.
The corollary also says that you will lose your files so close
to the due date that IC won't get your backups back to you in time.
In this case, 
you will get a 0 on the assignment for not submitting anything.
.P
Note that the code above has a magic string in it\(::
.V= \[Do]Id\[Do] \|.
These track your development progress.
For the initial checkin, do the following\(::
.DS
.PROMPT mkdir RCS
.PROMPT ci -zLT -s- -t- -m- -u hello.java
.OUTPUT RCS/hello.java,v  <--  hello.java
.OUTPUT initial revision: 1.1
.OUTPUT done
.DE
Now you have your initial version.
Look at the 
.V= man
page for
.V= ci
for all of the options.
You will also want to read the
.V= co 
page.
The options were\(::
.V= -zLT
causes the time stamp to be in local time instead of UTC,
.V= -s-
sets the state to \-,
.V= -t-
suppresses the descriptive text,
.V= -m-
suppresses the log message,
and
.V= -u
checks in the file unlocked, thus not destroying the source.
.P
.ne 7v
Unfortunately, 
the file is now read-only,
so you may want to make locking non-strict\(::
.DS
.PROMPT rcs -U hello.java
.OUTPUT RCS file: RCS/hello.java,v
.OUTPUT done
.PROMPT chmod u+w hello.java
.PROMPT ls -la hello.java
.OUTPUT -rw-r--r--  1 foobar  user  465 Sep 14 18:42 hello.java
.DE
Oops, you forgot to put your name and username at the top of the file.
Edit the comment on the first line to reflect your name and username.
Every file you submit must have a comment on the first line
with your name and username in it.
Add in your name and username.
Now check in another copy to make a backup.
.DS
.PROMPT ci -zLT -s- -t- -m- -u hello.java
.OUTPUT RCS/hello.java,v  <--  hello.java
.OUTPUT new revision: 1.2; previous revision: 1.1
.OUTPUT done
.DE
Use 
.V= cat
to look at the new version of your file.
.P
There are some alternatives to
.V= RCS \(::
.V= SCCS
(very old).
.V= CVS
(more flexible but more complicated).
.V= SVN
(some people like using this).
There are also some others.
.H 1 "Recovering lost files"
If you are keeping files in an
.V= RCS
subdirectory,
you may recover them using the
.V= co
command.
For example
.PROMPT co -r1.9 hello.java
will recover version 1.9 of the file
.V= hello.java
from the archive.
.P
To see what versions of
.V= hello.java
you have in the archive, use the command
.PROMPT rlog hello.java
.P
If you want to see the differences between, say, versions 1.7 and 1.11,
use the command
.PROMPT rcsdiff -r1.7 -r1.11 hello.java
.P
Whenever you create a new file, 
either the first or last line should be a comment with the
.V= \[Do]Id\[Do]
code in it, as in
.VTCODE* 1 "// \[Do]Id\[Do]"
After doing this, a check-in will automatically edit it to something
like
.VTCODE* 1 \
"// \[Do]Id: hello.java,v 1.1 2015-01-06 18:02:07-08 - - \[Do]"
which shows the name of the file,
the veersion,
and the date and time of check-in.
The ``-08'' at the end of the time indicates the number of
hours west of Greenwich Mean Time (GMT), 
aka Universal Time Co\[:o]rdinated (UTC).
.H 1 "The script \f[CB]cid\f[P]"
An alternative to using
.V= ci
(see below)
directly is the program
.V= cid .
It works just like
.V= ci ,
but automatically creates the
.V= RCS
subdirectory and does the correct locking.
To fetch back a deleted file, use the
.V= co
command.
You will find that the
.V= cid
command is much simpler to use,
since it automatically sets up the
.V= RCS
subdirectory and appropriate file locking.
.P
In order to find where that script is, you can do the following\(::
.DS
.PROMPT cd \*[COURSEDIR]
.PROMPT find * -name cid -follow 2>/dev/null
.DE
This says find all files whose name is
.V= cid ,
even if you have to follow symbolic links.
Without the redirection
.V= 2>/dev/null ,
you will get lots of error messages because of directories that you
don't have permission to access.
With this redirection, error messages will be sent to
.V= /dev/null ,
the bit bucket.
Try it both ways, with and without redirecting
.V= stderr .
.H 1 "Lab exercises"
Each of the following items specifies something that must be 
done for credit in this lab.
.ALX 1 ()
.LI
Write the program
.V= hello.java
.substring Header_HELLO_JAVA 0 -4
as described in section \*[Header_HELLO_JAVA].
Note that neither the class name nor the file name have upper-case
letters.
Then check it into an RCS subdirectory\(::
.PROMPT cid hello.java
Then edit the file and insert the following line as the first line in
the file\(::
.VTCODE* 1 "// \[Do]Id\[Do]"
Then check it in again\(::
.PROMPT cid hello.java
Now add your name and username immediately after the RCS Id line,
and check it in again.
Notice that it now has version 1.3 of the file,
or later, if you have edited it several more times.
.SUBMIT hello.java
.LI
Write a shell script for
.V= bash
called
.V= mkhello .
Make the first two lines of the file as follows\(::
.VTCODE* 1 "#!/bin/bash
.VTCODE* 1 "# \[Do]Id\[Do]
and insert your name and username as the third line.
After that add the necessary
.V= bash
commands to do the following (in the order specified here)\(::
.ALX i ()
.LI
Use the
.V= cid
command to check
.V= hello.java
into the RCS subdirectory.
.LI
Compile
.V= hello.java
into
.V= hello.class .
.LI
Create the
.V= Manifest
file.
.LI
Put the
.V= Manifest
and the class file into a jar file called
.V= hello .
.LI
Turn on the 
.V= x -bit
to make it executable.
.LI
Remove the
.V= Manifest
and
.V= hello.class
files.
.LE
.substring Header_MK_HELLO 0 -4
See section \*[Header_MK_HELLO] for details of some of these commands.
Check the script
.V= mkhello
into the RCS subdirectory.
.SUBMIT mkhello
.LI
Write another shell script called
.V= testhello .
Make sure the hashbang
.=V ( #! )
is the first line and the RCS Id is the second line.
This script should create 6 files\(::
.ALX a ()
.LI
When
.V= hello
is run without arguments, it creates
.V= test1.out ,
.V= test1.err ,
and
.V= test1.status .
.LI
When
.V= hello
is run with the argument
.V= world
it creates
.V= test2.out ,
.V= test2.err ,
and
.V= test2.status .
.LE
The files with suffix
.V= \&.out
should capture standard output\(;;
the files with suffix
.V= \&.err
should capture standard error\(;;
and the files with suffix
.V= status
should have the exit status values from 
.V= bash 's
variable
.V= \[Do]? .
.SUBMIT testhello
.LI
When programs are in nonstandard places, the
.V= \[Do]PATH
environment variable addes places to look for programs and scripts.
To ensure that
.V= cid
and
.V= checksource
are available, add the following to
.V= \[ti]/.bashrc\(::
.VTCODE* 1 "export cmps012b=/afs/cats.ucsc.edu/courses/cmps012b-wm"
.VTCODE* 1 "export PATH=\[Do]PATH:\[Do]cmps012b/bin"
.SUBMIT \[ti]/.bashrc
.LI
Using an alias is a useful way to avoid typing in long commands
and pathnames.
Add the following lines to
.V= \[ti]/.bashrc\(::
.VTCODE* 1 "alias 0='cd \[Do]cmps012b'"
.VTCODE* 1 "alias 0a='cd \[Do]cmps012b/Assignments'"
.VTCODE* 1 "alias 0m='cd \[Do]cmps012b/Labs-cmps012m'"
After sourcing 
.V= \[ti]/.bashrc ,
you can use the commands
.V= 0 ,
.V= 0a ,
and
.V= 0m
to quickly navigate to three of my directories.
Perhaps you wish to add a few more aliases.
Note that there are no spaces before or after the equal sign.
.SUBMIT \[ti]/.bashrc
.LI
Another way to refer to files quickly is with a symbolic link.
A symbolic link is just a file named somewhere convenient which
points at another file.
Type the following\(::
.PROMPT "ln -s /afs/cats.ucsc.edu/courses/cmps012b-wm/ \[ti]/12b
This creates the link
.V= 12b
in your home directory, so no you can get to the 12B directory with
PROMPT "cd \[ti]/12b"
Symbolic links remain as files unless/until you delete them.
The commands
.V= ls (1)
and
.V= stat (1)
can be used to find information about files.
Create a file
.V= links \(::
.PROMPT ls -la \[ti]/12b >links
.PROMPT stat \[ti]/12b >>links
Note that
.=V `` > ''
is a redirect to create a file or replace it,
and
.=V `` >> ''
appends to a file.
.SUBMIT links
.LI
Are you sure you submitted all required files\(??
You may submit files more than once before the due date.
In the directory containing the
.V= \&.tt
version of this file, type the command\(::
.PROMPT grep Submit: *.tt
This will list all of the lines with the string
.=V `` Submit: ''.
Cut and paste the output of this command into a file called
.V= submits .
.SUBMIT submits
.LE
.H 1 "Submit checklist"
Carefully review the submit checklist\(::
.VTCODE* 0 \*[COURSEDIR]/Syllabus/submit-checklist/
.VTCODE* 0 \*[COURSEURL]/Syllabus/submit-checklist/
.P
The subdirectory
.V= \&.score
has instructions to the grader,
along with a script that the grader will run.
.DS
\&
.B1
.SP
.ft CR
.nf
.eo
.pso cat -n bin/elimcr | expand
.ec
.fi
.ft R
.SP
.B2
.FG "\f[CB]bin/elimcr\f[P]" "" 0 ELIMCR_BASH
.DE
.DS
\&
.B1
.SP
.ft CR
.nf
.eo
.pso cat -n bashrc.example | expand
.ec
.fi
.ft R
.SP
.B2
.FG "Example \f[CB].bashrc" "" 0 ELIMCR_BASH
.DE
.FINISH
