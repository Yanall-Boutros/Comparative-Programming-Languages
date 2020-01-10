.so Tmac.mm-etc
.if t .Newcentury-fonts
.INITR* \n[.F]
.SIZE 12 14
.TITLE Lab0 "Introduction to Unix"
.RCS "$Id: lab0-intro-unix.mm,v 1.23 2019-12-12 14:28:13-08 - - $"
.PWD
.URL
.P
This lab
will not be graded and there is no credit for it.
However, it is important you understand the Unix command
line and submit procedure before you actually use it in the
first assignment.
.P
Make sure you know how to submit files.
Submit these files to
.V= lab0.
Verify that the submit actually worked by looking 
in the submit directory for your username.
The
.V= find (1)
command is very useful for things like this.
.P
The submit command copies your files into a directory
.VTCODE* 1 /afs/cats.ucsc.edu/class/cse112-wm.w20/lab0/\[Do]USER/
and prefixes each file with a sequence number.
You can see the names of the files,
but not their contents.
The submit directory is not accessible using a web browser.
.P
.ft BI
Prior experience with Unix is assumed.
.ft P
Attend a lab section to ask questions if you don't
understand how submit and the Unix command line in general works.
The TA can explain such things.
.P
You may wish to review some of the Unix labs in the subdirectory
.V= Labs-cmps012m/ .
.P
Read the submit checklist\(::
.VTCODE* 1 \
/afs/cats.ucsc.edu/courses/cse112-wm/Syllabus/submit-checklist/
Write a program in some language and then submit it.
See the submit checklist
to verify what you have submitted.
If you are not sure,
attend a lab and ask the TA to verify the submit.
.P
If you choose to do pair programming,
carefully read the pair programming guidelines,
including details on how to submit a
.V= PARTNER
file\(::
.VTCODE* 1 \
/afs/cats.ucsc.edu/courses/cse112-wm/Syllabus/pair-programming/
Points will be deducted for an incorrect
.V= PARTNER
file.
.P
To make available scripts such as
.V= checksource ,
.V= partnercheck ,
etc., put the following in your
.V= \[Do]PATH
environment variable\(::
.VTCODE* 1 /afs/cats.ucsc.edu/courses/cse112-wm/bin/
This can be done with the following
.V= bash (1)
command\(::
.VTCODE* 1 \
"export PATH=\[Do]PATH:/afs/cats.ucsc.edu/courses/cse112-wm/bin
.P
If you put this command in your
.V= \[ti]/.bash_profile ,
it will automatically be executed every time your log in.
Add similar paths to the executable binaries for the various
languages we will be using this quarter.
If
.V= bash (1)
is not your login shell,
it is recommended that your
change your login shell to
.V= bash .
To find out how to do this,
log into
.V= unix.ucsc.edu
and type the command
.V= chsh .
.P
Questions of general interest should be posted to Piazza
.=V ( https://piazza.com ),
rather than sent via email, since that usually gets a
faster response.
