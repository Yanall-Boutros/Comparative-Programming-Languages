#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: bytecopy.st,v 1.2 2019-02-05 16:16:28-08 - - $"

execname := '$RCSfile: bytecopy.st,v $' =~ '.*: (.*),v.*' at: 1.
quit := [:status| ObjectMemory quit: status ].
nl := Character nl.

usage := #(
   ''
   'NAME'
   '   bytecopy - copy bytes from input file to output file'
   'SYNOPSIS'
   '   bytecopy infile outfile'
   'DESCRIPTION'
   '   Open infile and outfile.  Copy bytes from in file to'
   '   outfile, then close both files.  Errors to stderr.'
   ''
).

Smalltalk arguments size = 2 ifFalse: [
   usage do: [:line |
      stderr << line << nl.
   ].
   quit value: 1.
].

openfile := [:filename :mode |
   |file|
   [
      file := FileStream open: filename mode: mode
   ] on: SystemExceptions.FileError do: [:signal|
      stderr << ': ' << signal messageText << Character nl.
      quit value: 1.
   ].
   file.
].

infilename := Smalltalk arguments at: 1.
outfilename := Smalltalk arguments at: 2.
infile := openfile value: infilename value: FileStream read.
outfile := openfile value: outfilename value: FileStream write.

[infile atEnd] whileFalse: [
   |byte|
   byte := infile nextByte.
   outfile nextPutByte: byte.
].

infile close.
outfile close.

