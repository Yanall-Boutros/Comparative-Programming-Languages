#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: catfile.st,v 1.3 2019-02-11 14:53:34-08 - - $"
execname := '$RCSfile: catfile.st,v $' =~ '.*: (.*),v.*' at: 1.

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         |padded|
         count := count + 1.
         padded := count printPaddedWith: Character space to: 6.
         stdout << padded << ': ' << self nextLine << nl.
      ]
   ]
].

nl := Character nl.
Smalltalk arguments size = 0
   ifTrue: [stdin catFile]
   ifFalse: [
      Smalltalk arguments do: [:filename |
         |separator|
         separator := '::::::::::::::::::::::::::::::::'.
         stdout << separator << nl << filename << nl << separator << nl.
         filename = '-'
            ifTrue: [stdin catFile]
            ifFalse: [
               [
                  |infile|
                  infile := FileStream open: filename
                                       mode: FileStream read.
                  infile catFile.
                  infile close.
               ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
                  stderr flush.
               ]
            ]
      ]
   ]

"TEST: echo 'This is a test file.' >test.data"
"TEST: echo 'Hello, world!' >>test.data"
"TEST: echo 'foo bar baz qux' >>test.data"
"TEST: catfile.st test.data mks /dev/null"
"TEST: rm test.data"

