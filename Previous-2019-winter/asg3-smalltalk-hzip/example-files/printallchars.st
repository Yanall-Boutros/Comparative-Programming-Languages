#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: printallchars.st,v 1.2 2019-02-05 13:56:55-08 - - $"

0 to: 255 do: [:byte |
   stdout nextPutByte: byte
]

