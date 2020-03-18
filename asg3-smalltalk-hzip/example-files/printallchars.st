#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: printallchars.st,v 1.3 2020-01-31 17:05:02-08 - - $"

0 to: 255 do: [:byte |
   stdout nextPutByte: byte
]

