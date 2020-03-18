#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: nextbyte.st,v 1.3 2020-01-31 17:05:02-08 - - $"

[stdin atEnd] whileFalse: [
   stdout << stdin nextByte << Character nl.
]
