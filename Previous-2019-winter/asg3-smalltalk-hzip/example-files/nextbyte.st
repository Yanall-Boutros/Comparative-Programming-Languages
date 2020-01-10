#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: nextbyte.st,v 1.2 2019-02-11 15:22:21-08 - - $"

[stdin atEnd] whileFalse: [
   stdout << stdin nextByte << Character nl.
]
