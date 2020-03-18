#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f
"$Id: extendstream.st,v 1.2 2020-01-31 17:05:02-08 - - $"

FileStream extend [
   |bitIndex currentChar|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]

nl := Character nl.
count := 0.
[stdin atBitEnd not] whileTrue: [
   stdout << stdin nextBit.
   count := count + 1.
   count = 4 ifTrue: [stdout << ' '].
   count = 8 ifTrue: [stdout << nl. count := 0].
]

"TEST: echo Hello World | extendstream.st"

