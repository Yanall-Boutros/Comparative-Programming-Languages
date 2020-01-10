#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: priority.st,v 1.1 2019-02-01 14:36:25-08 - - $"

nl := Character nl.

priority := SortedCollection new.
priority inspect.

priority add:  3; inspect.
priority add:  1; inspect.
priority add:  4; inspect.
priority add: 14; inspect.
priority add: 22; inspect.
priority add:  6; inspect.
priority add:  7; inspect.
priority add:  9; inspect.
priority add: 29; inspect.
priority add:  4; inspect.

"TEST: priority.st"

