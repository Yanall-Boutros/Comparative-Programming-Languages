#!/bin/sh
# $Id: mk,v 1.2 2018-11-06 18:52:16-08 - - $

cid + $0

cid + graph.perl
graph.perl >graph.output 2>&1

cid + mtime.perl
cp /dev/null zerotime.txt
utime zerotime.txt
mtime.perl * . /dev/null >mtime.output 2>&1

cid + sigtoperl.cpp
mkc sigtoperl.cpp
sigtoperl >sigtoperl.output 2>&1

mkpspdf Listing.ps \
      graph.perl graph.output \
      mtime.perl mtime.output \
      sigtoperl.cpp sigtoperl.output

