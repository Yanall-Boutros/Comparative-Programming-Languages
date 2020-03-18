#!/bin/perl
# $Id: 256.perl,v 1.1 2019-10-31 13:32:24-07 - - $
for $char (0..255) {
   printf "%c", $char;
   print "\n" if $char % 16 == 15;
}
