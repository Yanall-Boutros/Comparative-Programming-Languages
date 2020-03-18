#!/usr/bin/perl
# $Id: latin2utf8.perl,v 1.1 2019-10-31 13:32:24-07 - - $
#
# NAME
#    latin2utf8.perl - convert ISO-Latin-1 characters to UTF-8.
#
# SYNOPSIS
#    latin2utf8.perl [filename...]
#
# DESCRIPTION
#    Reads input files, assumed to be in ISO-Latin-1 (ISO-8859-1)
#    format and writes out the Unicode UTF-8 equivalent.
#

use strict;
use warnings;

while (my $input = <>) {
   for my $char (split '', $input) {
      my $ord = ord $char;
      $char = chr (0xC0 | $ord >> 6) . chr (0x80 | $ord & 0x3F)
            if $ord > 0x7F;
      print $char;
   }
}

