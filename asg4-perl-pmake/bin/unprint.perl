#!/usr/bin/perl
# $Id: unprint.perl,v 1.1 2019-10-31 13:32:24-07 - - $
#
# Uses <> to read all input and convert unprintable characters,
# according to the current locale, to octal escapes.
#

use POSIX qw (setlocale LC_CTYPE isprint);
setlocale LC_CTYPE, "en_US.ISO8859-1";

sub unprint ($) {
   my ($char) = @_;
   return $char if isprint $char or $char eq "\n";
   my $ord = ord $char;
   return sprintf "^%c", $ord ^ 0x40 if $ord < 0x80;
   return sprintf "\\%03o", $ord;
}

s/./unprint $&/ge and print while <>

