#!/usr/bin/perl
# $Id: mkst.perl,v 1.1 2020-01-31 17:03:47-08 - - $
# Run smalltalk and capture the output.

$0 =~ s|.*/||;
$line = ":" x 64 . "\n";

for $prog (@ARGV) {
   system "cid + $prog";

   $lis = "$prog.lis";

   open PROG, "<$prog" or die "$0: $prog: $!";
   open LIS, ">$lis" or die "$0: $prog: $!";

   while (<PROG>) {
      next unless m/"TEST:\s*(.*)"/;
      $cmd = $1;
      print LIS $line, "$0: $cmd\n", $line;
      print LIS `$cmd`;
      print LIS $line, "Exit status $?\n";
   }

   system "mkpspdf $prog.ps $prog $lis";

}
