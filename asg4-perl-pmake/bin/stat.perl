#!/usr/bin/perl
# $Id: stat.perl,v 1.1 2019-10-31 13:32:24-07 - - $
use POSIX qw (strftime);
$0 =~ s|.*/||;
$sixmths = 60 * 60 * 24 * 365 / 2;
$early = $^T - $sixmths;
$late = $^T + $sixmths;
for $file (@ARGV) {
   ($_, $_, $mode, $nlink, $_, $_, $_, $size, $_, $mtime, $_, $_, $_)
         = lstat $file;
   print "$0: $file: $!\n" and next unless $mode;
   $date = strftime "%a %e "
         . ($early <= $mtime && $mtime <= $late ? "%R" : " %Y"),
           localtime $mtime;
   printf "%06o %2d %8d %s %s", $mode, $nlink, $size, $date, $file;
   $link = readlink $file;
   printf " -> %s", $link if $link;
   printf "\n";
}
