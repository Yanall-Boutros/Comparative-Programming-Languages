#!/usr/bin/perl
# $Id: str2time.perl,v 1.1 2019-10-31 13:32:24-07 - - $

use HTTP::Date qw (str2time);
use POSIX qw (strftime);

$input = "@ARGV";
$time = str2time $input;
$date = strftime "%a %b %e %H:%M:%S %Z %Y", localtime $time;

print "Input: $input\n";
print "Time:  $time\n";
print "Date:  $date\n";

