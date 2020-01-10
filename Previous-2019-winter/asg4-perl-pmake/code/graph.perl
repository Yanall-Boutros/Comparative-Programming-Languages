#!/usr/bin/perl
# $Id: graph.perl,v 1.1 2018-11-06 18:50:43-08 - - $

use strict;
use warnings;
$0 =~ s|.*/||;

# Example setting up a directed graph.

my @inputs = (
   "all : hello",
   "hello : main.o hello.o",
   "main.o : main.c hello.h",
   "hello.o : hello.c hello.h",
   "ci : Makefile main.c hello.c hello.h",
   "test : hello",
   "clean : ",
   "spotless : clean",
);

sub parse_dep ($) {
   my ($line) = @_;
   return undef unless $line =~ m/^(\S+)\s*:\s*(.*?)\s*$/;
   my ($target, $dependency) = ($1, $2);
   my @dependencies = split m/\s+/, $dependency;
   return $target, \@dependencies;
}

my %graph;
for my $input (@inputs) {
   my ($target, $deps) = parse_dep $input;
   print "$0: syntax error: $input\n" and next unless defined $target;
   $graph{$target} = $deps;
}

for my $target (keys %graph) {
   print "\"$target\"";
   my $deps = $graph{$target};
   if (not @$deps) {
      print " has no dependencies";
   }else {
      print " depends on";
      print " \"$_\"" for @$deps;
   }
   print "\n";
}

