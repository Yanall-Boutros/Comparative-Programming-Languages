#!/usr/bin/perl
# $Id: mk-defnames.perl,v 1.4 2020-01-24 14:03:03-08 - - $

@mlfiles = (glob ("../*.mli"), glob ("../*.ml"));

map {s|.*/(.*)\.mli?$|$1|; $prefix{$_} = 1} @mlfiles;

sub cmd {
   print "@_\n";
   system "@_";
}

for $prefix (keys %prefix) {
   @morefiles = ();
   for $suffix (qw (mli ml)) {
      $file = "$prefix.$suffix";
      if (-e "../$file") {
         cmd "(cd ..; ocamlopt -i $file) >$file.defs";
         push @morefiles, "$file.defs";
      }
   }
   cmd "more @morefiles >$prefix.defines </dev/null";
   cmd "rm @morefiles";
}

cmd "mkpspdf Listing.defines.ps *.defines";

