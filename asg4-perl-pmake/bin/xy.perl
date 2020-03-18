#!/usr/bin/perl -w
# $Id: xy.perl,v 1.1 2019-10-31 13:32:24-07 - - $
#
# NAME
#    x - start a new command in an xterm, man and perldoc special.
#
# SYNOPSIS
#    x [man|perldoc|command] [operands...]]
#
# DESCRIPTION
#    The command given is started in a new xterm with the title
#    set to the command.  If no command is given, just starts an
#    xterm.
#
#    If the command is either man or perldoc, the program is run
#    with STDOUT and STDERR redirected to a temporary file, which
#    is then edited to remove backspace graphics, after which the
#    file is brought up in an xterm with view.
#

use POSIX;
$0 =~ s{.*/}{};

sub open_FILE(@) {
   my $tmpdir = $ENV{TMPDIR} || "/var/tmp";
   mkdir $tmpdir, 0755 or die "$0: mkdir $tmpdir: $!" unless -d $tmpdir;
   my (@argv) = @_;
   map { s:[\s!"#\$&'()*/;<>?[\\\]^`{|}\~]::g } @argv;
   my $tmpname = $tmpdir . "/" . join ".", $0, @argv;
   my @now = localtime;
   $tmpname .= strftime ".%m%d.%H%M", @now;
   return $tmpname if ! -e $tmpname and open FILE, ">$tmpname";
   my $tryname;
   for (my $uniq = "aa"; $uniq !~ m/.../; ++$uniq) {
      $tryname = "$tmpname.$uniq";
      return $tryname if ! -e $tryname and open FILE, ">$tryname";
   }
   die "$0: open >$tryname: $!";
   return undef;
}

sub view_command(@) {

   select STDERR; $| = 1;
   select STDOUT; $| = 1;
   $pid = open PIPE, '-|';
   defined $pid or die "$0: open PIPE, '-|': $!";
   if ($pid == 0) {
      close STDIN or die "$0: close STDIN: $!";
      exec @ARGV;
      die "$0: exec @ARGV: $!";
   }
   my $openfile = open_FILE @ARGV;
   $old_RS = $/;
   $/ = ''; # Paragraph mode.
   while ($line = <PIPE>) {
      0 while $line =~ s/(.)[\b]\1/$1/g;
      0 while $line =~ s/_[\b]|[\b]_//g;
      0 while $line =~ s/o[\b]\+|\+[\b]o/\+/g;
      print FILE $line;
   }
   $/ = $old_RS;
   close FILE;
   close PIPE;
   print "$0: $openfile: pid $pid, status $?\n" if $?;
   @ARGV =  ('view', $openfile);
}

sub byseq{
   my $aa = $a;
   my $bb = $b;
   return $aa =~ s/^.*?(\d+)$/$1/ && $bb =~ s/^.*?(\d+)$/$1/
        ? $aa <=> $bb
        : $a cmp $b;
}

sub view_info() {
   $dir = '/usr/local/share/gnu/info/';
   chdir $dir or die "$0: chdir $dir: $!";
   my $base = $ARGV[1];
   @allfiles = <*>;
   @files = sort {byseq} grep {m/^$base(\.info)?(-\d+)?$/} @allfiles;
   if (@files) {
      @ARGV =  ('more', @files);
   }else{
      map{ s/(\.info)?(-\d+)?$//; $hash{$_} = 1 } @allfiles;
      @allfiles = sort keys %hash;
      @ARGV =  ('echo', @ARGV, 'not found:', @allfiles);
   }
   &view_command;
}

%special = (
   'man'     => \&view_command,
   'perldoc' => \&view_command,
   'ginfo'   => \&view_info,
);

sub main() {

   @uname{ qw (-s -n -r -v -m) } = uname;
   $hostname = $uname{ '-n' };
   $hostname =~ s/\..*//;
   $ttyname = (ttyname 0) || "";
   $ttyname =~ s|^/dev|^|;

   if (@ARGV) {
      $title = join ' ', $0, @ARGV;
      $special = $special{ $ARGV[0] } and &$special;
      unshift @ARGV, '-e';
   }else{
      $title = "$ENV{USER}\@$hostname:$ttyname";
   }

   @title = ('-T', $title, '-n', $title);
   @command =  (qw (xterm -ut -ls), @title, @ARGV);
   print STDERR join (' ', @command), "\n";
   exec @command;
   die "$0: exec @command: $!";

}

main;

