#!/usr/bin/perl
# $Id = prtpwinfo.perl,v 1.2 2018-10-02 17:00:13-07 - - $
#
# NAME
#    prtpwinfo.perl - print password file information
#
# SYNOPSIS
#    prtpwinfo
#
# DESCRIPTION
#    Uses getpwnam to obtain and print password file entries.
#

my ($username, $passwd, $uid, $gid, $quota, $comment, $gcos,
    $homedir, $shell, $expire) = getpwnam getlogin;

print "$username:$passwd:$uid:$gid:$gcos:$homedir:$shell\n";

print "username = $username\n";
print "passwd   = $passwd\n";
print "uid      = $uid\n";
print "gid      = $gid\n";
print "quota    = $quota\n";
print "comment  = $comment\n";
print "gcos     = $gcos\n";
print "homedir  = $homedir\n";
print "shell    = $shell\n";
print "expire   = $expire\n";

