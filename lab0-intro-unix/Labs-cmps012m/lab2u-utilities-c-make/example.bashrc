# $Id: example.bashrc,v 1.2 2016-01-06 16:29:57-08 - - $

export cmps012b=/afs/cats.ucsc.edu/courses/cmps012b-wm
export submit012b=/afs/cats.ucsc.edu/class/cmps012b-wm.s13

export EDITOR=vim
export MANPAGER=more
export MANWIDTH=72
export PATH=$PATH:$cmps012b/bin
export SHELL=/bin/bash
export VISUAL=vim

export PS1='\s-\!\$ '
set -o ignoreeof
set -o noclobber
set -o physical
unset HISTFILE

alias cp='cp -i'
alias grind='valgrind --leak-check=full --show-reachable=yes'
alias m='more'
alias mv='mv -i'
alias rm='rm -i'

alias 0='cd $cmps012b'
alias 0a='cd $cmps012b/Assignments'
alias 0m='cd $cmps012b/Labs-cmps012m'

alias la='ls -la'
alias lf='ls -Fa'
alias ll='ls -goa'
alias llh='ls -goah'
alias llr='ls -goaR'
alias lls='ls -goaSr'
alias llt='ls -goatr'
unalias ls 2>/dev/null

