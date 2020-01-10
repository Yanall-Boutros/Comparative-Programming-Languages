// $Id: hello.c,v 1.1 2015-01-13 15:48:01-08 - - $

//
// NAME
//    hello - print the "Hello, World!" message.
//
// SYNOPSIS
//    hello
//
// DESCRIPTION
//    Prints the "Hello, World!" message if no operands are
//    present.  Errors out with a Usage message otherwise.
//

#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char **argv) {
   printf ("Hello, World!\n");
   return EXIT_SUCCESS;
}

