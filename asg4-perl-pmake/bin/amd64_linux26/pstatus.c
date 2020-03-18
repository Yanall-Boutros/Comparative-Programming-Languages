/* $Id: pstatus.c,v 1.4 2016-10-10 15:05:07-07 - - $ */

/*
* Print out a status code or error message.
*/

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <ctype.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wait.h>

void printsig (char *label, int signo, char *suffix) {
   char *sigstr = strsignal (signo);
   if (sigstr == NULL) sigstr = "Invalid Signal Number";
   printf ("%s %d: %s", label, signo, sigstr);
   if (suffix) printf (" (%s)", suffix);
}

long scanarg (int argc, char **argv) {
   char *endptr = NULL;
   long result;
   result = strtol (argc > 1 ? argv[1] : "", &endptr, 0);
   if (endptr == NULL || *endptr != '\0') {
      fprintf (stderr, "Usage: %s status\n", basename (argv[0]));
      fprintf (stderr, "argv[1] = \"%s\", bad chars = \"%s\"\n",
               argv[1], endptr);
   }
   return result;
}

int main (int argc, char **argv) {
   int argi;
   int status = scanarg (argc, argv);
   printf ("%s: 0x%04X ", basename (argv[0]), status);
   if (status < 0) {
      char *errstr = strerror (- status);
      if (errstr == NULL) errstr = "Invalid Error Number";
      printf ("ERROR %d: %s", - status, errstr);
   }else {
      if (WIFEXITED (status)) {
         printf ("EXIT STATUS = %d", WEXITSTATUS (status));
      }else if (WIFSIGNALED (status)) {
         printsig ("TERMINATED", WTERMSIG (status),
                   WCOREDUMP (status) ? "core dumped" : NULL);
      }else if (WIFSTOPPED (status)) {
         printsig ("STOPPED", WSTOPSIG (status), NULL);
      }else if (WIFCONTINUED (status)) {
         printf ("CONTINUED");
      }else {
         printf ("UNKNOWN EXIT STATUS");
      }
   }
   if (argc > 2) {
      printf (" --");
      for (argi = 2; argi < argc; ++argi) printf (" %s", argv[argi]);
   }
   printf ("\n");
   return EXIT_SUCCESS;
}

