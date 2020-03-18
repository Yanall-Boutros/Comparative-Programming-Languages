// $Id: ystat.c,v 1.12 2011-01-07 18:59:13-08 - - $
//
// NAME
//    stat - print file status
//         - print file system status
// SYNOPSIS
//    stat [-l] filename...
// DESCRIPTION
//    stat uses the stat() system call to display information
//    about a file.
//

#define __KERNEL__
#define __USE_BSD
#define __USE_POSIX2

#include <errno.h>
#include <grp.h>
#include <limits.h>
#include <linux/stat.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/vfs.h>
#include <time.h>
#include <unistd.h>

#define xprintf (void)printf
#define xstrcpy (void)strcpy
#define xstrftime (void)strftime
#define xsprintf (void)sprintf

char *permission[] = {"owner", "group", "other"};
char *argv_0;

void usage (void) {
   xprintf ("Usage: %s [-l] filename...\n", argv_0);
}

void print_error (const char *name, const char *item) {
   char message[1024];
   xsprintf (message, "%s: %s \"%s\"", argv_0, name, item);
   perror (message);
}

enum {MAXLINK = 1<<10};
void print_links (char *filename) {
   char link[_POSIX_PATH_MAX + 1];
   char name[_POSIX_PATH_MAX + 1];
   ssize_t length;
   int numlinks = 0;
   xstrcpy (link, filename);
   while (numlinks++ < MAXLINK) {
      length = readlink (link, name, sizeof name);
      if (length < 0) break;
      name[length] = '\0';
      xprintf ("%2d readlink = \"%s\"\n", numlinks, name);
      xstrcpy (link, name);
   }
   if (numlinks > MAXLINK) {
      errno = ELOOP;
   }
   if (errno != EINVAL) {
      print_error ("can't readlink", link);
   }
}

void print_mode_symbolically (mode_t mode) {
   mode_t ifmt = mode & S_IFMT;
   int permit;
   switch (ifmt) {
      case S_IFDIR : xprintf ("DIR" ); break;
      case S_IFCHR : xprintf ("CHR" ); break;
      case S_IFBLK : xprintf ("BLK" ); break;
      case S_IFREG : xprintf ("REG" ); break;
      case S_IFLNK : xprintf ("LNK" ); break;
      case S_IFSOCK: xprintf ("SOCK"); break;
      case S_IFIFO : xprintf ("FIFO"); break;
      default      : xprintf ("IFMT=%03o", ((unsigned) ifmt) >> 12);
   }
   if (mode & S_ISUID) xprintf (", SUID");
   if (mode & S_ISGID) xprintf (", SGID");
   if (mode & S_ISVTX) xprintf (", SVTX");
   for (permit = 0; permit < 3; permit ++) {
      xprintf (", %s=", permission[permit]);
      if (mode & S_IREAD ) xprintf ("R");
      if (mode & S_IWRITE) xprintf ("W");
      if (mode & S_IEXEC ) xprintf ("X");
      mode = mode << 3;
   }
   xprintf ("\n");
}

void print_time (const char *label, time_t xtime) {
   struct tm *time = localtime (& xtime);
   char buffer[1<<10];
   xstrftime (buffer, sizeof buffer, "%Y-%m-%d %b %a %H:%M:%S %Z",
              time);
   xprintf ("%11ld = %s = %s\n", xtime, label, buffer);
}

void print_device (const char *label, dev_t dev) {
   dev_t minor = 0;
   dev_t major = dev >> 32;
   if (major != 0) {
      minor = dev & 0xFFFFFFFF;
   }else {
      major = dev >> 16;
      if (major != 0) {
         minor = dev & 0xFFFF;
      }else {
         major = dev >> 8;
         minor = dev & 0xFF;
      }
   }
   xprintf ("%6ld,%4ld = %s\n", major, minor, label);
}

void do_stat (char *argv[], int linkstat) {
   struct stat buffer;
   struct passwd *passwd;
   struct group *group;
   char resolved_path[MAXPATHLEN];
   double dbl_st_size;
   int error;

   for(; *argv; argv++) {
      xprintf ("\nFile \"%s\":\n", *argv);
      if (realpath (*argv, resolved_path) == NULL) {
         print_error ("no realpath", *argv);
      }else{
         xprintf ("   realpath = \"%s\"\n", resolved_path);
         if (! linkstat) print_links (*argv);
      }
      error = linkstat ? lstat (*argv, & buffer)
                       : stat (*argv, & buffer);
      if (error < 0) {
         print_error ("can't stat", *argv);
      }else{
         passwd = getpwuid (buffer.st_uid);
         group = getgrgid (buffer.st_gid);
         print_device ("dev: device inode resides on", buffer.st_dev);
         xprintf ("%11ld = ino: this inode's number\n",
                  buffer.st_ino);
         xprintf ("    %07o = mode: ", buffer.st_mode);
         print_mode_symbolically (buffer.st_mode);
         xprintf ("%11ld = nlink: number of hard links to file\n",
                  buffer.st_nlink);
         xprintf ("%11d = uid: user ID of owner = \"%s\"\n",
                  buffer.st_uid, passwd ? passwd->pw_name : "(?!)");
         xprintf ("%11d = gid: group ID of owner = \"%s\"\n",
                  buffer.st_gid, group ? group->gr_name : "(?!)");
         print_device ("rdev: device type if inode is device",
                       buffer.st_rdev);
         xprintf ("%11ld = size: total size of file in bytes",
                  buffer.st_size);
         dbl_st_size = ((double) buffer.st_size) / 1024.0;
         if (dbl_st_size >= 1.0) {
            xprintf (" = %.1fKB", dbl_st_size);
            if (dbl_st_size >= 1024.0) {
               xprintf (" = %.1fMB", dbl_st_size / 1024.0);
            }
         }
         xprintf ("\n");
         xprintf ("%11ld = blksize: "
                  "optimal blocksize for file sys i/o\n",
                  buffer.st_blksize);
         xprintf ("%11ld = blocks: actual number of blocks allocated",
                  buffer.st_blocks);
         xprintf (" = %.1fKB\n", (double) buffer.st_blocks / 2.0);
         print_time ("atime: file last access", buffer.st_atime);
         print_time ("mtime: file last modify", buffer.st_mtime);
         print_time ("ctime: file last change", buffer.st_ctime);
         print_time ("time(): CPU time of day", time (NULL));
      }
   }
}

int main (int argc, char *argv[]) {
   (void) argc;

   argv_0 = *argv++;
   if (! *argv) {
      usage();
      return EXIT_FAILURE;
   }

   if (strcmp (*argv, "-l") == 0) do_stat (++argv, 1);
                             else do_stat (argv, 0);

   return EXIT_SUCCESS;
}

