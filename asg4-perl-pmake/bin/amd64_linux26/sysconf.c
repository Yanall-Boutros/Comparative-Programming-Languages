/* $Id: sysconf.c,v 1.17 2015-02-03 12:34:31-08 - - $ */

#define __USE_POSIX2

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define SELECT 1
#define SC(FLAG,NAME,RETVAL,MEANING) { FLAG, NAME, #RETVAL, MEANING }
struct sclist{
   int flag;
   int name;
   char *retval;
   char *meaning;
} sclistarray[] = {

SC(0,_SC_2_C_BIND,_POSIX2_C_BIND,\
   "Supports the C language binding option"),
SC(0,_SC_2_C_DEV,_POSIX2_C_DEV,\
   "Supports the C language development utilities option"),
SC(0,_SC_2_C_VERSION,_POSIX2_C_VERSION,\
   "Integer value indicates version of ISO POSIX-2 standard "
   "(Commands)"),
SC(0,_SC_2_CHAR_TERM,_POSIX2_CHAR_TERM,\
   "Supports at least one terminal"),
SC(0,_SC_2_FORT_DEV,_POSIX2_FORT_DEV,\
   "Supports FORTRAN Development Utilities Option"),
SC(0,_SC_2_FORT_RUN,_POSIX2_FORT_RUN,\
   "Supports FORTRAN Run-time Utilities Option"),
SC(0,_SC_2_LOCALEDEF,_POSIX2_LOCALEDEF,\
   "Supports creation of locales by the localedef utility"),
SC(0,_SC_2_SW_DEV,_POSIX2_SW_DEV,\
   "Supports Software Development Utility Option"),
SC(0,_SC_2_UPE,_POSIX2_UPE,\
   "Supports User Portability Utilities Option"),
SC(0,_SC_2_VERSION,_POSIX2_VERSION,\
   "Integer value indicates version of ISO POSIX-2 standard "
   "(C language binding)"),
SC(0,_SC_AIO_LISTIO_MAX,AIO_LISTIO_MAX,\
   "Max number of I/O operations in a single list I/O call supported"),
SC(0,_SC_AIO_MAX,AIO_MAX,\
   "Max number of outstanding asynchronous I/O operations supported"),
SC(0,_SC_AIO_PRIO_DELTA_MAX,AIO_PRIO_DELTA_MAX,\
   "Max amount by which process can decrease its asynchronous "
   "I/O priority level from its own scheduling priority"),
SC(SELECT,_SC_ARG_MAX,ARG_MAX,\
   "Max size of argv[] plus envp[]"),
SC(0,_SC_ASYNCHRONOUS_IO,_POSIX_ASYNCHRONOUS_IO,\
   "Supports Asynchronous I/O"),
SC(0,_SC_ATEXIT_MAX,ATEXIT_MAX,\
   "Max number of functions that can be registered with atexit()"),
SC(SELECT,_SC_AVPHYS_PAGES,_,\
   "Number of physical memory pages not currently in use by system"),
SC(0,_SC_BC_BASE_MAX,BC_BASE_MAX,\
   "Maximum obase values allowed by bc"),
SC(0,_SC_BC_DIM_MAX,BC_DIM_MAX,\
   "Max number of elements permitted in array by bc"),
SC(0,_SC_BC_SCALE_MAX,BC_SCALE_MAX,\
   "Max scale value allowed by bc"),
SC(0,_SC_BC_STRING_MAX,BC_STRING_MAX,\
   "Max length of string constant allowed by bc"),
SC(0,_SC_CHILD_MAX,CHILD_MAX,\
   "Max processes allowed to a UID"),
SC(SELECT,_SC_CLK_TCK,CLK_TCK,\
   "Ticks per second (clock_t)"),
SC(0,_SC_COLL_WEIGHTS_MAX,COLL_WEIGHTS_MAX,\
   "Max number of weights that can be assigned to entry of the "
   "LC_COLLATE order keyword in locale definition file"),
SC(0,_SC_DELAYTIMER_MAX,DELAYTIMER_MAX,\
   "Max number of timer expiration overruns"),
SC(0,_SC_EXPR_NEST_MAX,EXPR_NEST_MAX,\
   "Max number of parentheses by expr"),
SC(0,_SC_FSYNC,_POSIX_FSYNC,\
   "Supports File Synchronization"),
SC(0,_SC_GETGR_R_SIZE_MAX,NSS_BUFLEN_GROUP,\
   "Max size of group entry buffer"),
SC(0,_SC_GETPW_R_SIZE_MAX,NSS_BUFLEN_PASSWD,\
   "Max size of password entry buffer"),
SC(0,_SC_IOV_MAX,IOV_MAX,\
   "Max number of iovec structures available to one process for use "
   "with readv() and writev()"),
SC(0,_SC_JOB_CONTROL,_POSIX_JOB_CONTROL,\
   "Supports Job control"),
SC(SELECT,_SC_LINE_MAX,LINE_MAX,\
   "Max length of input line"),
SC(0,_SC_MAPPED_FILES,_POSIX_MAPPED_FILES,\
   "Supports Memory Mapped Files"),
SC(0,_SC_MEMLOCK,_POSIX_MEMLOCK,\
   "Supports Process Memory Locking"),
SC(0,_SC_MEMLOCK_RANGE,_POSIX_MEMLOCK_RANGE,\
   "Supports Range Memory Locking"),
SC(0,_SC_MEMORY_PROTECTION,_POSIX_MEMORY_PROTECTION,\
   "Supports Memory Protection"),
SC(0,_SC_MESSAGE_PASSING,_POSIX_MESSAGE_PASSING,\
   "Supports Message Passing"),
SC(0,_SC_MQ_OPEN_MAX,MQ_OPEN_MAX,\
   "Max number of open message queues a process can hold"),
SC(0,_SC_MQ_PRIO_MAX,MQ_PRIO_MAX,\
   "Max number of message priorities supported"),
SC(0,_SC_NGROUPS_MAX,NGROUPS_MAX,\
   "Max simultaneous groups to which one can belong"),
SC(0,_SC_NPROCESSORS_CONF,_,\
   "Number of processors configured"),
SC(0,_SC_NPROCESSORS_ONLN,_,\
   "Number of processors online"),
SC(SELECT,_SC_OPEN_MAX,OPEN_MAX,\
   "Max open files per process"),
SC(SELECT,_SC_PAGESIZE,PAGESIZE,\
   "System memory page size"),
SC(SELECT,_SC_PASS_MAX,PASS_MAX,\
   "Max number of significant bytes in a password"),
SC(SELECT,_SC_PHYS_PAGES,_,\
   "Total number of pages of physical memory in system"),
SC(0,_SC_PRIORITIZED_IO,_POSIX_PRIORITIZED_IO,\
   "Supports Prioritized I/O"),
SC(0,_SC_PRIORITY_SCHEDULING,_POSIX_PRIORITY_SCHEDULING,\
   "Supports Process Scheduling"),
SC(0,_SC_RE_DUP_MAX,RE_DUP_MAX,\
   "Max number of repeated occurrences of a regular expression "
   "permitted when using interval notation \\{m,n\\}"),
SC(0,_SC_REALTIME_SIGNALS,_POSIX_REALTIME_SIGNALS,\
   "Supports Realtime Signals"),
SC(0,_SC_RTSIG_MAX,RTSIG_MAX,\
   "Max number of realtime signals reserved for application use"),
SC(0,_SC_SAVED_IDS,_POSIX_SAVED_IDS,\
   "Supports Saved IDs (seteuid())"),
SC(0,_SC_SEM_NSEMS_MAX,SEM_NSEMS_MAX,\
   "Max number of POSIX semaphores a process can have"),
SC(0,_SC_SEM_VALUE_MAX,SEM_VALUE_MAX,\
   "Max value a POSIX semaphore can have"),
SC(0,_SC_SEMAPHORES,_POSIX_SEMAPHORES,\
   "Supports Semaphores"),
SC(0,_SC_SHARED_MEMORY_OBJECTS,_POSIX_SHARED_MEMORY_OBJECTS,\
   "Supports Shared Memory Objects"),
SC(0,_SC_SIGQUEUE_MAX,SIGQUEUE_MAX,\
   "Max number of queued signals that a process can send and have "
   "pending at receiver(s) at a time"),
SC(0,_SC_STREAM_MAX,STREAM_MAX,\
   "Number of streams one process can have open at a time"),
SC(0,_SC_SYNCHRONIZED_IO,_POSIX_SYNCHRONIZED_IO,\
   "Supports Synchronized I/O"),
SC(0,_SC_THREAD_ATTR_STACKADDR,_POSIX_THREAD_ATTR_STACKADDR,\
   "Supports Thread Stack Address Attribute option"),
SC(0,_SC_THREAD_ATTR_STACKSIZE,_POSIX_THREAD_ATTR_STACKSIZE,\
   "Supports Thread Stack Size Attribute option"),
SC(0,_SC_THREAD_DESTRUCTOR_ITERATIONS,PTHREAD_DESTRUCTOR_ITERATIONS,\
   "Number attempts made to destroy thread-specific data on "
   "thread exit"),
SC(0,_SC_THREAD_KEYS_MAX,PTHREAD_KEYS_MAX,\
   "Max number of data keys per process"),
SC(0,_SC_THREAD_PRIO_INHERIT,_POSIX_THREAD_PRIO_INHERIT,\
   "Supports Priority Inheritance option"),
SC(0,_SC_THREAD_PRIO_PROTECT,_POSIX_THREAD_PRIO_PROTECT,\
   "Supports Priority Protection option"),
SC(0,_SC_THREAD_PRIORITY_SCHEDULING,_POSIX_THREAD_PRIORITY_SCHEDULING,\
   "Supports Thread Execution Scheduling option"),
SC(0,_SC_THREAD_PROCESS_SHARED,_POSIX_THREAD_PROCESS_SHARED,\
   "Supports Process-Shared Synchronization option"),
SC(0,_SC_THREAD_SAFE_FUNCTIONS,_POSIX_THREAD_SAFE_FUNCTIONS,\
   "Supports Thread-Safe Functions option"),
SC(0,_SC_THREAD_STACK_MIN,PTHREAD_STACK_MIN,\
   "Min byte size of thread stack storage"),
SC(0,_SC_THREAD_THREADS_MAX,PTHREAD_THREADS_MAX,\
   "Max number of threads per process"),
SC(0,_SC_THREADS,_POSIX_THREADS,\
   "Supports Threads option"),
SC(0,_SC_TIMER_MAX,TIMER_MAX,\
   "Max number of timer per process supported"),
SC(0,_SC_TIMERS,_POSIX_TIMERS,\
   "Supports Timers"),
SC(0,_SC_TTY_NAME_MAX,TTYNAME_MAX,\
   "Max length of tty device name"),
SC(0,_SC_TZNAME_MAX,TZNAME_MAX,\
   "Max number of bytes supported for name of a time zone"),
SC(0,_SC_VERSION,_POSIX_VERSION,\
   "POSIX.1 version supported"),
SC(0,_SC_XBS5_ILP32_OFF32,_XBS_ILP32_OFF32,\
   "Supports X/Open ILP32 w/32-bit offset build environment"),
SC(0,_SC_XBS5_ILP32_OFFBIG,_XBS5_ILP32_OFFBIG,\
   "Supports X/Open ILP32 w/64-bit offset build environment"),
SC(0,_SC_XBS5_LP64_OFF64,_XBS5_LP64_OFF64,\
   "Supports X/Open LP64, 64-bit offset build environment"),
SC(0,_SC_XOPEN_CRYPT,_XOPEN_CRYPT,\
   "Supports X/Open Encryption Feature Group"),
SC(0,_SC_XOPEN_ENH_I18N,_XOPEN_ENH_I18N,\
   "Supports X/Open Enhanced Internationalization Feature Group"),
SC(0,_SC_XOPEN_LEGACY,_XOPEN_LEGACY,\
   "Supports X/Open Legacy Feature Group"),
SC(0,_SC_XOPEN_REALTIME,_XOPEN_REALTIME,\
   "Supports X/Open POSIX Realtime Feature Group"),
SC(0,_SC_XOPEN_REALTIME_THREADS,_XOPEN_REALTIME_THREADS,\
   "Supports X/Open POSIX Reatime Threads Feature Group"),
SC(0,_SC_XOPEN_SHM,_XOPEN_SHM,\
   "Supports X/Open Shared Memory Feature Group"),
SC(0,_SC_XOPEN_UNIX,_XOPEN_UNIX,\
   "Supports X/Open CAE Specification, August 1994, System "
   "Interfaces and Headers, Issue 4, Version 2"),
SC(0,_SC_XOPEN_VERSION,_XOPEN_VERSION,\
   "Integer value indicates version of X/Open Portability Guide "
   "to which implementation conforms"),
SC(0,_SC_XOPEN_XCU_VERSION,_XOPEN_XCU_VERSION,\
   "Integer value indicates version of XCU specification "
   "to which implementation conforms"),
SC(0,-1,_,NULL),
};

const unsigned long Kbytes = 1L<<10;
const unsigned long Mbytes = 1L<<20;
const unsigned long Gbytes = 1L<<30;
void printmem2( char *label, double size ){
   printf( "%-19s %12.0f = %9.0fK = %6.0fM = %5.1fG\n", label,
           size, size / (double)Kbytes, size / (double)Mbytes,
           size / (double)Gbytes );
}

void printmem( void ){
   double realmemtotal = sysconf( _SC_PHYS_PAGES );
   double realmemfree = sysconf( _SC_AVPHYS_PAGES );
   double pagesize = sysconf( _SC_PAGESIZE );
   printmem2( "real memory total:", pagesize * realmemtotal );
   printmem2( "real memory free:", pagesize * realmemfree );
}

int main( int argc, char **argv ){
   struct sclist *sc = sclistarray;
   long confval;
   int opt;
   int optboolean = 0;
   int opterror = 0;
   int optnumeric = 0;
   int optquiet = 0;
   int optselect = 0;
   int optverbose = 0;
   for(;;){
      opt = getopt( argc, argv, "bemnsqv" );
      if( opt == EOF ) break;
      switch( opt ){
         case 'b': optboolean = 1; break;
         case 'e': opterror = 1; break;
         case 'm': printmem(); return EXIT_SUCCESS;
         case 'n': optnumeric = 1; break;
         case 'q': optquiet = 1; break;
         case 's': optselect = 1; break;
         case 'v': optverbose = 1; break;
      };
   };
   for(; sc->meaning; ++sc ){
      errno = 0;
      if( optselect && (sc->flag & SELECT) == 0 ) continue;
      confval = sysconf( sc->name );
      if( confval >= 0 ){
         if( opterror ) continue;
         if( strncmp( sc->meaning, "Supports ", 9 ) == 0 ){
            if( optnumeric ) continue;
            switch( confval ){
               case 0 : printf( "%11s", "false" ); break;
               case 1 : printf( "%11s", "true" ); break;
               default: printf( "%11ld", confval ); break;
            };
         }else{
            if( optboolean ) continue;
            printf( "%11ld", confval );
         };
      }else{
         char buffer[64];
         if( optquiet ) continue;
         if( errno == EINVAL ) sprintf( buffer, "[EINVAL]" );
                          else sprintf( buffer, "ERROR(%d)", errno );
         printf( "%11s", buffer );
      };
      printf( " =" );
      if( optverbose ) printf( " %s", sc->retval );
      printf( " %s\n", sc->meaning );
      fflush( stdout );
   };
   return EXIT_SUCCESS;
}

