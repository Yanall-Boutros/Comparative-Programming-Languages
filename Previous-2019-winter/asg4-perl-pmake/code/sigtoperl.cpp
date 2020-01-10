// $Id: sigtoperl.cpp,v 1.1 2018-11-06 18:50:44-08 - - $

#include <cstdlib>
#include <cstring>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <string>
#include <sys/utsname.h>
using namespace std;

int main (int, char** argv) {

   // Print UTS system information.
   struct utsname utsbuf {};
   uname (&utsbuf);
   cout << "# " << basename (argv[0]) << ": " << utsbuf.machine
        << " " << utsbuf.sysname << " " << utsbuf.nodename << endl;

   // Print current date/time.
   struct tm tm_local;
   time_t tm_now = time (nullptr);
   localtime_r (&tm_now, &tm_local);
   char tm_buffer[256];
   strftime (tm_buffer, sizeof tm_buffer, "%c", &tm_local);
   cout << "# " << basename (argv[0]) << ": " << tm_buffer << endl;

   // Print strsignal information.
   constexpr int MAXSIG = 255;
   cout << "my %strsignal = (" << endl;
   for (int sig = 0; sig < MAXSIG; ++sig) {
      const char* strsig = strsignal (sig);
      if (strsig == nullptr) continue;
      string stringsig = strsig;
      if (stringsig.find_first_of ("Unknown signal ") == 0) continue;
      if (stringsig.find_first_of ("Real-time signal ") == 0) continue;
      cout << setw(5) << sig << " => \"" << stringsig << "\"," << endl;
   }
   printf (");\n");
   return EXIT_SUCCESS;
}

