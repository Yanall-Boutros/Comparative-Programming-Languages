// $Id: utimes.cpp,v 1.10 2020-02-14 18:01:52-08 - - $

#include <iostream>
#include <limits>
#include <string>
#include <map>
using namespace std;

#include <sys/types.h>
#include <utime.h>

map<string,time_t> table {
   {"time-int-max.txt", numeric_limits<int>::max()},
   {"time-int-min.txt", numeric_limits<int>::min()},
   {"time-time_t-max.txt", numeric_limits<time_t>::max()},
   {"time-time_t-min.txt", numeric_limits<time_t>::min()},
   {"time-zero.txt", 0}, 
};

int main() {
   struct utimbuf utimbuf {0, 0};
   cout << numeric_limits<time_t>::min() << endl;
   cout << numeric_limits<time_t>::max() << endl;
   for (const auto& item: table) {
      system (("cp /dev/null " + item.first).c_str());
      utimbuf.actime = utimbuf.modtime = item.second;
      utime (item.first.c_str(), &utimbuf);
      system (("ls -la " + item.first).c_str());
   }
}
