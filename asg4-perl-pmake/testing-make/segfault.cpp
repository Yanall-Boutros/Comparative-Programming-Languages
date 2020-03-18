// $Id: segfault.cpp,v 1.1 2018-11-06 18:04:21-08 - - $
int main() {
   char* p = nullptr;
   return *p;
}
