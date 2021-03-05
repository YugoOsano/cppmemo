// practice creation of a shared object
// transcribed from https://taiyakisun.hatenablog.com/entry/20150506/1430896155
// compile:
// gcc -shared -fPIC -o libsharedlib.so shared_library.c
// (of course g++ works as well; no difference in object)
// g++ -shared -fPIC -o libsharedlib.so shared_library.c

// 1) declaration without static let the library public in gcc
// 2) the file name has to start with lib***.so
// 3) PIC stands for Position Independent Code (recommended)
//    which means it runs regardless of its absolute address.

int add (int a, int b) {
  return a+b;
}
