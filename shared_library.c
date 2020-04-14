// practice creation of a shared object
// transcribed from https://taiyakisun.hatenablog.com/entry/20150506/1430896155
// compile:
// gcc -shared -fPIC -o libsharedlib.so shared_library.c

int add (int a, int b) {
  return a+b;
}
