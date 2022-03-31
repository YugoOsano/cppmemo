// main.cpp
// http://minus9d.hatenablog.com/entry/20140112/1389502918
// g++ -pg gprof_test.cc (-pg is specific option for gprof)
// ./a.out
// gprof a.out

// A file gmon.out will be created and it is browsed by
// gprof a.out gmon.out

/* result on WSL2 in Thirdwave PC was
Each sample counts as 0.01 seconds.
  %   cumulative   self              self     total
 time   seconds   seconds    calls  us/call  us/call  name
76.63     71.48    71.48   100000   714.81   714.81  b()
  19.45     89.62    18.14   100000   181.39   181.39  a()
    5.55     94.79     5.17                             main
*/
#include <cstdio>

int a(void) {
    int i=0,g=0;
    while(i++<100000)
    {
        g+=i;
    }
    return g;
}
int b(void) {
    int i=0,g=0;
    while(i++<400000)
    {
        g+=i;
    }
    return g;
}

int main()
{
    int iterations = 100000;
    while(iterations--)
    {
        a();
        b();

        int i=0,g=0;
        while(i++<30000)
        {
            g+=i;
        }
    }

    return 0;
}
