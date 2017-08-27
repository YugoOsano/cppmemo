// main.cpp
// http://minus9d.hatenablog.com/entry/20140112/1389502918
// g++ -pg gprof_test.cc
// ./a.out
// gprof a.out 

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
