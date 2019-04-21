
// clang 8 install into xenial (Ubuntu 16.04) :
// add following to /etc/apt/sources.list
// # 8
// deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main
// deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial-8 main

// Nana build by Clang
// https://github.com/cnjinhao/nana/wiki/Compiling-Nana-with-Clang-8.0

// -- install of libxft
// https://stackoverflow.com/questions/45045271/cant-include-x11-xft-xft-h-when-compiling-nana-library-no-such-file-or-direc

// -- compile:
// clang++-8 use-nana.cpp -o use-nana -std=c++17 -stdlib=libc++ -I../software/nana/include -L../software/nana/build/bin -lnana -lX11 -lXcursor -lpthread -lrt -lXft -lfontconfig -lc++ -lc++fs

#include <nana/gui.hpp>

int main()
{
  using namespace nana;
  
  form fm;
  drawing{fm}.draw([](paint::graphics& graph){
      graph.string({10, 10}, "Hello, Nana C++ Library", colors::white);
    });
  
  fm.show();
  exec();
}

