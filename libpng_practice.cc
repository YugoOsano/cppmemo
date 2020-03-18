// https://www.hiroom2.com/2014/05/28/libpng%E3%81%AEc-%E3%82%A4%E3%83%B3%E3%82%BF%E3%83%BC%E3%83%95%E3%82%A7%E3%83%BC%E3%82%B9%E3%81%A7%E3%81%82%E3%82%8Bpng-%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%A6%E3%81%BF%E3%82%8B/

/* to install to WSL:
   sudo apt install libpng++-dev

   to see relevant library:
   dpkg -L libpng++-dev

   to see linking option:
   libpng-config --libs

   then compile command is:
   g++ libpng_practice.cc -lpng12
 */

#include <png++/png.hpp>

int main() {
  png::image<png::rgb_pixel> image("input.png");

  return 0;
}
