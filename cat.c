/* Normal Linux Programming p-86
   handmade cat command */

/* open is included in fcntl.h */
#include <fcntl.h>

#define BUFFER_SIZE 2048

int main (int argc, char* argv[]) {
  return 0;
}

static void
do_cat (const char *path) {
  int fd;
  unsigned char buf[BUFFER_SIZE];
  int n;

  fd = open(path, O_RDONLY);
}
