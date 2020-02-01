/* Normal Linux Programming p-86
   handmade cat command */

#include <stdio.h>
#include <stdlib.h>
/* close is included in unistd.h */
#include <unistd.h>

/* open is included in fcntl.h */
#include <fcntl.h>

static void do_cat(const char* path);
static void die(   const char* s);

#define BUFFER_SIZE 2048

int main (int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr, "%s: file not given\n", argv[0]);
    exit(1);
  }
  for(int i = 1; i < argc; i++) {
    do_cat(argv[i]);
  }
  exit(0);
  return 0;
}

static void
do_cat (const char *path) {
  int fd;
  unsigned char buf[BUFFER_SIZE];
  int n;

  fd = open(path, O_RDONLY);
  if (fd < 0) die(path);

  /* set the offset of file descriptor:
     l stands for long (int)
   */
  lseek(fd, 10, SEEK_SET);
  for (;;) {
    n = read(fd, buf, sizeof buf);
    if (n < 0) die(path);
    if (n == 0) break;
    if (write(STDOUT_FILENO, buf, n) < 0) die(path);
  }
  if (close(fd) < 0) die(path);
}
static void die(const char* s) {
  /* this is stdio.h; p stands for print */
  perror(s);
  exit(1);   /* stdlib.h */
}
/*
  Note (1)
  StackOverFlow 62936
  What does the number in parentheses shown after Unix command names in manpages mean?
  1 General commands
  2 System calls
  3 C library functions
  4 Special files (usually devices, those found in /dev) and drivers
  5 File formats and conventions
  6 Games and screensavers
  7 Miscellanea
  8 System administration commands and daemons

  Note(2)
  file descriptors are listed at /proc/[process id]/fd/
 */
