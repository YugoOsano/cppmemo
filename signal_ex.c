// -- this is from Beginning C language for Linux engineer by Yutaka Hirata
//    (p-140)
// after running the binary, launch another terminal and type:
// ps -ef | grep a.out
// kill -USR1 [process ID]
// to rewind the counter

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

volatile int g_count = 0;

void sample_signal_handler(int num) {
  write(1, "IEEE\n", 5);
  g_count = -1;
}

int main () {
  int i, ret;
  struct sigaction sig;

  memset(&sig, 0, sizeof(sig));
  sig.sa_handler = sample_signal_handler;
  sig.sa_flags = 0;
  ret = sigaction(SIGUSR1, &sig, NULL);
  if (ret) {
    perror("sigaction error");
  }

  for (i = 0; i < 60; i++) {
    g_count++;
    printf("%d\n", g_count);
    sleep(1);
  }

  return 0;
}
