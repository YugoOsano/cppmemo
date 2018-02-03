// 'Normal Linux Programming' by M.Aoki
// p-258
// to run; ./a.out /bin/echo OK

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

int main (int argc, char *argv[]) {
  // fork creates a new process by copying self
  pid_t pid;

  if (argc != 3) {
    fprintf(stderr, "number of arg is wrong.\n");
    exit(1);
  }
  
  pid = fork();
  printf("%d\n", pid);

  if (pid < 0){
    fprintf(stderr, "fork failed.\n");
    exit(1);
  }

  if (pid == 0) { /* child process */
    execl(argv[1], argv[1], argv[2], NULL);
    perror(argv[1]);
    exit(99);
  }
  else { /* parent process */
    int status;

    waitpid(pid, &status, 0);
    printf("child (PID=%d) finished; ", pid);
    if (WIFEXITED(status))
      printf("exit, status=%d\n", WEXITSTATUS(status));
    else if (WIFSIGNALED(status))
      printf("signal, sig=%d\n", WTERMSIG(status));
    else
      printf("abnormal exit\n");

    exit(0);
  }  
  return 0;
}





