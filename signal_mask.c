/* transcribed from:
Linux – Handling Signals in a Multithreaded Application
By Liran B.H | November 30, 2017
https://devarea.com/linux-handling-signals-in-a-multithreaded-application/

compiled by
gcc signal_mask.c -pthread

after running, kill -37 [process id of a.out]
to see 'thread3' turn to 'signal' 
 */
#include<stdio.h>
#include<unistd.h>
#include<pthread.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>

void mask_sig(void)
{
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGRTMIN+3);

  pthread_sigmask(SIG_BLOCK, &mask, NULL);

}

void *threadfn1(void *p)
{
  mask_sig();
  while(1){
    printf("thread1\n");
    sleep(2);
  }
  return 0;
}

void *threadfn2(void *p)
{
  mask_sig();
  while(1){
    printf("thread2\n");
    sleep(2);
  }
  return 0;
}

void *threadfn3(void *p)
{
  while(1){
    printf("thread3\n");
    sleep(2);
  }
  return 0;
}


void handler(int signo, siginfo_t *info, void *extra)
{
  int i;
  for(i=0;i<10;i++)
    {
      puts("signal");
      sleep(2);
    }
}

void set_sig_handler(void)
{
  struct sigaction action;


  action.sa_flags = SA_SIGINFO;
  action.sa_sigaction = handler;

  if (sigaction(SIGRTMIN + 3, &action, NULL) == -1) {
    perror("sigusr: sigaction");
    _exit(1);
  }

}

int main()
{
  pthread_t t1,t2,t3;
  set_sig_handler();
  pthread_create(&t1,NULL,threadfn1,NULL);
  pthread_create(&t2,NULL,threadfn2,NULL);
  pthread_create(&t3,NULL,threadfn3,NULL);
  pthread_exit(NULL);
  return 0;
}
