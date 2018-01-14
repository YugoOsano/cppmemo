//
// to start xinetd,
// sudo apt-get install xinetd

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>

struct addrinfo {
  int          ai_flags;
  int          ai_family;
  int          ai_socktype;
  int          ai_protocol;
  socklen_t    ai_addrlen;
  struct sockaddr *ai_addr;
  char            *ai_canonname;
  struct addrinfo *ai_next;
};

static int open_connection(char *host,
			   char *service)
{
  int sock;
  struct addrinfo hints, *res, *ai;
  int err;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  for (ai = res; ai; ai = ai->ai_next) {
    sock = socket(ai->ai_family,
		  ai->ai_socktype,
		  ai->ai_protocol);
    if (sock < 0) {
      continue;
    }
    if (connect(sock, ai->ai_addr,
		ai->ai_addrlen) < 0) {
      close(sock);
      continue;
    }
    /* success */
    freeaddrinfo(res);
    return sock;
  }
  fprintf (stderr, "socket(2)/connect(2) failed");
  freeaddrinfo(res);
  exit(1);
}
    

int main(int argc, char *argv[]) {

  int sock;
  FILE *f;
  char  buf[1024];

  sock = open_connection("localhost", "daytime");
  f = fdopen(sock, "r");

  if(!f) {
    perror("fdopen(3)");
    exit(1);
  }
  fgets(buf, sizeof buf, f);
  fclose(f);
  fputs(buf, stdout);
  
  exit(0);
  return 0;
}
