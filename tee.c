/* tee command source transcribed from
   https://www.glamenv-septzen.net/view/574
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <locale.h>
#include <signal.h>
/* err, warn function is in: */
#include <err.h>
/* DEFFILEMODE is defined in */
#include <sys/stat.h>

typedef struct _list {
  struct _list *next;
  int fd;     /* ファイル記述子 */
  char *name; /* argvから取得したファイル名 */
} LIST;
LIST *head;

void add(fd, name)
     int fd;
     char *name;
{
  LIST *p;

  if ((p = malloc((size_t)sizeof(LIST))) == NULL)
    err(1, "malloc");
  p->fd = fd;
  p->name = name;
  p->next = head;
  head = p;
}
int main(argc, argv)
     int argc;
     char *argv[];
{
  LIST *p;
  int n, fd, rval, wval;
  char *bp;
  int append, ch, exitval;
  char *buf;
#define BSIZE (8 * 1024)

  /* ロカールを初期化 */
  setlocale(LC_ALL, "");

  append = 0;
  while ((ch = getopt(argc, argv, "ai")) != -1)
    switch((char)ch) {
    case 'a':
      /* "-a"ならファイルへ追記する */
      append = 1;
      break;
    case 'i':
      /* "-i"ならSIGINTを無視する */
      (void)signal(SIGINT, SIG_IGN);
      break;
    case '?':
    default:
      (void)fprintf(stderr, "usage: tee [-ai] [file ...]\n");
      exit(1);
    }
  argv += optind;
  argc -= optind;

  /* バッファ領域の確保 */
  if ((buf = malloc((size_t)BSIZE)) == NULL)
    err(1, "malloc");
  add(STDOUT_FILENO, "stdout");

  for (exitval = 0; *argv; ++argv)
    if ((fd = open(*argv, append ? O_WRONLY|O_CREAT|O_APPEND :
		   O_WRONLY|O_CREAT|O_TRUNC, DEFFILEMODE)) < 0) {
      warn("%s", *argv);
      exitval = 1;
    } else
      add(fd, *argv);

  while ((rval = read(STDIN_FILENO, buf, BSIZE)) > 0)
    for (p = head; p; p = p->next) {
      n = rval;
      bp = buf;/* bp stands for buffer pointer? */
      do {
	if ((wval = write(p->fd, bp, n)) == -1) {
	  warn("%s", p->name);
	  exitval = 1;
	  break;
	}
	bp += wval;
      } while (n -= wval);
    }
  if (rval < 0) {
    warn("read");
    exitval = 1;
  }

  for (p = head; p; p = p->next) {
    if (close(p->fd) == -1) {
      warn("%s", p->name);
      exitval = 1;
    }
  }

  exit(exitval);
}
