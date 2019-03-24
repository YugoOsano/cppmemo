/* https://oshiete.goo.ne.jp/qa/160619.html */
/* keyread function substitute without wait on Linux */

#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <signal.h>
#include <errno.h>

/* https://linuxjm.osdn.jp/html/LDP_man-pages/man2/open.2.html */
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static void post_proc(int);

static struct termios SavedTermAttr;

int main(int argc, char **argv) {
  char c;
  signal(SIGINT, &post_proc);
  struct termios term_attr;

  /* Terminal control */
  /* ファイルディスクリプタ０(標準入力)
     のターミナル属性を取得 */
  if( tcgetattr(0, &term_attr) < 0 ){
    fprintf(stderr, "Can't get terminal attributes.\n");
    post_proc(-1);
  }
  /* 取得したターミナル属性を保存 */
  SavedTermAttr = term_attr;
  /* ICANON、ECHOフラッグをセット */
  term_attr.c_lflag &= ~(ICANON|ECHO);
  /* 入力文字列の最小読出バイト数を１に */
  term_attr.c_cc[VMIN] = 1;
  /* 入力待ち時間を０に */
  term_attr.c_cc[VTIME] = 0;

  /* 変更したターミナル属性をセット */
  if( tcsetattr(0, TCSANOW, &term_attr) < 0 ){
    fprintf(stderr, "Can't change terminal attributes.\n");
    post_proc(-1);
  }
  /* NONBLOCKフラグのセット */
  if( fcntl(0, F_SETFL, O_NONBLOCK) == -1 ){
    fprintf(stderr, "Can't fcntl().\n");
    post_proc(-1);
  }
  /* Main loop */
  while(1){
    while( read(0, &c, 1) != 1 )
      if( errno != EAGAIN ){
	fprintf(stderr, "EOF\n");
	post_proc(0);
      }
    printf("%c\n", c);
  }
  return 0;
}

void post_proc(int sig)
{
  /* 保存されたターミナル属性を復元 */
  if( tcsetattr(0, TCSANOW, &SavedTermAttr) < 0 ){
    fprintf(stderr, "Can't change terminal attribute.\n");
    exit(-1);
  }
  if( sig < 0 )
    exit(-1);
  else
    exit(0);
}
