// ref:
// http://makkareru.hatenablog.com/entry/2013/04/05/013652

// install: sudo apt-get install libglew1.5-dev
// compile: gcc freeglut_test.c -lglut

// include path: /usr/include/GL

// Prof. Tokoi's lab
// https://tokoik.github.io/opengl/libglut.html

#include <GL/glut.h>
int main(int argc,char *argv[])
{
  /*初期化*/
  glutInit(&argc,argv);
  /*ウィンドウ作成*/
  glutCreateWindow(argv[0]);
  /*メインループ*/
  glutMainLoop();
  return 0;
}
