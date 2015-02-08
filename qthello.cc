// Qt install on Ubuntu
// https://qt-project.org/wiki/Install_Qt_5_on_Ubuntu

// getting started with Qt
// http://densan-labs.net/tech/qt/chapter1.html

// PATH setting in Ubuntu
// http://qiita.com/takanemu/items/6027291be8a2a6af9adc

// library setting in Ubuntu
// http://weble.org/2011/12/20/ubuntu-ld-so-conf

/* Compiled by:
 * qmake -project "QT += widgets"
 * qmake
 * make
 * http://stackoverflow.com/questions/23711592/qmake-does-not-add-widgets
 */

//#include <QApplication>
//#include <QLabel> //these are obsolete
#include <QtWidgets>

int main(int argc, char** argv){
  QApplication app(argc, argv);
  QLabel* label = new QLabel("Hello Qt!");
  label->show();
  return app.exec();
}
