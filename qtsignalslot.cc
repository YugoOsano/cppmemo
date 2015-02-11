/* 
 * Use of SIGNAL/SLOT in Qt
 * http://densan-labs.net/tech/qt/chapter3.html
 * Compiled by:
 * qmake -project "QT += widgets"
 * qmake
 * make
 */

#include <QtWidgets>

int main(int argc, char** argv){
  QApplication app(argc, argv);
  QPushButton* buttonA = new QPushButton("Quit");
  QObject::connect(buttonA, SIGNAL( clicked() ),
		   &app, SLOT(quit()) );

  buttonA->show();
  return app.exec();
}
