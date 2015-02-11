/* 
 * Use of QtLayout
 * http://densan-labs.net/tech/qt/chapter2.html
 * Compiled by:
 * qmake -project "QT += widgets"
 * qmake
 * make
 */

#include <QtWidgets>

int main(int argc, char** argv){
  QApplication app(argc, argv);
  QWidget* window = new QWidget;
  QPushButton* buttonA = new QPushButton("Button A");
  QPushButton* buttonB = new QPushButton("Button B");
  QPushButton* buttonC = new QPushButton("Button C");
  //QHBoxLayout* layout  = new QHBoxLayout;
  QVBoxLayout* layout  = new QVBoxLayout;

  layout->addWidget(buttonA);
  layout->addWidget(buttonB);
  layout->addWidget(buttonC);

  window->setLayout(layout);
  window->show();
  return app.exec();
}
