// polymorphism & virtual function 
// http://www58.atwiki.jp/cschola/pages/37.html

#include <string>
#include <iostream>

class Person{
protected:
  int age;
  std::string name;
public:
  Person(int age, std::string name);
  int         getAge();
  std::string getName();

  // output varies by adding virtual
  //void selfIntroduction();
  virtual void selfIntroduction();
};

Person::Person(int age, std::string name){
  this->age  = age;
  this->name = name;
}

int Person::getAge(){
  return this->age;
}
std::string Person::getName(){
  return this->name;
}

void Person::selfIntroduction(){
  std::cout << name << ", " << age << " years old." << std::endl;
}

//-----
class Student : public Person
{
protected:
  std::string ID;
public:
  Student(int age, std::string name, std::string ID);
  ~Student(void);

  std::string getID();

  void selfIntroduction();
};

Student::Student(int age, std::string name, std::string ID) 
  : Person(age, name){
  this->ID = ID;
}

std::string Student::getID(){
  return this->ID;
}

void Student::selfIntroduction(){
  std::cout << name << ", " << age << " years old student." << std::endl;
}

int main()
{
  Student *taro = new Student(20, "Taro", "aaaa");
  Person *p = taro;

  std::cout << "Age: " << p->getAge() << std::endl;

  std::cout << "ID: " << taro->getID() << std::endl;
  //std::cout << "ID: " << p->getID() << std::endl; //<- error
  
  taro->selfIntroduction();
  p->selfIntroduction();

  return 0;
}

 


