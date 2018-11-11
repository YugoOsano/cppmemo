//https://thispointer.com/c11-multithreading-part-9-stdasync-tutorial-example/
// g++ -std=c++11 -pthread async.cc
#include <iostream>
#include <string>
#include <chrono>
#include <thread>
#include <future>

std::string fetchDataFromDB(const std::string& recvdData) {
  std::this_thread::sleep_for(std::chrono::seconds(5));

  return "DB_" + recvdData;
}
std::string fetchDataFromFile(const std::string& recvdData) {
  std::this_thread::sleep_for(std::chrono::seconds(5));

  return "File_" + recvdData;
}


int main () {
  std::chrono::system_clock::time_point start =
    std::chrono::system_clock::now();

  std::future<std::string> resultFromDB =
    std::async(std::launch::async,
	       fetchDataFromDB,
	       "Data");
  std::string fileData = fetchDataFromFile("Data");

  std::string dbData = resultFromDB.get();
  
  auto end = std::chrono::system_clock::now();

  auto diff = std::chrono::duration_cast<std::chrono::seconds>
    (end - start).count();

  std::cout << diff << "sec" << std::endl;

  std::cout << dbData << " :: " << fileData << std::endl;
  
  return 0;
}

