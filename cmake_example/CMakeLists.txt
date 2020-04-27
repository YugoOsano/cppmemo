# transcribed from 
# https://kamino.hatenablog.com/entry/cmake_tutorial1
cmake_minimum_required(VERSION 3.1)

# project name and programming language
project(hello_cmake CXX)

# to switch compiler, the easiest way is:
# export CXX=clang++

# exe file and source files
add_executable(main_app main.cpp func.hpp func.cpp)

# to prevent intermediate files mixing,
# create build directory to work therein.
#
# cmake ..

# to build,
# cmake --build .
# (or make) : the happening is the same in default setting

# to remove object files and exe,
# make clean

# tips: to see git status in a subdirectory,
# git status . (or any path)
# https://stackoverflow.com/questions/1044446/how-to-get-git-status-of-a-single-subfolder

# Generator options are listed by
# cmake --help
# (the default is Unix Makefile)

# blog about cmake tutorial
# https://yuyubu-sub.hateblo.jp/entry/2019/11/25/cmake
