cmake_minimum_required(VERSION 2.8)

find_package (LLVM)

include(AddLLVM)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -fno-rtti -std=c++11")

add_llvm_loadable_module(libQsGc
  QsGCPrinter.cpp)
