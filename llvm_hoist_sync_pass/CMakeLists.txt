cmake_minimum_required(VERSION 2.8)

find_package (LLVM)

include(AddLLVM)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -fno-rtti -std=c++11")

add_llvm_loadable_module(libLLVMQs
  LiftSync.cpp)

enable_testing()

set(ll_tests
  branch_multi_proc_alias.ll
  branch_multi_proc_no_alias.ll
  branch_single_proc.ll
  loop_single_proc.ll
  )

foreach(ll_test ${ll_tests})
  add_test (NAME ${ll_test} COMMAND lit tests/${ll_test})
endforeach()