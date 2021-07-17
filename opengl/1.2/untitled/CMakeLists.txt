cmake_minimum_required(VERSION 3.19)
project(untitled)

set(CMAKE_CXX_STANDARD 14)

add_executable(untitled main.cpp)

target_link_libraries(untitled GL GLEW glfw)
