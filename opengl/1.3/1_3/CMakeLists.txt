cmake_minimum_required(VERSION 3.19)
project(1_3)

set(CMAKE_CXX_STANDARD 17)

add_executable(1_3 main.cpp shader.h shader.cpp)

target_link_libraries(1_3 GL GLEW glfw)
