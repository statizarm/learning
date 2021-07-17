cmake_minimum_required(VERSION 3.19)
project(1_5)

set(CMAKE_CXX_STANDARD 17)

add_executable(1_5 main.cpp shader.cpp shader.h texture.cpp texture.h)

target_link_libraries(1_5 GL glfw GLEW soil)
