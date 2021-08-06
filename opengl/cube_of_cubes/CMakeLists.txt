cmake_minimum_required(VERSION 3.20)
project(cube_of_cubes)

set(CMAKE_CXX_STANDARD 17)

set(SRC_DIR src)
set(INC_DIR include)

set(SOURCES ${SRC_DIR}/main.cpp)
set(INCLUDES include/shape.h src/shape.cpp)

add_executable(cube_of_cubes ${SOURCES} ${INCLUDES})

target_include_directories(cube_of_cubes PRIVATE ${INC_DIR})

target_link_libraries(cube_of_cubes GL GLEW glfw)
