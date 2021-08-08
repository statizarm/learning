cmake_minimum_required(VERSION 3.19)
project(cube_of_cubes)

set(CMAKE_CXX_STANDARD 17)

set(SRC_DIR src)
set(INC_DIR include)

set(SOURCES ${SRC_DIR}/main.cpp src/shape.cpp src/renderer.cpp src/shader_program.cpp)
set(INCLUDES include/shape.h include/renderer.h include/shader_program.h include/shader.h src/shader.cpp)

add_executable(cube_of_cubes ${SOURCES} ${INCLUDES})

target_include_directories(cube_of_cubes PRIVATE ${INC_DIR})

target_link_libraries(cube_of_cubes GL GLEW glfw)
