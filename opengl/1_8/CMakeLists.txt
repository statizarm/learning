cmake_minimum_required(VERSION 3.19)
project(1_8)

set(CMAKE_CXX_STANDARD 17)
set(INCLUDE_DIRECTORY include)
set(SOURCE_DIRECTORY src)

set(HEADERS ${INCLUDE_DIRECTORY}/shader.h ${INCLUDE_DIRECTORY}/texture.h)
set(SOURCES ${SOURCE_DIRECTORY}/main.cpp ${SOURCE_DIRECTORY}/shader.cpp ${SOURCE_DIRECTORY}/texture.cpp)

add_executable(1_8 ${SOURCES} ${HEADERS})

target_include_directories(1_8 PRIVATE ${INCLUDE_DIRECTORY})
target_link_libraries(1_8 GL soil GLEW glfw)
