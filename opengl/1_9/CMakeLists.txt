cmake_minimum_required(VERSION 3.19)
project(1_9)

set(CMAKE_CXX_STANDARD 17)

set(SRC_DIR src)
set(INC_DIR include)
set(SOURCES ${SRC_DIR}/main.cpp ${SRC_DIR}/shader.cpp ${SRC_DIR}/texture.cpp src/camera.cpp)
set(HEADERS ${INC_DIR}/shader.h ${INC_DIR}/texture.h include/camera.h)

add_executable(1_9 ${SOURCES} ${HEADERS})

target_include_directories(1_9 PRIVATE ${INC_DIR})
target_link_libraries(1_9 GL soil glfw GLEW)
