#include <iostream>

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "shader.h"
#include "shader_program.h"

void set_viewport(GLFWwindow *window) {
  int width, height;
  glfwGetFramebufferSize(window, &width, &height);

  glViewport(0, 0, width, height);
}

int main() {
  glfwInit();

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

  GLFWwindow *window = glfwCreateWindow(800, 600, "cube of cubes", nullptr, nullptr);
  if (window == nullptr) {
    const char *log;
    std::cerr << "Failed to create window. Error code = " << glfwGetError(&log) << std::endl
              << "Description: " << log << std::endl;

    glfwTerminate();
    exit(-1);
  }

  glfwMakeContextCurrent(window);

  glewInit();

  Shader shaders[] = {
      Shader::make_shader(GL_VERTEX_SHADER, "../shaders/shader.vert"),
      Shader::make_shader(GL_FRAGMENT_SHADER, "../shaders/shader.frag")
  };

  for (const auto &s: shaders) {
    if (!s) {
      std::cerr << s.info_log() << std::endl;
    }
  }

  ShaderProgram program = ShaderProgram::make_program(shaders[0],
                                                      shaders[1],
                                                      "model",
                                                      "view",
                                                      "projection",
                                                      {"texture1", "texture2"});
  if (!program) {
    std::cerr << program.info_log() << std::endl;
  }

  set_viewport(window);

  glClearColor(0.2f, 0.3f, 0.3f, 1.0f);

  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();

    glClear(GL_COLOR_BUFFER_BIT);

    glfwSwapBuffers(window);
  }

  glfwTerminate();
  return 0;
}
