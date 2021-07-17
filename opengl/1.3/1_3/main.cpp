#include <iostream>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "shader.h"

GLfloat first_triangle[] = {
    -0.5f,  0.2f, 0.0f,
    -0.4f,  0.4f, 0.0f,
    -0.3f,  0.2f, 0.0f,
};

GLfloat second_triangle[] = {
    0.5f,  0.2f, 0.0f,
    0.4f,  0.4f, 0.0f,
    0.3f,  0.2f, 0.0f,
};

void set_viewport(GLFWwindow *w) {
  int width, height;

  glfwGetFramebufferSize(w, &width, &height);

  glViewport(0, 0, width, height);
}

void window_size_callback(GLFWwindow *window, [[maybe_unused]] int width, [[maybe_unused]] int height) {
  set_viewport(window);
}

void key_callback(GLFWwindow *window, int key, [[maybe_unused]] int scancode, int action, [[maybe_unused]] int mode) {
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  }
}

int main() {
  std::cerr << "hey you" << std::endl;

  if (glfwInit() != GLFW_TRUE) {
    const char *desc = nullptr;
    int err_code = glfwGetError(&desc);
    std::cerr << "FAILED initialize glfw" << std::endl;
    std::cerr << "Error code: " << err_code << std::endl;
    std::cerr << "Error description: " << desc;

    exit(-1);
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

  GLFWwindow *window = glfwCreateWindow(1440, 720, "My triangle", nullptr, nullptr);
  if (window == nullptr) {
    const char *desc = nullptr;
    int err_code = glfwGetError(&desc);
    std::cerr << "Failed to crate window" << std::endl;
    std::cerr << "Error code: " << err_code << std::endl;
    std::cerr << "Error description: " << desc << std::endl;

    glfwTerminate();

    exit(-1);
  }

  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;

  if (auto err = glewInit(); err != GLEW_OK) {
    std::cerr << "Failed to initialize glew" << std::endl;
    std::cerr << "Error description: " << glewGetErrorString(err) << std::endl;

    glfwTerminate();

    exit(-1);
  }

  set_viewport(window);

  glfwSetWindowSizeCallback(window, window_size_callback);
  glfwSetKeyCallback(window, key_callback);

  Shader vertex_shader = compile_shader(GL_VERTEX_SHADER, {"../shaders/vertex.glsl"});
  Shader fragment_shaders[] = {
      compile_shader(GL_FRAGMENT_SHADER, {"../shaders/first_fragment.glsl"}),
      compile_shader(GL_FRAGMENT_SHADER, {"../shaders/second_fragment.glsl"})
  };

  GLuint shader_programs[] = {
      glCreateProgram(), glCreateProgram()
  };

  for (auto i : {0, 1}) {
    glAttachShader(shader_programs[i], vertex_shader.get_descriptor());
    glAttachShader(shader_programs[i], fragment_shaders[i].get_descriptor());
    glLinkProgram(shader_programs[i]);
  }

  GLuint vertex_buffer_objects[2];
  glGenBuffers(sizeof(vertex_buffer_objects) / sizeof(vertex_buffer_objects[0]), vertex_buffer_objects);

  GLuint vertex_array_objects[2];
  glGenVertexArrays(sizeof(vertex_array_objects) / sizeof(vertex_array_objects[0]), vertex_array_objects);

  glBindVertexArray(vertex_array_objects[0]);

  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer_objects[0]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(first_triangle), first_triangle, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nullptr);
  glEnableVertexAttribArray(0);

  glBindVertexArray(vertex_array_objects[1]);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer_objects[1]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(second_triangle), second_triangle, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nullptr);
  glEnableVertexAttribArray(0);

  glBindVertexArray(0);

  while (glfwWindowShouldClose(window) == GL_FALSE) {
    glfwPollEvents();

    glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    for (auto i : {0, 1}) {
      glUseProgram(shader_programs[i]);
      glBindVertexArray(vertex_array_objects[i]);
      glDrawArrays(GL_TRIANGLES, 0, 3);
    }

    glBindVertexArray(0);

    glfwSwapBuffers(window);
  }

  return 0;
}
