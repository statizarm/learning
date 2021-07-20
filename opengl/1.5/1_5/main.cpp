#include <cmath>
#include <iostream>

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "shader.h"
#include "texture.h"

GLfloat vertices[] = {
//Coords           color             texture coords
  -0.5, -0.5, 0.0, 0.4f, 0.4f, 0.0f, 0.0f, 0.0f,
  -0.5, 0.5, 0.0,  0.4f, 0.4f, 0.0f, 0.0f, 1.0f,
  0.5, 0.5, 0.0,   0.4f, 0.4f, 0.0f, 1.0f, 1.0f,
  0.5, -0.5, 0.0,  0.4f, 0.4f, 0.0f, 1.0f, 0.0f,
};

GLuint indices[] = {
  0, 1, 2,
  2, 3, 0
};

GLfloat perc = 0.2f;

void key_callback(GLFWwindow *window, int key, [[maybe_unused]] int scancode, int action, [[maybe_unused]] int mode) {
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  } else if (key == GLFW_KEY_UP && action == GLFW_PRESS) {
    perc += 0.05;
    if (perc > 1.0f) {
      perc = 1.0f;
    }
  } else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS) {
    perc -= 0.05;
    if (perc < 0.0f) {
      perc = 0.0f;
    }
  }
}

int main() {
  glfwInit();

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

  GLFWwindow *window = glfwCreateWindow(800, 600, "Shaders", nullptr, nullptr);
  if (window == nullptr) {
    const char *desc = nullptr;
    glfwGetError(&desc);
    std::cerr << desc << std::endl;

    glfwTerminate();
    exit(-1);
  }

  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  if (auto err = glewInit(); err != GLEW_OK) {
    std::cout << glewGetErrorString(err) << std::endl;

    glfwTerminate();
    exit(-1);
  }

  glViewport(0, 0, 800, 600);

  ShaderProgram prog("../shaders/base.vert", "../shaders/base.frag");
  if (!prog) {
    std::cerr << prog.error_msg() << std::endl;

    glfwTerminate();
    exit(-1);
  }

  GLuint buffer_objects[1];
  GLuint vertex_array_objects[1];
  GLuint element_buffer_objects[1];

  glGenBuffers(sizeof(buffer_objects) / sizeof (buffer_objects[0]), buffer_objects);
  glGenBuffers(sizeof(element_buffer_objects) / sizeof(element_buffer_objects[0]), element_buffer_objects);
  glGenVertexArrays(sizeof(vertex_array_objects) / sizeof(vertex_array_objects[0]), vertex_array_objects);

  glBindVertexArray(vertex_array_objects[0]);

  glBindBuffer(GL_ARRAY_BUFFER, buffer_objects[0]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), (void *) 0);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), (void *) (3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), (void *) (6 * sizeof(GLfloat)));
  glEnableVertexAttribArray(2);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, element_buffer_objects[0]);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);

  glBindVertexArray(0);

  Texture textures[] = {
      load_texture("../imgs/container.jpg"),
      load_texture("../imgs/awesomeface.png")
  };

  for (auto &tex : textures) {
    if (!tex) {
      std::cerr << tex.error_msg() << std::endl;
    }
  }

  glfwSetKeyCallback(window, key_callback);

  while (glfwWindowShouldClose(window) != GL_TRUE) {
    glfwPollEvents();

    glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    prog.use();
    glBindVertexArray(vertex_array_objects[0]);

    glActiveTexture(GL_TEXTURE0);
    textures[0].bind();
    glActiveTexture(GL_TEXTURE1);
    textures[1].bind();

    glUniform1i(prog.uniform_location("tex1"), 0);
    glUniform1i(prog.uniform_location("tex2"), 1);
    glUniform1f(prog.uniform_location("perc"), perc);

    glDrawElements(GL_TRIANGLES, sizeof(indices) / sizeof(indices[0]), GL_UNSIGNED_INT, nullptr);

    Texture::unbind();
    glBindVertexArray(0);


    glfwSwapBuffers(window);
  }

  return 0;
}
