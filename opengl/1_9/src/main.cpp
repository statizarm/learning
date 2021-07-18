#include <cmath>
#include <iostream>

#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>


#include "shader.h"
#include "texture.h"

GLfloat vertices[] = {
    -0.5f, -0.5f, -0.5f,  0.0f, 0.0f,
    0.5f, -0.5f, -0.5f,  1.0f, 0.0f,
    0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    -0.5f,  0.5f, -0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 0.0f,

    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
    0.5f, -0.5f,  0.5f,  1.0f, 0.0f,
    0.5f,  0.5f,  0.5f,  1.0f, 1.0f,
    0.5f,  0.5f,  0.5f,  1.0f, 1.0f,
    -0.5f,  0.5f,  0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,

    -0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
    -0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
    -0.5f,  0.5f,  0.5f,  1.0f, 0.0f,

    0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
    0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
    0.5f,  0.5f,  0.5f,  1.0f, 0.0f,

    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    0.5f, -0.5f, -0.5f,  1.0f, 1.0f,
    0.5f, -0.5f,  0.5f,  1.0f, 0.0f,
    0.5f, -0.5f,  0.5f,  1.0f, 0.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,

    -0.5f,  0.5f, -0.5f,  0.0f, 1.0f,
    0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
    0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
    -0.5f,  0.5f,  0.5f,  0.0f, 0.0f,
    -0.5f,  0.5f, -0.5f,  0.0f, 1.0f
};

glm::vec3 cube_positions[] = {
    glm::vec3( 0.0f,  0.0f,  0.0f),
    glm::vec3( 2.0f,  5.0f, -15.0f),
    glm::vec3(-1.5f, -2.2f, -2.5f),
    glm::vec3(-3.8f, -2.0f, -12.3f),
    glm::vec3( 2.4f, -0.4f, -3.5f),
    glm::vec3(-1.7f,  3.0f, -7.5f),
    glm::vec3( 1.3f, -2.0f, -2.5f),
    glm::vec3( 1.5f,  2.0f, -2.5f),
    glm::vec3( 1.5f,  0.2f, -1.5f),
    glm::vec3(-1.3f,  1.0f, -1.5f)
};

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode) {
  if (key == GLFW_KEY_ESCAPE) {
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  }
}

void set_viewport(GLFWwindow *window) {
  int width, height;
  glfwGetFramebufferSize(window, &width, &height);

  glViewport(0, 0, width, height);
}

void mouse_callback(GLFWwindow *window, double x, double y) {
}

void scroll_callback(GLFWwindow *window, double x_offset, double y_offset) {

}

void framebuffer_size_callback(GLFWwindow *window, int width, int height) {
  std::cout << "window framebuffer size changed to: " << width << "x" << height << std::endl;
  glViewport(0, 0, width, height);
}

int main() {
  if (glfwInit() != GLFW_TRUE) {
    std::cerr << "Failed to initialize glfw" << std::endl;
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE);

  GLFWwindow *window = glfwCreateWindow(800, 600, "1_9", nullptr, nullptr);
  if (!window) {
    std::cerr << "Failed to create window" << std::endl;

    glfwTerminate();
    exit(-1);
  }

  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  if (auto error_code = glewInit(); error_code != GLEW_OK) {
    std::cerr << "Failed to initialize GLEW, due to: " << glewGetErrorString(error_code) << std::endl;

    glfwTerminate();
    exit(-1);
  }

  set_viewport(window);

  ShaderProgram program {"../shaders/base.vert", "../shaders/base.frag"};
  if (!program) {
    std::cerr << "Failed to compile and link program due to:\n" << program.error_msg() << std::endl;
  }

  Texture textures[] = {
      load_texture("../imgs/container.jpg"),
      load_texture("../imgs/awesomeface.png")
  };
  for (auto &t : textures) {
    if (!t) {
      std::cerr << "Failed to load texture due to:\n" << t.error_msg() << std::endl;
    }
  }

  GLuint vertex_arrays[1];
  GLuint vertex_buffers[1];

  glGenVertexArrays(1, vertex_arrays);
  glGenBuffers(1, vertex_buffers);

  glBindVertexArray(vertex_arrays[0]);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffers[0]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), nullptr);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), (void *) (3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);
  glBindVertexArray(0);

  //glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

  glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
  glfwSetKeyCallback(window, key_callback);
  glfwSetCursorPosCallback(window, mouse_callback);
  glfwSetScrollCallback(window, scroll_callback);

  glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
  glEnable(GL_DEPTH_TEST);

  glm::vec3 look_dest (0.0f, 0.0f, 0.0f);

  while (glfwWindowShouldClose(window) == GLFW_FALSE) {
    glfwPollEvents();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    int width, height;
    glfwGetFramebufferSize(window, &width, &height);

    program.use();
    glBindVertexArray(vertex_arrays[0]);
    for (int i = 0; i < sizeof(textures) / sizeof(textures[0]); ++i) {
      glActiveTexture(GL_TEXTURE0 + i);
      textures[i].bind();
    }

    glUniform1i(program.uniform_location("tex1"), 0);
    glUniform1i(program.uniform_location("tex2"), 1);

    for (int i = 0; i < sizeof(cube_positions) / sizeof (cube_positions[0]); ++i) {
      glm::mat4x4 model = glm::translate(glm::mat4x4(1.0f), cube_positions[i]);

      if (i % 3 == 0) {
        model = glm::rotate(model, glm::radians((float) glfwGetTime()) * 40.0f,
                            glm::normalize(glm::vec3(1.0f, 1.0f, 0.0f)));
      }

      float camX = sin(glm::radians(glfwGetTime() * 40.0f)) * 10.0f;
      float camZ = cos(glm::radians(glfwGetTime() * 40.0f)) * 10.0f;
      glm::mat4x4 view = glm::lookAt(glm::vec3(camX, 0.0f, camZ), look_dest, glm::vec3(0.0f, 1.0f, 0.0f));
      glm::mat4x4 projection = glm::perspective(45.0f, (float) width / (float) height, 0.1f, 100.0f);

      glUniformMatrix4fv(program.uniform_location("model"), 1, GL_FALSE, glm::value_ptr(model));
      glUniformMatrix4fv(program.uniform_location("view"), 1, GL_FALSE, glm::value_ptr(view));
      glUniformMatrix4fv(program.uniform_location("projection"), 1, GL_FALSE, glm::value_ptr(projection));

      glDrawArrays(GL_TRIANGLES, 0, 36);
    }

    glfwSwapBuffers(window);
  }

  glDeleteBuffers(1, vertex_buffers);
  glDeleteVertexArrays(1, vertex_arrays);
  glfwTerminate();
  return 0;
}
