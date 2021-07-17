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

GLfloat perspective_angle = 45.0f;

glm::vec3 view_dir(0.0f, 0.0f, -3.0f);

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode) {
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  } else if (key == GLFW_KEY_UP && action == GLFW_PRESS) {
    perspective_angle += 1.0f;
    if (perspective_angle > 90.0f) {
      perspective_angle = 90.0f;
    }
  } else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS) {
    perspective_angle -= 1.0f;
    if (perspective_angle < 0.0f) {
      perspective_angle = 0.0f;
    }
  } else if (key == GLFW_KEY_W && action == GLFW_PRESS) {
    view_dir.z += 0.2f;
  } else if (key == GLFW_KEY_S && action == GLFW_PRESS) {
    view_dir.z -= 0.2f;
  } else if (key == GLFW_KEY_A && action == GLFW_PRESS) {
    view_dir.x += 0.2f;
  } else if (key == GLFW_KEY_D && action == GLFW_PRESS) {
    view_dir.x -= 0.2f;
  }
}

void set_viewport(GLFWwindow *window) {
  int width, height;
  glfwGetWindowSize(window, &width, &height);

  glViewport(0, 0, width, height);
}

int main() {
  if (glfwInit() != GLFW_TRUE) {
    std::cerr << "Failed to initialize glfw" << std::endl;
  }

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

  GLFWwindow *window = glfwCreateWindow(800, 600, "1_8", nullptr, nullptr);
  if (window == nullptr) {
    std::cerr << "Failed to create window" << std::endl;

    glfwTerminate();
    exit(-1);
  }

  glfwMakeContextCurrent(window);

  glewExperimental = GL_TRUE;
  if (auto err = glewInit(); err != GLEW_OK) {
    std::cerr << "Failed to initialize GLEW: " << glewGetErrorString(err) << std::endl;

    glfwTerminate();
    exit(-1);
  }

  set_viewport(window);

  ShaderProgram program {"../shaders/base.vert", "../shaders/base.frag"};
  if (!program) {
    std::cerr << "Failed to compile and link shader program due to:\n" << program.error_msg() << std::endl;

    glfwTerminate();
    exit(-1);
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

  GLuint vertex_buffers[1];
  GLuint vertex_arrays[1];

  glGenVertexArrays(sizeof(vertex_arrays) / sizeof(vertex_arrays[0]), vertex_arrays);
  glGenBuffers(sizeof(vertex_buffers) / sizeof(vertex_buffers[0]), vertex_buffers);

  glBindVertexArray(vertex_arrays[0]);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffers[0]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), nullptr);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), (void *) (3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);

  glBindVertexArray(0);

  glfwSetKeyCallback(window, key_callback);
  glClearColor(0.2f, 0.3f, 0.3f, 1.0f);


  glm::vec3 positions[] = {
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

  glEnable(GL_DEPTH_TEST);
  while (!glfwWindowShouldClose(window)) {
    glfwPollEvents();
    glm::mat4x4 projection = glm::perspective(perspective_angle * glm::pi<GLfloat>() / 180, 800.0f / 600.0f, 0.1f, 100.0f);
    glm::mat4x4 view = glm::translate(glm::mat4x4(1), view_dir);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    program.use();
    glBindVertexArray(vertex_arrays[0]);
    glActiveTexture(GL_TEXTURE0);
    textures[0].bind();
    glActiveTexture(GL_TEXTURE1);
    textures[1].bind();

    glUniform1i(program.uniform_location("tex1"), 0);
    glUniform1i(program.uniform_location("tex2"), 1);


    for (int i = 0; i < sizeof(positions) / sizeof(positions[0]); ++i) {
      glm::mat4x4 model (1.0f);

      model = glm::translate(model, positions[i]);

      if (i % 3 == 0) {
        model = glm::rotate(model, (GLfloat) glfwGetTime() * 1.0f, glm::normalize(glm::vec3(1.0f, 1.0f, 1.0f)));
      } else {
        model = glm::rotate(model, 20.0f * i, glm::normalize(glm::vec3(1.0f, 1.0f, 1.0f)));
      }

      glUniformMatrix4fv(program.uniform_location("model"), 1, GL_FALSE, glm::value_ptr(model));
      glUniformMatrix4fv(program.uniform_location("view"), 1, GL_FALSE, glm::value_ptr(view));
      glUniformMatrix4fv(program.uniform_location("projection"), 1, GL_FALSE, glm::value_ptr(projection));

      glDrawArrays(GL_TRIANGLES, 0, sizeof(vertices) / sizeof(vertices[0]));
    }

    glBindVertexArray(0);

    glfwSwapBuffers(window);
  }

  return 0;
}
