#include <iostream>

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "shader.h"
#include "shader_program.h"
#include "renderer.h"
#include "shape.h"
#include "texture.h"
#include "camera.h"

GLfloat cube_mesh_data[] = {
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

AttributeSpecification attrs[] = {
    {0, 3, GL_FLOAT, 5 * sizeof(GLfloat), (void *) 0},
    {1, 2, GL_FLOAT, 5 * sizeof(GLfloat), (void *) (3 * sizeof(GLfloat))}
};


ViewCamera view_camera {glm::vec3(0.0f, 0.0f, 3.0f), glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f)};
ProjectionCamera projection_camera {glm::radians(45.0f), 800, 600, 0.1f, 100.0f};

Renderer renderer {&view_camera, &projection_camera};

double frame_time = 0.0f;

void set_viewport(GLFWwindow *window) {
  int width, height;
  glfwGetFramebufferSize(window, &width, &height);

  projection_camera.set_width((float) width);
  projection_camera.set_height((float) height);

  glViewport(0, 0, width, height);
}

void resize_callback(GLFWwindow *window, int width, int height) {
  set_viewport(window);
}

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode) {
  if (key == GLFW_KEY_ESCAPE)  {
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  } else if (key == GLFW_KEY_W && action == GLFW_REPEAT) {
    view_camera.move(ViewCamera::FORWARD, frame_time);
  } else if (key == GLFW_KEY_S && action == GLFW_REPEAT) {
    view_camera.move(ViewCamera::BACKWARD, frame_time);
  } else if (key == GLFW_KEY_A && action == GLFW_REPEAT) {
    view_camera.move(ViewCamera::LEFT, frame_time);
  } else if (key == GLFW_KEY_D && action == GLFW_REPEAT) {
    view_camera.move(ViewCamera::RIGHT, frame_time);
  }
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

  glewExperimental = GL_TRUE;
  if (glewInit() != GLEW_OK) {
    std::cerr << "Failed to initialize glew" << std::endl;
    glfwTerminate();
    exit(-1);
  }

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

  auto meshd = renderer.specify_mesh(cube_mesh_data, sizeof(cube_mesh_data), 36, attrs, 2);

  Texture2D container_texture = Texture2D::make_texture("../textures/container.jpg");
  if (!container_texture) {
    std::cout << container_texture.info_log() << std::endl;
  }
  Texture2D awesomeface_texture = Texture2D::make_texture("../textures/awesomeface.png");
  if (!awesomeface_texture) {
    std::cout << awesomeface_texture.info_log() << std::endl;
  }

  std::vector<Texture2D *> textures = {
      &container_texture,
      &awesomeface_texture
  };

  Prototype cube_proto {meshd, textures};
  Shape cube {&cube_proto, glm::vec3 (0.0f, 0.0f, 0.0f)};

  ShapeSystem cube_of_cubes{&cube,  3.0f};
  ShapeSystem cube_of_cubes_of_cubes{&cube_of_cubes, 6.0f};

  glEnable(GL_DEPTH_TEST);
  glClearColor(0.2f, 0.3f, 0.3f, 1.0f);

  glfwSetWindowSizeCallback(window, resize_callback);
  glfwSetKeyCallback(window, key_callback);

  while (!glfwWindowShouldClose(window)) {
    double start_time = glfwGetTime();
    glfwPollEvents();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    cube_of_cubes_of_cubes.set_position(glm::vec3(0.0f, 0.0f, -10.0f));
    cube_of_cubes_of_cubes.set_rotation(glm::normalize(glm::vec3(0.7f, 1.0f, 0.2f)), glm::radians((float) glfwGetTime()) * 10.0f);
    cube_of_cubes_of_cubes.render(renderer, program);

    glfwSwapBuffers(window);
    frame_time = glfwGetTime() - start_time;
  }

  glfwTerminate();
  return 0;
}
