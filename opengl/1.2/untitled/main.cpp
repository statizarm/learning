#include <iostream>
#include <cstdlib>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

struct Color {
    float red;
    float green;
    float blue;
};

Color gen_random_color() {
    Color c = {
            .red = (float) rand() / (float) RAND_MAX,
            .green = (float) rand() / (float) RAND_MAX,
            .blue = (float) rand() / (float) RAND_MAX
    };

    return c;
}

Color bg_color = {
        .red = 0.2f,
        .green = 0.3f,
        .blue = 0.1f
};

void key_callback(GLFWwindow *w, int key, int scancode, int action, int mode) {
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
        glfwSetWindowShouldClose(w, GL_TRUE);
    } else if (key == GLFW_KEY_SPACE && action == GLFW_PRESS) {
        bg_color = gen_random_color();
    }
}

int main() {
    glfwInit();

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

    GLFWwindow *window = glfwCreateWindow(800, 600, "My window", nullptr, nullptr);
    if (window == nullptr) {
        const char *desc = nullptr;
        std::cerr << "Failed to create glfw window" << std::endl;
        std::cerr << "ERROR code: " << glfwGetError(&desc) << std::endl;
        std::cerr << "DESC: " << desc << std::endl;
        glfwTerminate();
        exit(-1);
    }
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, key_callback);

    glewExperimental = GL_TRUE;
    if (glewInit() != GLEW_OK) {
        std::cerr << "Failed to initialize glew" << std::endl;
        glfwTerminate();
        exit(-1);
    }

    glViewport(0, 0, 800, 600);

    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();

        glClearColor(bg_color.red, bg_color.green, bg_color.blue, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        glfwSwapBuffers(window);
    }

    glfwTerminate();

    std::cout << "Hello, World!" << std::endl;
    return 0;
}
