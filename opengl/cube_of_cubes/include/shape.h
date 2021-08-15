//
// Created by art on 8/4/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_SHAPE_H_
#define CUBE_OF_CUBES_INCLUDE_SHAPE_H_

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "renderer.h"
#include "texture.h"

struct Prototype {
  Prototype(Renderer::MeshDescriptor meshd, const std::vector<Texture2D *> &textures) noexcept;

  Prototype() = delete;

  GLuint *textures;
  Renderer::MeshDescriptor meshd;
  GLsizei n_textures;
};

class Shape {
 public:
  explicit Shape(const Prototype *shape_prototype) noexcept;
  Shape(const Prototype *shape_prototype, glm::vec3 position) noexcept;

  Shape() = delete;

  Shape *rotate(glm::vec3 axis, float angle) noexcept;
  Shape *move(glm::vec3 direction) noexcept;
  Shape *scale(glm::vec3 scale) noexcept;

  Shape *set_rotation(glm::vec3 axis, float angle) noexcept {
    this->rotation_ = glm::rotate(glm::mat4x4(1.0f), angle, axis);
    return this;
  }

  Shape *set_position(glm::vec3 position) noexcept {
    this->position_ = position;
    return this;
  }

  Shape *set_scale(glm::vec3 scale) noexcept {
    this->scale_ = glm::scale(glm::mat4x4(1.0f), scale);
    return this;
  }

  void render(const Renderer &renderer, const ShaderProgram &program) noexcept;

 protected:
  virtual glm::mat4x4 model() {
    return this->scale_ * glm::translate(glm::mat4x4(1.0f), this->position_) * this->rotation_;
  }

  const Prototype *prototype_;
  glm::mat4x4 rotation_;
  glm::mat4x4 scale_;
  glm::vec3 position_;
 private:
};

#endif //CUBE_OF_CUBES_INCLUDE_SHAPE_H_
