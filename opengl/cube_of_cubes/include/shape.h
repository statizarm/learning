//
// Created by art on 8/4/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_SHAPE_H_
#define CUBE_OF_CUBES_INCLUDE_SHAPE_H_

#include "renderer.h"
#include "texture.h"

#include <unordered_map>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>


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

  virtual ~Shape() = default;

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

  virtual void render(const Renderer &renderer, const ShaderProgram &program) const noexcept;
  [[nodiscard]] virtual Shape *clone() const noexcept;

 protected:
  const Prototype *prototype_;
  glm::mat4x4 rotation_;
  glm::mat4x4 scale_;
  glm::vec3 position_;

 private:
};

class ShapeSystem : public Shape {
 public:
  ShapeSystem(const Shape *shape_proto, float distance) noexcept;

  ~ShapeSystem() noexcept override;

  void render(const Renderer &renderer, const ShaderProgram &program) const noexcept override;
  [[nodiscard]] Shape *clone() const noexcept override;
 protected:
 private:
  std::unordered_map<Shape *, glm::vec4> positions_;
  glm::vec3 rotation_vector_;
  Shape *shapes_[8];
  float distance_;
};

#endif //CUBE_OF_CUBES_INCLUDE_SHAPE_H_
