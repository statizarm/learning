//
// Created by art on 8/4/21.
//
#include "shape.h"
#include <glm/glm.hpp>

class Prototype {
 public:
  Prototype() noexcept;

 private:
  float *vertex_data_;
  size_t vertex_data_size_;


};

class Shape {
 public:
  Shape() noexcept;

  virtual ~Shape() = default;

  void move(glm::vec3 dir) noexcept;
  void rotate(glm::vec3 axis, float angle) noexcept;
  void scale(glm::vec3 scale) noexcept;
 protected:
  glm::mat4x4 model_;
};
