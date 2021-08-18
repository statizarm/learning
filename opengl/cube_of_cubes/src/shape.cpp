//
// Created by art on 8/4/21.
//
#include "shape.h"

#include <cstdlib>

#include <iostream>

#include <GLFW/glfw3.h>


Prototype::Prototype(Renderer::MeshDescriptor meshd, const std::vector<Texture2D *> &textures) noexcept
    : meshd(meshd), n_textures((GLsizei) textures.size()), textures(new GLuint[textures.size()]) {
  if (this->textures != nullptr) {
    for (int i = 0; i < n_textures; ++i) {
      this->textures[i] = textures[i]->texture();
    }
  }
}

Shape::Shape(const Prototype *shape_prototype) noexcept
    : prototype_(shape_prototype), position_(0.0f, 0.0f, 0.0f), rotation_(1.0f), scale_(1.0f) { }

Shape::Shape(const Prototype *shape_prototype, glm::vec3 position) noexcept
    : prototype_(shape_prototype), position_(position), rotation_(1.0f), scale_(1.0f) { }

Shape *Shape::move(glm::vec3 direction) noexcept {
  this->position_ = direction + this->position_;
  return this;
}

Shape *Shape::rotate(glm::vec3 axis, float angle) noexcept {
  this->rotation_ = glm::rotate(this->rotation_, angle, axis);
  return this;
}

Shape *Shape::scale(glm::vec3 scale) noexcept {
  this->scale_ = glm::scale(this->scale_, scale);
  return this;
}

void Shape::render(const Renderer &renderer, const ShaderProgram &program) const noexcept {
  auto model = this->scale_ * glm::translate(glm::mat4x4(1.0f), this->position_) * this->rotation_;
  renderer.render(this->prototype_->meshd,
                  program,
                  this->prototype_->textures,
                  this->prototype_->n_textures,
                  model);
}

Shape *Shape::clone() const noexcept {
  auto *new_shape = new Shape(this->prototype_, this->position_);

  new_shape->rotation_ = this->rotation_;
  new_shape->scale_ = this->scale_;

  return new_shape;
}

ShapeSystem::ShapeSystem(const Shape *shape_proto, float distance) noexcept
    : Shape(nullptr), shapes_(), positions_(), distance_(distance) {
  srand((unsigned int) glfwGetTime());
  for (auto &s : this->shapes_) {
    s = shape_proto->clone();
  }

  auto half_distance = distance / 2;
  // front
  this->positions_[this->shapes_[0]] = glm::vec4(-half_distance, -half_distance, half_distance, 1.0f);
  this->positions_[this->shapes_[1]] = glm::vec4(-half_distance, half_distance, half_distance, 1.0f);
  this->positions_[this->shapes_[2]] = glm::vec4(half_distance, half_distance, half_distance, 1.0f);
  this->positions_[this->shapes_[3]] = glm::vec4(half_distance, -half_distance, half_distance, 1.0f);
  //back
  this->positions_[this->shapes_[4]] = glm::vec4(-half_distance, -half_distance, -half_distance, 1.0f);
  this->positions_[this->shapes_[5]] = glm::vec4(-half_distance, half_distance, -half_distance, 1.0f);
  this->positions_[this->shapes_[6]] = glm::vec4(half_distance, half_distance, -half_distance, 1.0f);
  this->positions_[this->shapes_[7]] = glm::vec4(half_distance, -half_distance, -half_distance, 1.0f);

  this->rotation_vector_ = glm::normalize(glm::vec3((float) rand(), (float) rand(), (float) rand()));
}

ShapeSystem::~ShapeSystem() noexcept {
  for (auto s : this->shapes_) {
    delete s;
  }
}

void ShapeSystem::render(const Renderer &renderer, const ShaderProgram &program) const noexcept {
  float angle = glm::radians((float) glfwGetTime()) * 10.0f;
  auto model = this->scale_ * glm::translate(glm::mat4x4(1.0f), this->position_) * this->rotation_;

  for (auto *s : this->shapes_) {
    const auto &pos = this->positions_.find(s)->second;
    s->set_position(model * pos);
    s->set_rotation(this->rotation_vector_, angle);
    s->render(renderer, program);
  }
}

Shape *ShapeSystem::clone() const noexcept {
  auto *new_shape = new ShapeSystem(this->shapes_[0], this->distance_);

  new_shape->position_ = this->position_;
  new_shape->rotation_ = this->rotation_;
  new_shape->scale_ = this->scale_;

  return new_shape;
}