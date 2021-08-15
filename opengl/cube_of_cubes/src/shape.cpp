//
// Created by art on 8/4/21.
//
#include "shape.h"

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

void Shape::render(const Renderer &renderer, const ShaderProgram &program) noexcept {
  renderer.render(this->prototype_->meshd,
                  program,
                  this->prototype_->textures,
                  this->prototype_->n_textures,
                  this->model());
}