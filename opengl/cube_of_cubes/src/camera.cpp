//
// Created by art on 8/15/21.
//

#include <iostream>
#include <glm/gtc/matrix_transform.hpp>

#include "camera.h"


ViewCamera::ViewCamera(const glm::vec3 &position, const glm::vec3 &up, const glm::vec3 &dst) noexcept
    : position_(position), up_(up), front_(glm::normalize(dst - position)), speed_(20.0f) { }

ViewCamera &ViewCamera::move(Direction direction, float delta_time) noexcept {
  switch (direction) {
    case ViewCamera::RIGHT:
      this->position_ += glm::cross(this->front_, this->up_) * this->speed_ * delta_time;
      break;
    case ViewCamera::LEFT:
      this->position_ -= glm::cross(this->front_, this->up_) * this->speed_ * delta_time;
      break;
    case ViewCamera::FORWARD:
      this->position_ += this->front_ * this->speed_ * delta_time;
      break;
    case ViewCamera::BACKWARD:
      this->position_ -= this->front_ * this->speed_ * delta_time;
      break;
    default:
      break;
  }

  std::cout << "Camera position: " << this->position_[0] << " " << this->position_[1] << " " << this->position_[2] << std::endl;
  return *this;
}

ViewCamera &ViewCamera::look_to(float yaw, float pitch) noexcept {
  return *this;
}

glm::mat4x4 ViewCamera::look_at() const noexcept {
  return glm::lookAt(this->position_, this->front_ + this->position_, this->up_);
}
ProjectionCamera::ProjectionCamera(float fov,
                                   float viewport_width,
                                   float viewport_height,
                                   float near,
                                   float far) noexcept
    : fov_(fov), width_(viewport_width), height_(viewport_height), near_(near), far_(far) { }

ProjectionCamera &ProjectionCamera::fov_up(float delta) noexcept {
  this->fov_ += delta;
  return *this;
}

ProjectionCamera &ProjectionCamera::set_width(float width) noexcept {
  this->width_ = width;
  return *this;
}

ProjectionCamera &ProjectionCamera::set_height(float height) noexcept {
  this->height_ = height;
  return *this;
}

glm::mat4x4 ProjectionCamera::projection() const noexcept {
  return glm::perspective(this->fov_, this->width_ / this->height_, this->near_, this->far_);
}
