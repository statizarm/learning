//
// Created by art on 8/1/21.
//
#include "camera.h"
#include "glm/gtc/matrix_transform.hpp"

Camera::Camera(glm::vec3 position, glm::vec3 destination, glm::vec3 up_axis, float speed) noexcept
    : position_(position), front_(glm::normalize(destination - position)), up_(up_axis), speed_(speed)
{
  this->pitch_ = asin(this->front_.y);
  this->yaw_ = asin(this->front_.z / cos(this->pitch_));
}

glm::mat4x4 Camera::look_at() const noexcept {
  return glm::lookAt(this->position_, this->front_ + this->position_, this->up_);
}

Camera &Camera::move_right(double delta_time) noexcept {
  this->position_ -= glm::normalize(glm::cross(this->up_, this->front_)) * (float) delta_time * this->speed_;
  return *this;
}

Camera &Camera::move_left(double delta_time) noexcept {
  this->position_ += glm::normalize(glm::cross(this->up_, this->front_)) * (float) delta_time * this->speed_;
  return *this;
}

Camera &Camera::move_forward(double delta_time) noexcept {
  this->position_ += this->front_ * (float) delta_time * this->speed_;
  return *this;
}

Camera &Camera::move_backward(double delta_time) noexcept {
  this->position_ -= this->front_ * (float) delta_time * this->speed_;
  return *this;
}

Camera &Camera::rotate(double dx, double dy) noexcept {

  this->yaw_ += asin(dx);
  this->pitch_ += asin(dy);

  if (this->pitch_ > glm::pi<float>() / 2) {
    this->pitch_ = glm::pi<float>() / 2.01;
  } else if (this->pitch_ < -glm::pi<float> () / 2.0f) {
    this->pitch_ = -glm::pi<float>() / 2.01f;
  }

  this->front_.x = cos(this->yaw_) * cos(this->pitch_);
  this->front_.y = sin(this->pitch_);
  this->front_.z = sin(this->yaw_) * cos(this->pitch_);
  this->front_ = glm::normalize(this->front_);

  return *this;
}

