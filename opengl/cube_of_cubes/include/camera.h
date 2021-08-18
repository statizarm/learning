//
// Created by art on 8/15/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_CAMERA_H_
#define CUBE_OF_CUBES_INCLUDE_CAMERA_H_

#include <glm/glm.hpp>

class ViewCamera {
 public:
  enum Direction {RIGHT, LEFT, FORWARD, BACKWARD};
  ViewCamera(const glm::vec3 &position, const glm::vec3 &up, const glm::vec3 &dst) noexcept;

  ViewCamera &move(Direction direction, float delta_time) noexcept;
  ViewCamera &look_to(float yaw, float pitch) noexcept;

  ViewCamera &speed_up(float delta) noexcept;

  [[nodiscard]] glm::mat4x4 look_at() const noexcept;
 protected:
 private:
  glm::vec3 position_;
  glm::vec3 up_;
  glm::vec3 front_;
  float speed_;
};

class ProjectionCamera {
 public:
  ProjectionCamera(float fov, float viewport_width, float viewport_height, float front, float back) noexcept;

  ProjectionCamera &fov_up(float delta) noexcept;
  ProjectionCamera &set_width(float width) noexcept;
  ProjectionCamera &set_height(float height) noexcept;

  [[nodiscard]] glm::mat4x4 projection() const noexcept;

 protected:
 private:
  float fov_;
  float width_;
  float height_;
  float near_;
  float far_;
};

#endif //CUBE_OF_CUBES_INCLUDE_CAMERA_H_
