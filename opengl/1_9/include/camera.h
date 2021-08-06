//
// Created by art on 8/1/21.
//

#ifndef INC_1_9_INCLUDE_CAMERA_H_
#define INC_1_9_INCLUDE_CAMERA_H_

#include <glm/glm.hpp>

class Camera {
 public:
  Camera(glm::vec3 position, glm::vec3 target, glm::vec3 up_axis, float speed = 0.10f) noexcept;

  [[nodiscard]] glm::mat4x4 look_at() const noexcept;

  Camera &move_right(double delta_time) noexcept;
  Camera &move_left(double delta_time) noexcept;
  Camera &move_forward(double delta_time) noexcept;
  Camera &move_backward(double delta_time) noexcept;

  Camera &rotate(double dx, double dy) noexcept;

 private:
  double pitch_;
  double yaw_;

  glm::vec3 position_;
  glm::vec3 front_;
  glm::vec3 up_;

  float speed_;
};

#endif //INC_1_9_INCLUDE_CAMERA_H_
