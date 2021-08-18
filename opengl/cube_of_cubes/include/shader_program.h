//
// Created by art on 8/8/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_SHADER_PROGRAM_H_
#define CUBE_OF_CUBES_INCLUDE_SHADER_PROGRAM_H_

#include "shader.h"

#include <filesystem>
#include <vector>
#include <string>

#include <GL/glew.h>
#include <GL/gl.h>


class ShaderProgram {
 public:
  [[nodiscard]] static ShaderProgram make_program(const Shader &vertex_shader,
                                                  const Shader &fragment_shader);

  [[nodiscard]] static ShaderProgram make_program(const Shader &vertex_shader,
                                                  const Shader &fragment_shader,
                                                  const std::string &model_matrix_name,
                                                  const std::string &view_matrix_name,
                                                  const std::string &projection_matrix_name,
                                                  const std::vector<std::string> &textures_names);

  ShaderProgram();

  ShaderProgram(const ShaderProgram &other) = delete;
  ShaderProgram &operator=(const ShaderProgram &other) = delete;

  ShaderProgram(ShaderProgram &&other) noexcept;

  ~ShaderProgram();

  explicit operator bool() const noexcept {
    return this->shader_program_ != 0;
  }

  [[nodiscard]] GLint model_matrix_location() const noexcept {
    return this->matrices_locations_[ShaderProgram::MODEL_INDEX];
  }
  [[nodiscard]] GLint view_matrix_location() const noexcept {
    return this->matrices_locations_[ShaderProgram::VIEW_INDEX];
  }
  [[nodiscard]] GLint projection_matrix_location() const noexcept {
    return this->matrices_locations_[ShaderProgram::PROJECTION_INDEX];
  }
  [[nodiscard]] const std::vector<GLint> &textures_locations() const noexcept {
    return this->textures_locations_;
  }

  [[nodiscard]] GLint uniform_location(const char *uniform_name) const noexcept {
    return glGetUniformLocation(this->shader_program_, uniform_name);
  }

  [[nodiscard]] const std::string &info_log() const noexcept {
    return this->info_log_;
  }

  void use() const noexcept {
    glUseProgram(this->shader_program_);
  }

 protected:
 private:
  enum matrices_indexes {MODEL_INDEX = 0, VIEW_INDEX, PROJECTION_INDEX};

  std::vector<GLint> textures_locations_;
  std::string info_log_;
  std::array<GLint, 3> matrices_locations_;
  GLuint shader_program_;
};

#endif //CUBE_OF_CUBES_INCLUDE_SHADER_PROGRAM_H_
