//
// Created by art on 8/8/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_SHADER_H_
#define CUBE_OF_CUBES_INCLUDE_SHADER_H_

#include <filesystem>

#include <GL/glew.h>
#include <GL/gl.h>

class Shader {
 public:
  static Shader make_shader(GLenum shader_type, const std::filesystem::path &shader_source) noexcept;

  Shader() noexcept;
  Shader(Shader &&other) noexcept;

  Shader &operator=(const Shader &other) = delete;
  Shader(const Shader &other) = delete;

  ~Shader() noexcept;

  explicit operator bool() const noexcept {
    return shader_ != 0;
  }

  void attach(GLuint program) const noexcept {
    glAttachShader(program, this->shader_);
  }

  [[nodiscard]] const std::string &info_log() const noexcept {
    return info_log_;
  }

 protected:
 private:
  std::string info_log_;
  GLuint shader_;
};

#endif //CUBE_OF_CUBES_INCLUDE_SHADER_H_
