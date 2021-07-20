//
// Created by art on 7/4/21.
//

#ifndef INC_1_3__SHADER_H_
#define INC_1_3__SHADER_H_

#include <filesystem>
#include <GL/gl.h>

class Shader {
 public:
  Shader() noexcept;
  Shader(Shader &&other) noexcept;
  Shader &operator=(Shader &&other) noexcept;

  ~Shader() noexcept;

  [[nodiscard]] GLuint get_descriptor() const;

  friend Shader compile_shader(GLenum shader_type, const std::filesystem::path &source_path);

  Shader(const Shader &other) = delete;
 private:
  explicit Shader(GLuint shader_id) noexcept;

  GLuint shader_id_;
};

Shader compile_shader(GLenum shader_type, const std::filesystem::path &source_path);

#endif //INC_1_3__SHADER_H_
