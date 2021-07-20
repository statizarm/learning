//
// Created by art on 7/13/21.
//

#ifndef INC_1_5__SHADER_H_
#define INC_1_5__SHADER_H_

#include <filesystem>

#include <GL/gl.h>

class Shader {
 public:
  friend Shader compile_shader(GLenum shader_type, const std::filesystem::path &shader_source) noexcept;

  Shader() noexcept;
  Shader(const Shader &other) = delete;
  Shader(Shader &&other) noexcept;

  Shader &operator=(Shader &&other) noexcept;
  Shader &operator=(const Shader &other) = delete;

  ~Shader() noexcept;

  [[nodiscard, maybe_unused]] GLuint shader_id() const noexcept;
  [[nodiscard, maybe_unused]] const std::string &error_msg() const noexcept;

  explicit operator bool() const noexcept;
  operator GLuint() const noexcept;

 private:
  explicit Shader(GLuint shader_id) noexcept;
  Shader &operator=(GLuint shader_id) noexcept;

  std::string error_msg_;
  GLuint shader_id_;
};

class ShaderProgram {
 public:
  ShaderProgram() = delete;
  ShaderProgram(const std::filesystem::path &vertex_shader, const std::filesystem::path &fragment_shader) noexcept;
  ShaderProgram(const std::filesystem::path &vertex_shader,
                const std::filesystem::path &geom_shader,
                const std::filesystem::path &fragment_shader) noexcept;
  ShaderProgram(const ShaderProgram &other) = delete;
  ShaderProgram(ShaderProgram &&other) noexcept;

  ShaderProgram &operator=(ShaderProgram &&other) noexcept;
  ShaderProgram &operator=(const ShaderProgram &other) = delete;

  ~ShaderProgram() noexcept;

  void use() const noexcept;
  GLint uniform_location(std::string_view uniform_name) const noexcept;

  [[nodiscard]] const std::string &error_msg() const noexcept;

  explicit operator bool() const noexcept;
 private:
  void link_program(std::size_t n_shaders) noexcept;

  Shader shaders_[3];
  std::string error_msg_;
  GLuint program_id_;
};

[[maybe_unused]] Shader compile_shader(GLenum shader_type, const std::filesystem::path &shader_source) noexcept;

#endif //INC_1_5__SHADER_H_
