//
// Created by art on 8/8/21.
//
#include <fstream>
#include <vector>

#include "shader.h"

Shader Shader::make_shader(GLenum shader_type, const std::filesystem::path &shader_source) noexcept {
  Shader s;
  s.shader_ = glCreateShader(shader_type);
  if (s.shader_ == 0) {
    s.info_log_ = "Failed to create shader";
    return s;
  }

  std::fstream source_input(shader_source, std::ios_base::in);
  if (!source_input.is_open()) {
    s.info_log_ = "Failed to open source file: " + shader_source.string();
    glDeleteShader(s.shader_);
    s.shader_ = 0;
    return s;
  }

  std::vector<char *> source_lines;
  do {
    char *buf = new char[64];
    buf[source_input.read(buf, 63).gcount()] = '\0';
    source_lines.push_back(buf);
  } while(source_input);

  glShaderSource(s.shader_, (GLsizei) source_lines.size(), source_lines.data(), nullptr);
  glCompileShader(s.shader_);

  GLint shader_compiled;
  glGetShaderiv(s.shader_, GL_COMPILE_STATUS, &shader_compiled);
  if (shader_compiled != GL_TRUE) {
    char info_log[1024];
    glGetShaderInfoLog(s.shader_, 1024, nullptr, info_log);
    glDeleteShader(s.shader_);
    s.shader_ = 0;
    s.info_log_ = info_log;
  }

  return s;
}

Shader::Shader() noexcept : shader_(0) { }

Shader::Shader(Shader &&other) noexcept : shader_(other.shader_), info_log_(std::move(other.info_log_)) {
  other.shader_ = 0;
}

Shader::~Shader() noexcept {
  if (this->shader_ != 0) {
    glDeleteShader(this->shader_);
  }
}
