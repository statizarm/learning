//
// Created by art on 7/4/21.
//
#include <iostream>
#include <GL/glew.h>
#include <fstream>
#include <vector>
#include "shader.h"

Shader::Shader() noexcept : shader_id_(0) { }

Shader::Shader(GLuint shader_id) noexcept : shader_id_(shader_id) { }

[[maybe_unused]] Shader::Shader(Shader &&other) noexcept : shader_id_(other.shader_id_) {
  other.shader_id_ = 0;
}

Shader &Shader::operator=(Shader &&other) noexcept {
  this->shader_id_ = other.shader_id_;
  other.shader_id_ = 0;
  return *this;
}

Shader::~Shader() noexcept {
  if (this->shader_id_ != 0) {
    glDeleteShader(this->shader_id_);
  }
}

GLuint Shader::get_descriptor() const {
  return this->shader_id_;
}

Shader compile_shader(GLenum shader_type, const std::filesystem::path &source_path) {
  Shader shader {glCreateShader(shader_type)};

  if (shader.shader_id_ == 0) {
    std::cerr << "FAILED to create shader with type id: " << shader_type;
    return Shader();
  }

  std::ifstream source_in(source_path, std::ios::in);

  if (!source_in.is_open()) {
    std::cerr << "FAILED to open file: " << std::filesystem::absolute(source_path) << std::endl;
    return Shader();
  }

  std::vector<char *> src_lines;

  do {
    char *buf = new char[1024];
    buf[source_in.read(buf, 1023).gcount()] = '\0';
    src_lines.push_back(buf);
  } while(source_in);

  glShaderSource(shader.shader_id_, static_cast<GLint>(src_lines.size()), src_lines.data(), nullptr);
  glCompileShader(shader.shader_id_);

  for (char *b: src_lines) {
    delete[] b;
  }

  GLint success;
  glGetShaderiv(shader.shader_id_, GL_COMPILE_STATUS, &success);
  if (success == GL_FALSE) {
    char info_log[512];
    glGetShaderInfoLog(shader.shader_id_, 512, nullptr, info_log);
    std::cerr << "FAILED to compile shader" << std::endl;
    std::cerr << info_log << std::endl;

    return Shader();
  }

  return shader;
}
