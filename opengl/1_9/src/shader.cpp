//
// Created by art on 7/13/21.
//
#include <fstream>
#include <vector>

#include <GL/glew.h>

#include "shader.h"

[[maybe_unused]] Shader compile_shader(GLenum shader_type, const std::filesystem::path &shader_source) noexcept {
  Shader shader {glCreateShader(shader_type)};
  if (!shader) {
    shader.error_msg_ = "Failed to create shader";
    return shader;
  }

  std::ifstream source_input {shader_source};
  if (!source_input.is_open()) {
    shader.error_msg_ = std::string {"File \""} + shader_source.string() + std::string {"\" not found"};
    return shader;
  }

  std::vector<char *> source_lines;
  do {
    char *line = new char[512];
    source_lines.push_back(line);
    line[source_input.read(line, 511).gcount()] = '\0';
  } while (source_input);

  glShaderSource(shader, static_cast<GLsizei>(source_lines.size()), source_lines.data(), nullptr);

  for (auto line : source_lines) {
    delete[] line;
  }

  glCompileShader(shader);

  GLint success;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
  if (success == GL_FALSE) {
    char info_log[512];
    glGetShaderInfoLog(shader, 512, nullptr, info_log);

    glDeleteShader(shader);
    shader.shader_id_ = 0;
    shader.error_msg_ = info_log;
    return shader;
  }

  return shader;
}

Shader::Shader() noexcept : shader_id_(0), error_msg_() { }

Shader::Shader(Shader &&other) noexcept : shader_id_(other.shader_id_), error_msg_(std::move(other.error_msg_)) {
  other.shader_id_ = 0;
}

Shader &Shader::operator=(Shader &&other) noexcept {
  this->shader_id_ = other.shader_id_;
  this->error_msg_ = std::move(other.error_msg_);

  other.shader_id_ = 0;

  return *this;
}

Shader::~Shader() noexcept {
  if (this->shader_id_ != 0) {
    glDeleteShader(this->shader_id_);
  }
}

GLuint Shader::shader_id() const noexcept {
  return this->shader_id_;
}

const std::string &Shader::error_msg() const noexcept {
  return this->error_msg_;
}

Shader::operator GLuint() const noexcept {
  return this->shader_id_;
}

Shader::operator bool() const noexcept {
  return this->shader_id_ != 0;
}

Shader::Shader(GLuint shader_id) noexcept : shader_id_(shader_id) { }

Shader &Shader::operator=(GLuint shader_id) noexcept {
  this->shader_id_ = shader_id;

  return *this;
}

ShaderProgram::ShaderProgram(const std::filesystem::path &vertex_shader,
                             const std::filesystem::path &fragment_shader) noexcept : program_id_(glCreateProgram()){
  if (this->program_id_ == 0) {
    this->error_msg_ = "Failed to create shader program";
  } else if (Shader vs = compile_shader(GL_VERTEX_SHADER, vertex_shader); !vs) {
    this->error_msg_ = "Failed to compile vertex shader due to:\n" + vs.error_msg();
    glDeleteProgram(this->program_id_);
    this->program_id_ = 0;
  } else if (Shader fs = compile_shader(GL_FRAGMENT_SHADER, fragment_shader); !fs) {
    this->error_msg_ = "Failed to compile fragment shader due to:\n" + fs.error_msg();
    glDeleteProgram(this->program_id_);
    this->program_id_ = 0;
  } else {
    this->shaders_[0] = std::move(vs);
    this->shaders_[1] = std::move(fs);

    this->link_program(2);
  }
}

ShaderProgram::ShaderProgram(const std::filesystem::path &vertex_shader,
                             const std::filesystem::path &geom_shader,
                             const std::filesystem::path &fragment_shader) noexcept : program_id_(glCreateProgram()) {
  if (this->program_id_ == 0) {
    this->error_msg_ = "Failed to create shader program";
  } else if (Shader vs = compile_shader(GL_VERTEX_SHADER, vertex_shader); !vs) {
    this->error_msg_ = "Failed to compile vertex shader due to:\n" + vs.error_msg();
    glDeleteProgram(this->program_id_);
    this->program_id_ = 0;
  } else if (Shader gs = compile_shader(GL_GEOMETRY_SHADER, geom_shader); !gs) {
    this->error_msg_ = "Failed to compile geometry shader due to:\n" + gs.error_msg();
    glDeleteProgram(this->program_id_);
    this->program_id_ = 0;
  } else if (Shader fs = compile_shader(GL_FRAGMENT_SHADER, fragment_shader); !fs) {
    this->error_msg_ = "Failed to compile fragment shader due to:\n" + fs.error_msg();
    glDeleteProgram(this->program_id_);
    this->program_id_ = 0;
  } else {
    this->shaders_[0] = std::move(vs);
    this->shaders_[1] = std::move(gs);
    this->shaders_[2] = std::move(fs);

    this->link_program(3);
  }
}

ShaderProgram::ShaderProgram(ShaderProgram &&other) noexcept
    : program_id_(other.program_id_), error_msg_(std::move(other.error_msg_)) {
  other.program_id_ = 0;

  for (int i = 0; i < 3; ++i) {
    this->shaders_[i] = std::move(other.shaders_[i]);
  }
}

ShaderProgram &ShaderProgram::operator=(ShaderProgram &&other) noexcept {
  this->program_id_ = other.program_id_;
  other.program_id_ = 0;

  this->error_msg_ = std::move(other.error_msg_);

  for (int i = 0; i < 3; ++i) {
    this->shaders_[i] = std::move(other.shaders_[i]);
  }
  return *this;
}

ShaderProgram::~ShaderProgram() noexcept {
  if (this->program_id_ != 0) {
    glDeleteProgram(this->program_id_);
  }
}

void ShaderProgram::use() const noexcept {
  glUseProgram(this->program_id_);
}

GLint ShaderProgram::uniform_location(std::string_view uniform_name) const noexcept {
  return glGetUniformLocation(this->program_id_, uniform_name.data());
}

const std::string &ShaderProgram::error_msg() const noexcept {
  return this->error_msg_;
}

ShaderProgram::operator bool() const noexcept {
  return this->program_id_ != 0;
}

void ShaderProgram::link_program(std::size_t n_programs) noexcept {
  while (n_programs-- > 0) {
    glAttachShader(this->program_id_, this->shaders_[n_programs]);
  }
  glLinkProgram(this->program_id_);

  GLint success;
  glGetProgramiv(this->program_id_, GL_LINK_STATUS, &success);
  if (success == GL_FALSE) {
    char info_log[512];
    glGetProgramInfoLog(this->program_id_, 512, nullptr, info_log);
    this->error_msg_ = "Failed to link program due to:\n" + std::string {info_log};
    glDeleteProgram(this->program_id_);
    this->program_id_ = 0;
  }
}
