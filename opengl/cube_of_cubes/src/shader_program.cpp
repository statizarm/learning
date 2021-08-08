//
// Created by art on 8/8/21.
//

#include "shader_program.h"

ShaderProgram ShaderProgram::make_program(const Shader &vertex_shader,
                                          const Shader &fragment_shader) {
  ShaderProgram sp;
  sp.shader_program_ = glCreateProgram();

  if (!sp) {
    sp.info_log_ = "Failed to create program";
    return sp;
  }

  vertex_shader.attach(sp.shader_program_);
  fragment_shader.attach(sp.shader_program_);

  glLinkProgram(sp.shader_program_);
  GLint program_linked;
  glGetProgramiv(sp.shader_program_, GL_LINK_STATUS, &program_linked);
  if (program_linked != GL_TRUE) {
    GLchar info_log[1024];
    glGetProgramInfoLog(sp.shader_program_, 1024, nullptr, info_log);
    sp.info_log_ = info_log;

    glDeleteProgram(sp.shader_program_);
    sp.shader_program_ = 0;
  }

  return sp;
}

ShaderProgram ShaderProgram::make_program(const Shader &vertex_shader,
                                          const Shader &fragment_shader,
                                          const std::string &model_matrix_name,
                                          const std::string &view_matrix_name,
                                          const std::string &projection_matrix_name,
                                          const std::vector<std::string> &textures_names) {
  ShaderProgram sp = make_program(vertex_shader, fragment_shader);

  if (!sp) {
    return sp;
  }

  // Initialize matrices locations
  sp.matrices_locations_[ShaderProgram::MODEL_INDEX] = sp.uniform_location(model_matrix_name.c_str());
  sp.matrices_locations_[ShaderProgram::VIEW_INDEX] = sp.uniform_location(view_matrix_name.c_str());
  sp.matrices_locations_[ShaderProgram::PROJECTION_INDEX] = sp.uniform_location(projection_matrix_name.c_str());

  // Initialize textures locations
  sp.textures_locations_.resize(textures_names.size());
  auto it = sp.textures_locations_.begin();
  for (const auto &name : textures_names) {
    *it = sp.uniform_location(name.c_str());
  }

  return sp;
}

ShaderProgram::ShaderProgram() : shader_program_(0), matrices_locations_({0, 0, 0}) { }

ShaderProgram::ShaderProgram(ShaderProgram &&other) noexcept
    : textures_locations_(std::move(other.textures_locations_)), matrices_locations_(other.matrices_locations_),
      shader_program_(other.shader_program_) {
  other.shader_program_ = 0;
}

ShaderProgram::~ShaderProgram() {
  if (this->shader_program_ != 0) {
    glDeleteProgram(this->shader_program_);
  }
}
