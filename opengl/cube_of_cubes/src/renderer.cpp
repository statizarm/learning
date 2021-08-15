//
// Created by art on 8/7/21.
//
#include <iostream>

#include <GL/glew.h>
#include <glm/gtc/type_ptr.hpp>

#include "renderer.h"

Renderer::Renderer(ViewCamera *view, ProjectionCamera *projection) noexcept
    : view_(view), projection_(projection) { }

Renderer::~Renderer() {
  glDeleteBuffers((GLsizei) this->vbos_.size(), this->vbos_.data());
  glDeleteVertexArrays((GLsizei) this->vaos_.size(), this->vaos_.data());
}

void Renderer::render(Renderer::MeshDescriptor md,
                      const ShaderProgram &shader_program,
                      const Renderer::TextureDescriptor *texture_descriptors,
                      GLsizei n_textures,
                      const glm::mat4x4 &model) const noexcept {
  auto description = this->mesh_render_description_[md];
  shader_program.use();
  glBindVertexArray(description.vaod);

  const auto &textures_locations = shader_program.textures_locations();
  if (n_textures > textures_locations.size()) {
    n_textures = (GLsizei) textures_locations.size();
  }

  for (GLsizei i = 0; i < n_textures; i++) {
    glActiveTexture(GL_TEXTURE0 + i);
    glBindTexture(GL_TEXTURE_2D, texture_descriptors[i]);
    glUniform1i(textures_locations[i], i);
  }

  auto &&view = glm::lookAt(glm::vec3(0.0f, 0.0f, 3.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 1.0f, 0.0f));
  auto &&projection = this->projection_->projection();
  glUniformMatrix4fv(shader_program.model_matrix_location(), 1, GL_FALSE, glm::value_ptr(model));
  glUniformMatrix4fv(shader_program.view_matrix_location(), 1, GL_FALSE, glm::value_ptr(view));
  glUniformMatrix4fv(shader_program.projection_matrix_location(), 1, GL_FALSE, glm::value_ptr(projection));

  glDrawArrays(GL_TRIANGLES, description.first, description.n_vertices);
  //std::cout << glGetError() << std::endl;

  glBindVertexArray(0);
}

Renderer::MeshDescriptor Renderer::specify_mesh(const GLfloat *mesh_data,
                                                GLsizei mesh_size,
                                                GLsizei n_vertices,
                                                const AttributeSpecification *specs,
                                                GLsizei n_attributes) noexcept {
  auto md = (Renderer::MeshDescriptor) this->mesh_render_description_.size();

  this->mesh_render_description_.emplace_back();
  this->mesh_render_description_[md].first = 0;
  this->mesh_render_description_[md].n_vertices = n_vertices;

  GLuint vbod;
  glGenBuffers(1, &vbod);
  glGenVertexArrays(1, &this->mesh_render_description_[md].vaod);

  this->vbos_.push_back(vbod);
  this->vaos_.push_back(this->mesh_render_description_[md].vaod);

  glBindVertexArray(this->mesh_render_description_[md].vaod);

  glBindBuffer(GL_ARRAY_BUFFER, vbod);
  glBufferData(GL_ARRAY_BUFFER, mesh_size, mesh_data, GL_STATIC_DRAW);

  for (int i = 0; i < n_attributes; ++i) {
    glVertexAttribPointer(specs[i].location, specs[i].size, specs[i].type, GL_FALSE, specs[i].stride, specs[i].offset);
    glEnableVertexAttribArray(specs[i].location);
  }

  glBindVertexArray(0);
  return md;
}
