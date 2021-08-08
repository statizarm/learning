//
// Created by art on 8/7/21.
//
#include <GL/glew.h>
#include <glm/gtc/type_ptr.hpp>

#include "renderer.h"


Renderer::~Renderer() {
  glDeleteBuffers((GLsizei) this->vbos_.size(), this->vbos_.data());
  glDeleteVertexArrays((GLsizei) this->vaos_.size(), this->vaos_.data());
}

void Renderer::render(Renderer::MeshDescriptor md,
                      const ShaderProgram &shader_program,
                      const Renderer::TextureDescriptor *texture_descriptors,
                      GLsizei n_textures,
                      const glm::mat4x4 &model,
                      const glm::mat4x4 &view,
                      const glm::mat4x4 &projection) const noexcept {
  auto description = this->mesh_render_description_[md];
  glBindVertexArray(description.vaod);
  shader_program.use();

  const auto &textures_locations = shader_program.textures_locations();
  if (n_textures > textures_locations.size()) {
    n_textures = (GLsizei) textures_locations.size();
  }

  for (GLsizei i = 0; i < n_textures; i++) {
    glActiveTexture(GL_TEXTURE0 + i);
    glBindTexture(GL_TEXTURE_2D, texture_descriptors[i]);
    glUniform1i(textures_locations[i], i);
  }

  glUniformMatrix4fv(shader_program.model_matrix_location(), 1, GL_FALSE, glm::value_ptr(model));
  glUniformMatrix4fv(shader_program.view_matrix_location(), 1, GL_FALSE, glm::value_ptr(view));
  glUniformMatrix4fv(shader_program.projection_matrix_location(), 1, GL_FALSE, glm::value_ptr(projection));

  glDrawArrays(GL_TRIANGLES, description.first, description.n_triangles);
}

Renderer::MeshDescriptor Renderer::allocate_mesh(const GLfloat *mesh_data, GLsizei size, GLsizei n_triangles) noexcept {
  auto md = (Renderer::MeshDescriptor) this->mesh_render_description_.size();

  this->mesh_render_description_.emplace_back();
  this->mesh_render_description_[md].first = 0;
  this->mesh_render_description_[md].n_triangles = n_triangles;

  glGenBuffers(1, &this->mesh_render_description_[md].vbod);
  this->vbos_.push_back(this->mesh_render_description_[md].vbod);

  glBindBuffer(GL_ARRAY_BUFFER, this->mesh_render_description_[md].vbod);
  glBufferData(GL_ARRAY_BUFFER, size, mesh_data, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  return md;
}

Renderer::AttributesDescriptor
Renderer::specify_attributes(const AttributeSpecification *specs, GLsizei size) noexcept {
  Renderer::VAODescriptor vaod;

  glGenVertexArrays(1, &vaod);
  glBindVertexArray(vaod);
  while(--size >= 0) {
    glVertexAttribPointer(specs[size].location, specs[size].size, specs[size].type, GL_FALSE, specs[size].stride, specs[size].offset);
    glEnableVertexAttribArray(specs[size].location);
  }

  auto ad = (Renderer::AttributesDescriptor) this->vaos_.size();
  this->vaos_.push_back(vaod);

  return ad;
}

void Renderer::specify_mesh_render_data(Renderer::MeshDescriptor meshd,
                                        Renderer::AttributesDescriptor attrsd) noexcept {

  this->mesh_render_description_[meshd].vaod = this->vaos_[attrsd];

  glBindVertexArray(this->mesh_render_description_[meshd].vaod);
  glBindBuffer(GL_ARRAY_BUFFER, this->mesh_render_description_[meshd].vbod);
  glBindVertexArray(0);
}


