//
// Created by art on 8/6/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_RENDERER_H_
#define CUBE_OF_CUBES_INCLUDE_RENDERER_H_

#include <vector>
#include <unordered_map>

#include <GL/gl.h>
#include <glm/glm.hpp>

#include "shader_program.h"

struct AttributeSpecification {
  AttributeSpecification(GLuint location, GLsizei size, GLenum type, GLsizei stride, GLvoid *offset)
      : offset(offset), size(size), stride(stride), location(location), type(type) { }
  GLvoid *offset;
  GLsizei size;
  GLsizei stride;
  GLuint location;
  GLenum type;
};

class Renderer {
 public:
  using MeshDescriptor = GLint;
  using TextureDescriptor = GLuint;
  using AttributesDescriptor = GLint;

  Renderer() = default;

  ~Renderer();

  void render(MeshDescriptor md,
              const ShaderProgram &shader_program,
              const TextureDescriptor *texture_descriptors,
              GLsizei n_textures,
              const glm::mat4x4 &model,
              const glm::mat4x4 &view,
              const glm::mat4x4 &projection) const noexcept;

  [[nodiscard]] MeshDescriptor allocate_mesh(const GLfloat *mesh_data, GLsizei size, GLsizei n_triangles) noexcept;
  [[nodiscard]] AttributesDescriptor specify_attributes(const AttributeSpecification *specs, GLsizei size) noexcept;
  void specify_mesh_render_data(MeshDescriptor meshd,
                                AttributesDescriptor attrsd) noexcept;

 private:
  using VAODescriptor = GLuint;
  using VBODescriptor = GLuint;

  struct RenderDescription {
    GLsizei n_triangles;
    VAODescriptor vaod;
    VBODescriptor vbod;
    GLint first;
  };

  std::vector<RenderDescription> mesh_render_description_;
  std::vector<VAODescriptor> vaos_;
  std::vector<VBODescriptor> vbos_;
};

#endif //CUBE_OF_CUBES_INCLUDE_RENDERER_H_
