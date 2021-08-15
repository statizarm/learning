//
// Created by art on 8/6/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_RENDERER_H_
#define CUBE_OF_CUBES_INCLUDE_RENDERER_H_

#include "shader_program.h"
#include "camera.h"

#include <vector>
#include <unordered_map>

#include <GL/gl.h>
#include <glm/glm.hpp>


struct AttributeSpecification {
  AttributeSpecification(GLuint location, GLsizei size, GLenum type, GLsizei stride, GLvoid *offset) noexcept
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

  Renderer(ViewCamera *view, ProjectionCamera *projection) noexcept;

  ~Renderer();

  void render(MeshDescriptor md,
              const ShaderProgram &shader_program,
              const TextureDescriptor *texture_descriptors,
              GLsizei n_textures,
              const glm::mat4x4 &model) const noexcept;

  [[nodiscard]] MeshDescriptor specify_mesh(const GLfloat *mesh_data,
                                            GLsizei mesh_size,
                                            GLsizei n_vertices,
                                            const AttributeSpecification *specs,
                                            GLsizei n_attributes) noexcept;

 private:
  using VAODescriptor = GLuint;
  using VBODescriptor = GLuint;

  struct RenderDescription {
    GLsizei n_vertices;
    VAODescriptor vaod;
    GLint first;
  };

  std::vector<RenderDescription> mesh_render_description_;
  std::vector<VAODescriptor> vaos_;
  std::vector<VBODescriptor> vbos_;
  ViewCamera *view_;
  ProjectionCamera *projection_;
};

#endif //CUBE_OF_CUBES_INCLUDE_RENDERER_H_
