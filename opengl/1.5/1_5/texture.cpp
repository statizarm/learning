//
// Created by art on 7/16/21.
//
#include <GL/glew.h>
#include <SOIL/SOIL.h>

#include "texture.h"

Texture load_texture(const std::filesystem::path &texture_path, GLint type) {
  GLint width, height;
  GLuint texture_id;
  glGenTextures(1, &texture_id);
  Texture tex {texture_id};

  if (!tex) {
    tex.error_msg_ = "Failed to create texture";
  } else if (GLubyte *image = SOIL_load_image(texture_path.c_str(), &width, &height, nullptr, SOIL_LOAD_RGB); !image) {
    glDeleteTextures(1, &tex.texture_id_);
    tex.texture_id_ = 0;
    tex.error_msg_ = "Failed to load image from " + texture_path.string();
  } else {
    tex.bind();

    glTexImage2D(GL_TEXTURE_2D, 0, type, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, image);
    SOIL_free_image_data(image);
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    Texture::unbind();
  }

  return tex;
}

Texture::Texture(GLuint texture_id) noexcept : texture_id_(texture_id) { }

Texture::Texture(Texture &&other) noexcept : texture_id_(other.texture_id_) {
  other.texture_id_ = 0;
}

Texture & Texture::operator=(Texture &&other) noexcept {
  if (*this) {
    glDeleteTextures(1, &this->texture_id_);
    this->texture_id_ = 0;
  }

  std::swap(this->texture_id_, other.texture_id_);
  this->texture_id_ = other.texture_id_;

  return *this;
}

Texture::~Texture() noexcept {
  if (*this) {
    glDeleteTextures(1, &this->texture_id_);
  }
}

const std::string &Texture::error_msg() const noexcept {
  return this->error_msg_;
}

void Texture::bind() const noexcept {
  glBindTexture(GL_TEXTURE_2D, this->texture_id_);
}

Texture::operator bool() const noexcept {
  return this->texture_id_ != 0;
}
