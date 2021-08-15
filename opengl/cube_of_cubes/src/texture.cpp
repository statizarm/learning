//
// Created by art on 8/9/21.
//
#include "texture.h"
#include <SOIL/SOIL.h>

Texture2D Texture2D::make_texture(const std::filesystem::path &path) noexcept {
  GLuint td;
  glGenTextures(1, &td);
  if (td == 0) {
    Texture2D texture {0};
    texture.info_log_ = "Failed to create texture object";
    return texture;
  }
  Texture2D texture {td};
  int width, height;
  uint8_t *image_data = SOIL_load_image(path.c_str(), &width, &height, nullptr, SOIL_LOAD_RGB);
  if (image_data == nullptr) {
    texture.info_log_ = "Failed to load image: " + path.string();
    glDeleteTextures(1, &texture.texture_);
    texture.texture_ = 0;
    return texture;
  }

  glBindTexture(GL_TEXTURE_2D, texture.texture_);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  // Set texture filtering
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, image_data);
  glGenerateMipmap(texture.texture_);
  glBindTexture(GL_TEXTURE_2D, 0);
  SOIL_free_image_data(image_data);

  return texture;
}

Texture2D::Texture2D(Texture2D &&other) noexcept : texture_(other.texture_), info_log_(std::move(other.info_log_)) {
  other.texture_ = 0;
}

Texture2D::~Texture2D() noexcept {
  if (this->texture_ != 0) {
    glDeleteTextures(1, &this->texture_);
  }
}

Texture2D::Texture2D(GLuint texture) noexcept : texture_(texture) { }
