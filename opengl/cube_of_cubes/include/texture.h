//
// Created by art on 8/9/21.
//

#ifndef CUBE_OF_CUBES_INCLUDE_TEXTURE_H_
#define CUBE_OF_CUBES_INCLUDE_TEXTURE_H_

#include <filesystem>

#include <GL/glew.h>

class Texture2D {
 public:
  static Texture2D make_texture(const std::filesystem::path &path) noexcept;

  Texture2D(Texture2D &&other) noexcept;

  Texture2D() = delete;
  Texture2D(const Texture2D &) = delete;

  ~Texture2D() noexcept;

  [[nodiscard]] GLuint texture() const noexcept {
    return this->texture_;
  }
  [[nodiscard]] const std::string &info_log() const noexcept {
    return this->info_log_;
  }

  explicit operator bool() const noexcept {
    return this->texture_ != 0;
  }

 protected:
 private:
  explicit Texture2D(GLuint texture) noexcept;

  std::string info_log_;
  GLuint texture_;
};

#endif //CUBE_OF_CUBES_INCLUDE_TEXTURE_H_
