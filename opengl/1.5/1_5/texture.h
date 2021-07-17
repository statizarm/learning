//
// Created by art on 7/16/21.
//

#ifndef INC_1_5__TEXTURE_H_
#define INC_1_5__TEXTURE_H_

#include <filesystem>
#include <string>
#include <GL/gl.h>

class Texture {
 public:
  friend Texture load_texture(const std::filesystem::path &texture_path, GLint type);

  Texture() = delete;
  Texture(const Texture &other) = delete;
  Texture(Texture &&other) noexcept;

  Texture &operator=(const Texture &other) = delete;
  Texture &operator=(Texture &&other) noexcept;

  ~Texture() noexcept;

  [[nodiscard]] const std::string &error_msg() const noexcept;
  void bind() const noexcept;
  static void unbind() noexcept { glBindTexture(GL_TEXTURE_2D, 0); }

  explicit operator bool() const noexcept;

 private:
  explicit Texture(GLuint texture_id) noexcept;

  std::string error_msg_;
  GLuint texture_id_;
};

Texture load_texture(const std::filesystem::path &texture_path, GLint type = GL_RGB);

#endif //INC_1_5__TEXTURE_H_
