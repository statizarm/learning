#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 color;
layout (location = 2) in vec2 texture_coords;

out vec3 vertex_color;
out vec2 tex_coords;

void main() {
    vertex_color = vec3(0.4f, 0.4f, 0.0f);
    tex_coords = texture_coords;
    gl_Position = vec4(position, 1.0f);
}
