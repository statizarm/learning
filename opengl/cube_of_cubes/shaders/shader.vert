#version 330 core

layout (location = 0) in vec3 vertex_coord;
layout (location = 1) in vec2 texture_coord;

out vec2 tex_coord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    gl_Position = projection * view * model * vec4(vertex_coord, 1.0f);
    tex_coord = texture_coord;
}
