#version 330 core

layout (location = 0) in vec3 vertex_coords;
layout (location = 1) in vec2 tex_coords;

out vec2 texture_coords;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    gl_Position = projection * view * model * vec4(vertex_coords, 1.0f);
    //gl_Position = vec4(vertex_coords, 1.0f);
    texture_coords = tex_coords;
}
