#version 330 core

in vec2 tex_coord;
out vec4 color;

uniform sampler2D texture1;
uniform sampler2D texture2;

void main() {
    color = texture(texture1, tex_coord);
}
