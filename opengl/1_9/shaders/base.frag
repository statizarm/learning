#version 330 core

in vec2 texture_coords;

out vec4 color;

uniform sampler2D tex1;
uniform sampler2D tex2;

void main() {
    color = mix(texture(tex1, texture_coords), texture(tex2, texture_coords), 0.2f);
    //color = vec4(1.0f, 1.0f, 0.0f, 1.0f);
}