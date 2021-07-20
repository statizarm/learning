#version 330 core

in vec3 vertex_color;
in vec2 tex_coords;
out vec4 out_color;

uniform sampler2D tex1;
uniform sampler2D tex2;
uniform float perc;

void main() {
    out_color = mix(texture(tex1, tex_coords), texture(tex2, tex_coords), perc);
}
