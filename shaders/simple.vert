#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
layout (location = 2) in vec2 aTexCoord; // UV coordinates

uniform mat4 projectionMatrix;

out vec3 ourColor;
out vec2 TexCoord; // Pass UV coordinates to the fragment shader

void main()
{
    gl_Position = projectionMatrix * vec4(aPos, 1.0);
    ourColor = aColor;
    TexCoord = aTexCoord; // Pass UV coordinates
}
