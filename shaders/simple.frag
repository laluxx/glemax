#version 330 core
out vec4 FragColor;

in vec3 ourColor;
in vec2 TexCoord; // Receive UV coordinates

void main()
{
    // For now, just output the color. This shader doesn't yet use TexCoord.
    FragColor = vec4(ourColor, 1.0);
}

