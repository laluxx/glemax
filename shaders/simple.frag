#version 330 core
out vec4 FragColor;

in vec4 ourColor; // Receives RGBA from vertex shader
in vec2 TexCoord;

void main()
{
    FragColor = ourColor; // Use original alpha
//    FragColor = vec4(ourColor.rgb, 0.5); // Uncomment to force 50% alpha
}

