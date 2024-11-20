#version 330 core
in vec2 TexCoord; // Received from the vertex shader
in vec3 ourColor; // Received from the vertex shader, optional for tinting

out vec4 FragColor;

uniform sampler2D ourTexture; // Texture sampler

void main() {
    vec4 texColor = texture(ourTexture, TexCoord); // Sample the texture color
    FragColor = texColor;
    // FragColor = vec4(TexCoord, 0.0, 1.0);

}
