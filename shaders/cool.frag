// NOTE Grayscale
#version 330 core
in vec2 TexCoord;
in vec4 ourColor;  // Receive color from vertex shader

out vec4 FragColor;

uniform sampler2D ourTexture;

void main() {
    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(ourTexture, TexCoord).r);
    FragColor = ourColor * sampled;  // Apply vertex color to texture
}




// #version 330 core

// out vec4 FragColor;

// uniform float time;

// vec3 hsl2rgb(vec3 c) {
//     vec3 rgb = clamp(abs(mod(c.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
//     return c.z + c.y * (rgb - 0.5) * (1.0 - abs(2.0 * c.z - 1.0));
// }

// void main() {
//     vec2 fragCoord = gl_FragCoord.xy / vec2(800.0, 600.0); // Example resolution, adjust as needed

//     // Create the rainbow color based on time and fragment position
//     vec3 rainbowColor = hsl2rgb(vec3(mod(time * 0.2 + fragCoord.x + fragCoord.y, 1.0), 0.5, 0.5));

//     // Set the final fragment color
//     FragColor = vec4(rainbowColor, 1.0);
// }
