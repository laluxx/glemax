// NOTE LCD 
// #version 330 core
// in vec2 TexCoord;
// in vec4 ourColor;  // Receive color from vertex shader
// out vec4 FragColor;
// uniform sampler2D ourTexture;

// void main() {
//     vec3 textColor = texture(ourTexture, TexCoord).rgb;  // Get the RGB subpixel values
//     FragColor = vec4(textColor * ourColor.rgb, ourColor.a);  // Multiply by the vertex color for any additional tinting
// }



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


// NOTE RAINY
// #version 330 core
// in vec2 TexCoord;
// in vec4 ourColor;  // Receive color from vertex shader

// out vec4 FragColor;

// uniform sampler2D ourTexture;
// uniform float time;  // Time variable for animation
// uniform float screenWidth;  // Width of the screen or text area to normalize light position

// void main() {
//     // Uniform wave distortion across the text
//     float waveAmplitude = 0.006;  // Smaller amplitude for a subtler effect
//     float waveFrequency = 60.0;  // Reduced frequency for a smoother wave
//     float wave = waveAmplitude * sin(TexCoord.y * waveFrequency + time);

//     // Uniform moving light effect across the text
//     float lightPosition = (sin(time * 0.5) + 1.0) * 0.5 * screenWidth;  // Normalize light position across the screen width
//     float lightStrength = exp(-pow(TexCoord.x * screenWidth - lightPosition, 2.0) * 0.002);  // Adjust falloff for wider spread

//     // Sample the texture with wave distortion
//     vec4 sampled = vec4(1.0, 1.0, 1.0, texture(ourTexture, TexCoord + vec2(wave, 0.0)).r);

//     // Combine texture alpha with light strength for a dramatic backlight effect
//     float alpha = sampled.a * (0.5 + 0.5 * lightStrength);  // Blend original alpha with light effect

//     // Set the output color using the original color, apply a smooth fade with distance from light
//     FragColor = vec4(ourColor.rgb * lightStrength, alpha);
// }

