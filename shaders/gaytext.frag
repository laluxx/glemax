#version 330 core

// Input from vertex shader
in vec2 TexCoord;
in vec4 ourColor;

// Output color
out vec4 FragColor;

// Uniforms
uniform sampler2D ourTexture;
uniform float time;

// HSL to RGB conversion function
vec3 hsl2rgb(vec3 c) {    vec3 rgb = clamp(abs(mod(c.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return c.z / c.y + (rgb - 0.5) * (1.0 - abs(2.0 * c.z - 1.0));
}

void main() {
    // Get texture alpha from red channel (for text)
    float textAlpha = texture(ourTexture, TexCoord).r;
    
    // Skip processing if pixel is fully transparent
    if (textAlpha < 0.01) {
        discard;
    }
    
    // Normalized coordinates
    vec2 fragCoord = gl_FragCoord.xy / vec2(800.0, 600.0);
    
    // Create rainbow effect with some cool features
    float rainbowSpeed = 0.15;
    float rainbowScale = 2.5;
    float xOffset = sin(time * 0.7) * 0.2;
    float yOffset = cos(time * 0.5) * 0.1;
    
    // Create wavy distortion for more dynamic effect
    float wave = sin(fragCoord.y * 20.0 + time * 2.0) * 0.05;
    vec2 distortedCoord = vec2(fragCoord.x + wave, fragCoord.y);
    
    // Generate rainbow color with improved saturation and lightness
    vec3 rainbowColor = hsl2rgb(vec3(
                                     mod(time * rainbowSpeed + distortedCoord.x * rainbowScale + distortedCoord.y + xOffset + yOffset, 1.0),
                                     0.7,  // Higher saturation
                                     0.6   // Adjusted lightness for better visibility
                                     ));
    
    // Apply pulsing effect
    float pulse = 0.05 * sin(time * 3.0) + 0.95;
    rainbowColor *= pulse;
    
    // Apply glow effect based on text alpha
    float glow = pow(textAlpha, 1.5);
    rainbowColor = mix(rainbowColor * 0.6, rainbowColor, glow);
    
    // Final color combines rainbow effect with text alpha
    FragColor = vec4(rainbowColor, textAlpha);
}
