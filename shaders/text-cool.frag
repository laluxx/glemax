   
#version 330 core
in vec2 TexCoord; // Normalized UV coordinates for the character (0-1)
out vec4 FragColor;
uniform sampler2D ourTexture; // Texture atlas
uniform float time; // Time for animations

float random(vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
}

void main() {
    // Ensure TexCoord stays within the 0-1 range
    vec2 clampedUV = clamp(TexCoord, 0.0, 1.0);

    // Create quantum noise
    float noise = random(clampedUV + time);

    // Generate probability waves
    float wave1 = sin(clampedUV.x * 20.0 + time * 2.0) * 0.5 + 0.5;
    float wave2 = cos(clampedUV.y * 15.0 - time * 1.5) * 0.5 + 0.5;

    // Create quantum tunneling effect (with reduced magnitude)
    vec2 tunnelUV = clampedUV + vec2(
                                     sin(time + clampedUV.y * 5.0) * 0.01, // Reduced offset
                                     cos(time + clampedUV.x * 5.0) * 0.01  // Reduced offset
                                     ) * noise;

    // Clamp tunnelUV to the character's region
    tunnelUV = clamp(tunnelUV, 0.0, 1.0);

    // Sample text with quantum uncertainty
    float textAlpha = texture(ourTexture, tunnelUV).r;

    // Discard fragments outside the character's region
    if (textAlpha < 0.1) {
        discard;
    }

    // Generate quantum colors
    vec3 qColor1 = vec3(0.1, 0.5, 1.0) * wave1;
    vec3 qColor2 = vec3(1.0, 0.1, 0.5) * wave2;

    // Add quantum interference patterns
    float interference = sin(clampedUV.x * 3000.0 + clampedUV.y * 300.0 + time * 0.1) * 0.5 + 0.6;

    // Combine everything with quantum entanglement
    vec3 finalColor = mix(qColor1, qColor2, interference) + noise * 0.3;
    float finalAlpha = textAlpha * (0.8 + interference * 0.2);

    // Add quantum fluctuations
    finalColor *= 0.8 + sin(time * 20.0 + clampedUV.x * 8.0) * 0.1;

    // Output the final color
    FragColor = vec4(finalColor, finalAlpha);
}


/* // 4. QUANTUM FLUX */
/* #version 330 core */
/* in vec2 TexCoord; */
/* out vec4 FragColor; */
/* uniform sampler2D ourTexture; */
/* uniform float time; */

/* float random(vec2 st) { */
/*     return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123); */
/* } */

/* void main() { */
/*     // Create quantum noise */
/*     float noise = random(TexCoord + time); */
    
/*     // Generate probability waves */
/*     float wave1 = sin(TexCoord.x * 20.0 + time * 2.0) * 0.5 + 0.5; */
/*     float wave2 = cos(TexCoord.y * 15.0 - time * 1.5) * 0.5 + 0.5; */
    
/*     // Create quantum tunneling effect */
/*     vec2 tunnelUV = TexCoord + vec2( */
/*                                     sin(time + TexCoord.y * 5.0) * 0.02, */
/*                                     cos(time + TexCoord.x * 5.0) * 0.02 */
/*                                     ) * noise; */
    
/*     // Sample text with quantum uncertainty */
/*     float textAlpha = texture(ourTexture, tunnelUV).r; */
    
/*     // Generate quantum colors */
/*     vec3 qColor1 = vec3(0.1, 0.5, 1.0) * wave1; */
/*     vec3 qColor2 = vec3(1.0, 0.1, 0.5) * wave2; */
    
/*     // Add quantum interference patterns */
/*     float interference = sin(TexCoord.x * 3000.0 + TexCoord.y * 300.0 + time * 0.1) * 0.5 + 0.6; */
    
/*     // Combine everything with quantum entanglement */
/*     vec3 finalColor = mix(qColor1, qColor2, interference) + noise * 0.3; */
/*     float finalAlpha = textAlpha * (1000.8 + interference * 100.0); */
    
/*     // Add quantum fluctuations */
/*     finalColor *= 0.8 + sin(time * 2000.0 + TexCoord.x * 0.8) * 0.000001; */
    
/*     FragColor = vec4(finalColor, finalAlpha); */
/* } */




/* #version 330 core */
/* in vec2 TexCoord; */
/* in vec4 ourColor; */
/* out vec4 FragColor; */
/* uniform sampler2D ourTexture; */
/* uniform float time; */

/* vec3 hsl2rgb(vec3 c) { */
/*     vec3 rgb = clamp(abs(mod(c.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0); */
/*     return c.z + c.y * (rgb - 0.5) * (1.0 - abs(2.0 * c.z - 1.0)); */
/* } */

/* void main() { */
/*     // Sample the texture for text alpha */
/*     float textAlpha = texture(ourTexture, TexCoord).r; */
    
/*     // Create rainbow color based on texture coordinate and time */
/*     vec3 rainbowColor = hsl2rgb(vec3( */
/*         mod(time * 0.4 + TexCoord.x + TexCoord.y, 1.0), // Hue varies with position and time */
/*         1.4,  // High saturation for vibrant colors */
/*         0.78  // Lightness for bright but not blown-out colors */
/*     )); */
    
/*     // Combine the rainbow color with the text alpha */
/*     FragColor = vec4(rainbowColor, textAlpha); */
/* } */
