#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
layout (location = 2) in vec2 aTexCoord; // UV coordinates for texture and used for wave phase variation

uniform mat4 projectionMatrix;
uniform float time;

out vec3 ourColor;
out vec2 TexCoord;

void main()
{
    vec4 projectedPos = projectionMatrix * vec4(aPos, 1.0);

    // Enhanced wave effect for a cloth-like motion
    float phaseShift = aTexCoord.x * 10.0; // Reduce for more gradual changes
    float waveAmplitudeY = 0.006; // Y amplitude for vertical movement
    float waveAmplitudeX = 0; // X amplitude for horizontal bending
    float waveFrequency = 0.06; // Frequency for tighter waves

    // Create waves with different phases for x and y movements
    float waveY = waveAmplitudeY * sin((aPos.x * waveFrequency) + phaseShift + (time * 19.0));
    float waveX = waveAmplitudeX * sin((aPos.x * waveFrequency) + phaseShift + (time * 28.0) + .1415 / 2.0); // 90 degrees phase offset

    // Apply the wave transformation to both x and y positions for bending effect
    vec3 animatedPos = projectedPos.xyz;
    animatedPos.y += waveY;
    animatedPos.x += waveX; // Adds horizontal movement simulating bending

    // Output the modified position and original attributes
    gl_Position = vec4(animatedPos, projectedPos.w);
    ourColor = aColor;
    TexCoord = aTexCoord;
}


// #version 330 core
// layout (location = 0) in vec3 aPos;
// layout (location = 1) in vec3 aColor;
// layout (location = 2) in vec2 aTexCoord; // UV coordinates for texture and used for wave phase variation

// uniform mat4 projectionMatrix;
// uniform float time;

// out vec3 ourColor;
// out vec2 TexCoord;

// void main()
// {
//     vec4 projectedPos = projectionMatrix * vec4(aPos, 1.0);

//     // Enhanced wave effect using a combination of position and texture coordinates
//     float phaseShift = aTexCoord.x * 20.0; // Increasing the influence of texture coordinate on phase
//     float waveAmplitude = 0.003; // Larger amplitude for more noticeable wave
//     float waveFrequency = 0.06; // More frequency for tighter waves

//     // Using both x position and texture coordinate x to create a lateral wave effect
//     float wave = waveAmplitude * sin((aPos.x * waveFrequency) + phaseShift + (time * 10)); // Speed up the wave a bit

//     // Apply the wave transformation to the y-position
//     vec3 animatedPos = projectedPos.xyz;
//     animatedPos.y += wave;

//     // Output the modified position and original attributes
//     gl_Position = vec4(animatedPos, projectedPos.w);
//     ourColor = aColor;
//     TexCoord = aTexCoord;
// }



// #version 330 core
// layout (location = 0) in vec3 aPos;
// layout (location = 1) in vec3 aColor;
// layout (location = 2) in vec2 aTexCoord; // UV coordinates

// uniform mat4 projectionMatrix;
// uniform float time;

// out vec3 ourColor;
// out vec2 TexCoord;

// void main()
// {
//     vec4 projectedPos = projectionMatrix * vec4(aPos, 1.0);

//     vec3 animatedPos = projectedPos.xyz;
//     animatedPos.y += sin(time + aPos.x * 1.0) * 0.014;

//     gl_Position = vec4(animatedPos, projectedPos.w);
    
//     ourColor = aColor;
//     TexCoord = aTexCoord;
// }
