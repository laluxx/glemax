#version 330 core

out vec4 FragColor;

in vec2 TexCoord;
in vec3 ourColor;

void main()
{
    vec2 normalizedPos = TexCoord - vec2(0.5, 0.5);

    // Calculate the distance from the center of the circle
    float dist = length(normalizedPos / vec2(0.5, 0.5));

    // Adjust edge width based on screen resolution (optional)
    float edgeWidth = 0.005 * fwidth(dist); // Use fwidth for adaptive edge width

    // Apply an exponential function for smoother anti-aliasing effect
    float alpha = exp(-pow(dist / (1.0 - edgeWidth), 160.0)); // TODO unhardcode 100, it should be width*height/2 of the quad

    // Gamma correction (sRGB to linear space)
    vec3 linearColor = pow(ourColor, vec3(2.2));

    FragColor = vec4(linearColor, alpha);

    // Gamma correction back to sRGB
    FragColor.rgb = pow(FragColor.rgb, vec3(1.0 / 2.2));
}

// #version 330 core

// out vec4 FragColor;

// in vec2 TexCoord;
// in vec3 ourColor;

// void main()
// {
//     vec2 normalizedPos = TexCoord - vec2(0.5, 0.5);

//     float dist = length(normalizedPos / vec2(0.5, 0.5));

//     if (dist < 1.0)
//         FragColor = vec4(ourColor, 1.0);
//     else
//         discard;
// }


