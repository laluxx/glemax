#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor; // Change to vec4 to include alpha
layout (location = 2) in vec2 aTexCoord;

uniform mat4 projectionMatrix;

out vec4 ourColor; // Change to vec4
out vec2 TexCoord;

void main()
{
    gl_Position = projectionMatrix * vec4(aPos, 1.0);
    ourColor = aColor; // Pass the color with alpha
    TexCoord = aTexCoord;
}


/* #version 330 core */
/* layout (location = 0) in vec3 aPos; */
/* layout (location = 1) in vec3 aColor; */
/* layout (location = 2) in vec2 aTexCoord; // UV coordinates */

/* uniform mat4 projectionMatrix; */

/* out vec3 ourColor; */
/* out vec2 TexCoord; // Pass UV coordinates to the fragment shader */

/* void main() */
/* { */
/*     gl_Position = projectionMatrix * vec4(aPos, 1.0); */
/*     ourColor = aColor; */
/*     TexCoord = aTexCoord; // Pass UV coordinates */
/* } */
