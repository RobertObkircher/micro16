#define M_PI 3.1415926535897932384626433832795

#ifdef VertexShader

layout (location = 0) in vec3 vPos;
layout (location = 1) in vec3 vColor;

out vec3 fColor;

void main()
{
  fColor = vColor;
  gl_Position = vec4(vPos, 1.0);
}

#endif

#ifdef FragmentShader

in vec3 fColor;
out vec4 FragColor;

void main()
{
    FragColor = vec4(fColor, 1.0);
}

#endif
