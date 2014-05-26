attribute vec3 Color;
attribute vec2 Position;

void main()
{
	gl_Position = gl_ModelViewProjectionMatrix * vec4(Position, 0.0, 1.0);
	gl_FrontColor = vec4(Color, 1.0);
}
