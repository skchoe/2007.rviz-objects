// Utilisation avec GLSL

#include "precomputed.h"
#include "vbo06.h"

static const GLsizeiptr PositionSize = 6 * 2 * sizeof(GLfloat);
static const GLfloat PositionData[] =
{
	-1.0f,-1.0f,
	 1.0f,-1.0f,
	 1.0f, 1.0f,
	 1.0f, 1.0f,
	-1.0f, 1.0f,
	-1.0f,-1.0f,
};

static const GLsizeiptr ColorSize = 6 * 3 * sizeof(GLubyte);
static const GLubyte ColorData[] =
{
	255,   0,   0,
	255, 255,   0,
	  0, 255,   0,
	  0, 255,   0,
	  0,   0, 255,
	255,   0,   0
};

static const int BufferSize = 2;
static GLuint BufferName[BufferSize];

static const GLsizei VertexCount = 6; 

static GLuint AttributIndex[BufferSize];
static GLuint Program;

enum
{
    POSITION_OBJECT = 0,
    COLOR_OBJECT = 1
};

static std::string LoadShader(const char* filename)
{
	std::ifstream stream(filename, std::ios::in);

    if(!stream.is_open())
        return "";

    std::string Line = "";
	std::string Text = "";

	while(std::getline(stream, Line))
    	Text += "\n" + Line;

	stream.close();

	return Text;
}

static bool CheckShader(int Object, int Type, const char* Message)
{
    int Success;
    glGetShaderiv(Object, Type, &Success);
	if(Success == GL_FALSE)
	{
        fprintf(stderr, Message);
        int InfoLogSize;
        glGetShaderiv(Object, GL_INFO_LOG_LENGTH, &InfoLogSize);
        char* Buffer = new char[InfoLogSize];
		glGetShaderInfoLog(Object, InfoLogSize, NULL, Buffer);
		fprintf(stderr, "%s\n", Buffer);
        delete[] Buffer;
		return false;
	}
    return true;
}

static bool CheckProgram(int Object, int Type, const char* Message)
{
    int Success;
    glGetProgramiv(Object, Type, &Success);
	if(Success == GL_FALSE)
	{
        fprintf(stderr, Message);
        int InfoLogSize;
        glGetProgramiv(Object, GL_INFO_LOG_LENGTH, &InfoLogSize);
        char* Buffer = new char[InfoLogSize];
		glGetProgramInfoLog(Object, InfoLogSize, NULL, Buffer);
		fprintf(stderr, "%s\n", Buffer);
        delete[] Buffer;
		return false;
	}
    return true;
}

void CTest06::Begin()
{
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glShadeModel(GL_SMOOTH);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45.0f, 640.0f / 480.0f, 0.1f, 100.0f);

	glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0f, 0.0f,-4.0f);

    // Shader loading.
    std::string VertexShader = LoadShader("./data/test5.vert");
    const char* ShaderBuffer = VertexShader.c_str();
	Program = glCreateProgram();
	GLuint Shader = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(Shader, 1, &ShaderBuffer, NULL);
	glCompileShader(Shader);
    if(!CheckShader(Shader, GL_COMPILE_STATUS, "Compile\n"))
        return;
    glAttachShader(Program, Shader);
    glLinkProgram(Program);
    glDeleteShader(Shader);
    if(!CheckProgram(Program, GL_LINK_STATUS, "Link\n"))
        return;
    AttributIndex[COLOR_OBJECT] = glGetAttribLocation(Program, "Color");
    AttributIndex[POSITION_OBJECT] = glGetAttribLocation(Program, "Position");

    glGenBuffers(BufferSize, BufferName);
}

void CTest06::End()
{
    glDeleteProgram(Program);
    glDeleteBuffers(BufferSize, BufferName);
}

void CTest06::Render()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glUseProgram(Program);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[COLOR_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, ColorSize, ColorData, GL_STREAM_DRAW);
    glVertexAttribPointer(AttributIndex[COLOR_OBJECT], 3, GL_UNSIGNED_BYTE, GL_TRUE, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, BufferName[POSITION_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, PositionSize, PositionData, GL_STREAM_DRAW);
    glVertexAttribPointer(AttributIndex[POSITION_OBJECT], 2, GL_FLOAT, GL_FALSE, 0, 0);

    glEnableVertexAttribArray(AttributIndex[COLOR_OBJECT]);
    glEnableVertexAttribArray(AttributIndex[POSITION_OBJECT]);

    glDrawArrays(GL_TRIANGLES, 0, VertexCount);

    glDisableVertexAttribArray(AttributIndex[POSITION_OBJECT]);
    glDisableVertexAttribArray(AttributIndex[COLOR_OBJECT]);

    glUseProgram(0);
}
