//2.2 Utilisation avec index buffer

#include "precomputed.h"
#include "vbo02.h"

static const GLsizeiptr PositionSize = 4 * 2 * sizeof(GLfloat);
static const GLfloat PositionData[] =
{
	-1.0f,-1.0f,
	 1.0f,-1.0f,
	 1.0f, 1.0f,
	-1.0f, 1.0f
};

static const GLsizeiptr ColorSize = 4 * 3 * sizeof(GLubyte);
static const GLubyte ColorData[] =
{
	255,   0,   0,
	255, 255,   0,
	  0, 255,   0,
	  0,   0, 255
};

static const GLsizeiptr IndexSize = 6 * sizeof(GLuint);
static const GLuint IndexData[] =
{
	0, 1, 2, 3, 0, 2
};

static const int BufferSize = 3;
static GLuint BufferName[BufferSize];

static const GLsizei VertexCount = 6; 

enum EVertexBufferTag
{
    INDEX_OBJECT = 0,
    POSITION_OBJECT = 1,
    COLOR_OBJECT = 2
};

void CTest02::Begin()
{
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glShadeModel(GL_SMOOTH);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45.0f, 640.0f / 480.0f, 0.1f, 100.0f);

	glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0f, 0.0f,-4.0f);

    glGenBuffers(BufferSize, BufferName);
}

void CTest02::End()
{
    glDeleteBuffers(BufferSize, BufferName);
}

void CTest02::Render()
{
    glClear(GL_COLOR_BUFFER_BIT);

	glBindBuffer(GL_ARRAY_BUFFER, BufferName[COLOR_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, ColorSize, ColorData, GL_STREAM_DRAW);
	glColorPointer(3, GL_UNSIGNED_BYTE, 0, 0);

	glBindBuffer(GL_ARRAY_BUFFER, BufferName[POSITION_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, PositionSize, PositionData, GL_STREAM_DRAW);
	glVertexPointer(2, GL_FLOAT, 0, 0);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, BufferName[INDEX_OBJECT]);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, IndexSize, IndexData, GL_STREAM_DRAW);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

	glDrawElements(GL_TRIANGLES, VertexCount, GL_UNSIGNED_INT, 0);

    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}

