// 2.4 Tableaux sérialisées

#include "precomputed.h"
#include "vbo04.h"

#define BUFFER_OFFSET(i) ((char*)NULL + (i))

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

static GLuint BufferName;

static const GLsizei VertexCount = 6; 

enum EVertexBufferTag
{
    INDEX_OBJECT = 0,
    POSITION_OBJECT = 1,
    COLOR_OBJECT = 2
};

void CTest04::Begin()
{
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glShadeModel(GL_SMOOTH);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45.0f, 640.0f / 480.0f, 0.1f, 100.0f);

	glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0f, 0.0f,-4.0f);

    glGenBuffers(1, &BufferName);
}

void CTest04::End()
{
    glDeleteBuffers(1, &BufferName);
}

void CTest04::Render()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName);
    glBufferData(GL_ARRAY_BUFFER, ColorSize + PositionSize, 0, GL_STREAM_DRAW);

    glBufferSubData(GL_ARRAY_BUFFER, 0, ColorSize, ColorData);
    glBufferSubData(GL_ARRAY_BUFFER, ColorSize, PositionSize, PositionData);

    glColorPointer(3, GL_UNSIGNED_BYTE, 0, 0);
    glVertexPointer(2, GL_FLOAT, 0, BUFFER_OFFSET(ColorSize));

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

    glDrawArrays(GL_TRIANGLES, 0, VertexCount);

    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
