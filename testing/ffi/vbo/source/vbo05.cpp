// 2.5 Vertex mapping

#include "precomputed.h"
#include "vbo05.h"

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

enum
{
    POSITION_OBJECT = 0,
    COLOR_OBJECT = 1
};

void CTest05::Begin()
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

void CTest05::End()
{
    glBindBuffer(GL_ARRAY_BUFFER, BufferName[POSITION_OBJECT]);
    GLfloat* PositionBuffer = static_cast<GLfloat*>(glMapBuffer(GL_ARRAY_BUFFER, GL_READ_ONLY));
    printf("Position: vec2(%f, %f)\n", PositionBuffer[0], PositionBuffer[1]);
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glDeleteBuffers(BufferSize, BufferName);
}

// Transfère parallélisé
void CTest05::Render()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[COLOR_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, ColorSize, NULL, GL_STREAM_DRAW);
    GLvoid* ColorBuffer = glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[POSITION_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, PositionSize, NULL, GL_STREAM_DRAW);
    GLvoid* PositionBuffer = glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);

    memcpy(ColorBuffer, ColorData, ColorSize);
    memcpy(PositionBuffer, PositionData, PositionSize);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[COLOR_OBJECT]);
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glColorPointer(3, GL_UNSIGNED_BYTE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[POSITION_OBJECT]);
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glVertexPointer(2, GL_FLOAT, 0, 0);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

    glDrawArrays(GL_TRIANGLES, 0, VertexCount);

    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
/* Transfère sérialisé
void CTest05::Render()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[COLOR_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, ColorSize, NULL, GL_STREAM_DRAW);
    GLvoid* ColorBuffer = glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    memcpy(ColorBuffer, ColorData, ColorSize);
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glColorPointer(3, GL_UNSIGNED_BYTE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName[POSITION_OBJECT]);
    glBufferData(GL_ARRAY_BUFFER, PositionSize, NULL, GL_STREAM_DRAW);
    GLvoid* PositionBuffer = glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    memcpy(PositionBuffer, PositionData, PositionSize);
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glVertexPointer(2, GL_FLOAT, 0, 0);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

    glDrawArrays(GL_TRIANGLES, 0, VertexCount);

    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
*/
