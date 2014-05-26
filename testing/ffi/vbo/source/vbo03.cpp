// 2.3 Tableaux entrelacés

#include "precomputed.h"
#include "vbo03.h"

#define BUFFER_OFFSET(i) ((char*)NULL + (i))

#pragma pack(push, 1)
struct SVertex
{
    GLubyte r;
    GLubyte g;
    GLubyte b;
    GLfloat x;
    GLfloat y;
};
#pragma pack(pop)

static const GLsizei VertexCount = 6; 

static const GLsizeiptr VertexOffset = 3;
static const GLsizeiptr ColorOffset = 0;
static const GLsizeiptr VertexSize = VertexCount * sizeof(SVertex);
static const SVertex VertexData[] =
{
	255,   0,   0,-1.0f,-1.0f, 
	255, 255,   0, 1.0f,-1.0f, 
	  0, 255,   0, 1.0f, 1.0f, 
	  0, 255,   0, 1.0f, 1.0f, 
	  0,   0, 255,-1.0f, 1.0f, 
	255,   0,   0,-1.0f,-1.0f, 
};

static GLuint BufferName;

void CTest03::Begin()
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

void CTest03::End()
{
    glDeleteBuffers(1, &BufferName);
}

void CTest03::Render()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glBindBuffer(GL_ARRAY_BUFFER, BufferName);
    glBufferData(GL_ARRAY_BUFFER, VertexSize, VertexData, GL_STREAM_DRAW);

    glColorPointer(3, GL_UNSIGNED_BYTE, sizeof(SVertex), BUFFER_OFFSET(ColorOffset));
    glVertexPointer(2, GL_FLOAT, sizeof(SVertex), BUFFER_OFFSET(VertexOffset));

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);

    glDrawArrays(GL_TRIANGLES, 0, VertexCount);

    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
}
