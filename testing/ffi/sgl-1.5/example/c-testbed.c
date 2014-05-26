#include <stdio.h>
#include <GL/gl.h>
#include <malloc.h>

int main(int argc, char** argv)
{
	int no = 1;
	printf ("testing %d\n", no);

	GLuint bufids[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids);
	glBindBuffer(GL_ARRAY_BUFFER, bufids[0]);
	GLuint bufids2[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids2);
	glBindBuffer(GL_ARRAY_BUFFER, bufids2[0]);
	GLuint bufids3[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids3);
	glBindBuffer(GL_ARRAY_BUFFER, bufids3[0]);
	GLuint bufids4[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids4);
	glBindBuffer(GL_ARRAY_BUFFER, bufids4[0]);
	GLuint bufids5[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids5);
	glBindBuffer(GL_ARRAY_BUFFER, bufids5[0]);	
	GLuint bufids6[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids6);
	glBindBuffer(GL_ARRAY_BUFFER, bufids6[0]);
	GLuint bufids7[1];// = (int*)malloc(no*sizeof(int));
	glGenBuffers(no, bufids7);
	glBindBuffer(GL_ARRAY_BUFFER, bufids7[0]);

	printf ("buf cont = %u, %u, %u, %u, %u, %u, %u, %u  \n", bufids[0], bufids2[0], bufids3[0], bufids4[0], bufids5[0], bufids6[0], bufids7[0]);




       	//GLuint bufidsN[] = {100, 200, 300};// = (int*)malloc(no*sizeof(int));
	no = 6;
       	GLuint bufidsN[6]; //= (GLuint*)malloc(no*sizeof(GLuint));
	glGenBuffers(no, bufidsN);
	printf ("gen->buf cont = %u, %u, %u\n", bufidsN[0], bufidsN[1], bufidsN[2]);
	printf ("gen->buf cont = %u, %u, %u\n", bufidsN[3], bufidsN[4], bufidsN[5]);
	
	return 0;
}
