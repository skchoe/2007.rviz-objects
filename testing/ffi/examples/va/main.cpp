/**********************************************************************

  VertexArray Tutorial

  June, 10th, 2000

  This tutorial was written by Philipp Crocoll
  Contact: 
	philipp.crocoll@web.de
	www.codecolony.de

  Every comment would be appreciated.

  If you want to use parts of any code of mine:
	let me know and
	use it!
***********************************************************************

  p : switches between line and point mode

***********************************************************************/

#include <GL/glut.h>
#include <math.h>
#include <vector>
using namespace std;
#define PI 3.1415265359
struct SVertex
{
	GLfloat x,y,z;
	GLfloat r,g,b;
};

//Data for the sphere vertices:
SVertex * Vertices;
int NumVertices;  //size of the vertex array
vector <GLuint> IndexVect;  //we first put the indices into this vector, then copy them to the array below
GLuint * Indices;
int NumIndices;   //size of the index array

GLfloat ChangeY = 0.025;//Indicates how much the y values of the highest and deepest vertex
						//in the sphere are changed each time it is rendered
float yRotated = 0.0;
bool PointsOnly = false;//Indicated, if the triangles or only points are drawn

void CreateSphere(int PointRows, int PointsPerRow)
{
	NumVertices = (PointRows-2)*PointsPerRow + 2;
	Vertices = new SVertex[NumVertices];
	IndexVect.clear();  //to be sure it is empty
	float x,y,z;
	int i,j;
	double r;
	for (i = 1; i < (PointRows-1); i++)
	{
		for (j = 0; j < PointsPerRow; j++)
		{
			y = 1.0 - float(i) / float(PointRows-1)*2.0;
			r = sin (acos(y));  //radius of the row
			x = r * sin(float(j) / float(PointsPerRow)*PI*2.0);
			
			z = r * cos(float(j) / float(PointsPerRow)*PI*2.0);
			Vertices[(i-1)*PointsPerRow+j].x = x;
			Vertices[(i-1)*PointsPerRow+j].y = y;
			Vertices[(i-1)*PointsPerRow+j].z = z;
			Vertices[(i-1)*PointsPerRow+j].r = (float)(i) / float(PointRows);
			Vertices[(i-1)*PointsPerRow+j].g = 0.7;
			Vertices[(i-1)*PointsPerRow+j].b = (float)(j) / float(PointsPerRow);
			int ss = (i-1)*PointsPerRow+j;
			printf("%d th coord = %f %f %f\n", ss, x, y, z);
			//cout << ss << "th coord = " << x <<", "<<y<<", "<<endl;
		}

	}
	//The highest and deepest vertices:
	Vertices[(PointRows-2)*PointsPerRow].x = 0.0;
	Vertices[(PointRows-2)*PointsPerRow].y = 1.0;
	Vertices[(PointRows-2)*PointsPerRow].z = 0.0;
	Vertices[(PointRows-2)*PointsPerRow].r = 1.0;
	Vertices[(PointRows-2)*PointsPerRow].g = 0.7;
	Vertices[(PointRows-2)*PointsPerRow].b = 1.0;
	Vertices[(PointRows-2)*PointsPerRow+1].x = 0.0;
	Vertices[(PointRows-2)*PointsPerRow+1].y = -1.0;
	Vertices[(PointRows-2)*PointsPerRow+1].z = 0.0;
	Vertices[(PointRows-2)*PointsPerRow+1].r = 1.0;
	Vertices[(PointRows-2)*PointsPerRow+1].g = 0.7;
	Vertices[(PointRows-2)*PointsPerRow+1].b = 1.0;
	int tt0 = (PointRows-2)*PointsPerRow;
	int tt1 = tt0+1;
	printf ("%d th coord = %f %f %f\n", tt0, 0, 1, 0);
	printf ("%d th coord = %f %f %f\n", tt1, 0, -1, 0);
	//cout << ss << "th coord = " << x <<", "<<y<<", "<<endl;
	
	for (i = 1; i < (PointRows-2); i++)
	{
		for (j = 0; j < (PointsPerRow-1); j++)
		{
			IndexVect.push_back((i-1)*PointsPerRow+j);
			IndexVect.push_back((i-1)*PointsPerRow+j+1);
			IndexVect.push_back((i)*PointsPerRow+j);

			IndexVect.push_back((i-1)*PointsPerRow+j+1);
			IndexVect.push_back((i)*PointsPerRow+j+1);
			IndexVect.push_back((i)*PointsPerRow+j);
		}

		IndexVect.push_back((i-1)*PointsPerRow+PointsPerRow-1);
		IndexVect.push_back((i-1)*PointsPerRow);
		IndexVect.push_back((i)*PointsPerRow+j);

		IndexVect.push_back((i)*PointsPerRow);
		IndexVect.push_back((i-1)*PointsPerRow);
		IndexVect.push_back((i)*PointsPerRow+j);
	}		

	//The triangles to the highest and deepest vertices:
	for (j = 0; j< (PointsPerRow-1); j++)
	{
		IndexVect.push_back(j);
		IndexVect.push_back(j+1);
		IndexVect.push_back((PointRows-2)*PointsPerRow);
	}
	IndexVect.push_back(j);
	IndexVect.push_back(0);
	IndexVect.push_back((PointRows-2)*PointsPerRow);

	for (j = 0; j< (PointsPerRow-1); j++)
	{
		IndexVect.push_back((PointRows-3)*PointsPerRow+j);
		IndexVect.push_back((PointRows-3)*PointsPerRow+j+1);
		IndexVect.push_back((PointRows-2)*PointsPerRow+1);
	}
	IndexVect.push_back((PointRows-3)*PointsPerRow+j);
	IndexVect.push_back((PointRows-3)*PointsPerRow);
	IndexVect.push_back((PointRows-2)*PointsPerRow+1);
	Indices = new GLuint[IndexVect.size()];  //allocate the required memory
	for (i = 0; i < IndexVect.size(); i++)
	{
		Indices[i] = IndexVect[i];
	}
	NumIndices = IndexVect.size();
	IndexVect.clear();  //no longer needed, takes only memory
}

void DrawSphere(void)
{
	if (!PointsOnly)
		glDrawElements(	GL_TRIANGLES, //mode
						NumIndices,  //count, ie. how many indices
						GL_UNSIGNED_INT, //type of the index array
						Indices);
	else 
		glDrawArrays(GL_POINTS,0,NumVertices);

}

void Display(void)
{
	glClear(GL_COLOR_BUFFER_BIT);
	glLoadIdentity();
	glTranslatef(0.0,0.0,-4.0);
	glRotatef(yRotated, 0.0, 1.0, 0.0);
	DrawSphere();
	glFlush();			//Finish rendering
	glutSwapBuffers();
}

void Reshape(int x, int y)
{
	if (y == 0 || x == 0) return;  //Nothing is visible then, so return
	//Set a new projection matrix
	glMatrixMode(GL_PROJECTION);  
	glLoadIdentity();
	//Angle of view:40 degrees
	//Near clipping plane distance: 0.5
	//Far clipping plane distance: 20.0
	gluPerspective(40.0,(GLdouble)x/(GLdouble)y,0.5,20.0);
	glViewport(0,0,x,y);  //Use the whole window for rendering
	glMatrixMode(GL_MODELVIEW);
}

void Idle(void)
{
	Vertices[NumVertices-2].y += ChangeY;
	Vertices[NumVertices-1].y -= ChangeY;

	if (Vertices[NumVertices-2].y> 1.5 || Vertices[NumVertices-2].y< 0.5)
		 ChangeY *= -1;
	yRotated += 0.3;
	Display();
}

void Keyboard(unsigned char key, int x, int y)
{
	switch(key)
	{
	case 'p': 
		PointsOnly = !PointsOnly;
		break;
	}
}

int main (int argc, char **argv)
{
	//Initialize GLUT
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(300,300);
	//Create a window with rendering context and everything else we need
	glutCreateWindow("Vertex Arrays");
	//compute the vertices and indices
	CreateSphere(16,16);
	//as we do not yet use a depth buffer, we cannot fill our sphere
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	//Enable the vertex array functionality:
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glVertexPointer(	3,   //3 components per vertex (x,y,z)
						GL_FLOAT,
						sizeof(SVertex),
						Vertices);
	glColorPointer(		3,   //3 components per vertex (r,g,b)
						GL_FLOAT,
						sizeof(SVertex),
						&Vertices[0].r);  //Pointer to the first color
	glPointSize(2.0);
	glClearColor(0.0,0.0,0.0,0.0);
	//Assign the two used Msg-routines
	glutDisplayFunc(Display);
	glutReshapeFunc(Reshape);
	glutKeyboardFunc(Keyboard);
	glutIdleFunc(Idle);
	//Let GLUT get the msgs
	glutMainLoop();
	return 0;
}
