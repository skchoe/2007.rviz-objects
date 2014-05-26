
/* Copyright (c) Mark J. Kilgard, 1997. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

/* This program was requested by Patrick Earl; hopefully someone else
   will write the equivalent Direct3D immediate mode program. */

/* Seungkeol Choe modify the code to support vbo */

#define GL_GLEXT_PROTOTYPES

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <GL/gl.h>
#include <GL/glut.h>
#include <GL/glext.h>

// cube ///////////////////////////////////////////////////////////////////////
//    v6----- v5
//   /|      /|
//  v1------v0|
//  | |     | |
//  | |v7---|-|v4
//  |/      |/
//  v2------v3

// vertex coords array
GLfloat vertices[] = {1,1,1,  -1,1,1,  -1,-1,1,  1,-1,1,        // v0-v1-v2-v3
                      1,1,1,  1,-1,1,  1,-1,-1,  1,1,-1,        // v0-v3-v4-v5
                      1,1,1,  1,1,-1,  -1,1,-1,  -1,1,1,        // v0-v5-v6-v1
                      -1,1,1,  -1,1,-1,  -1,-1,-1,  -1,-1,1,    // v1-v6-v7-v2
                      -1,-1,-1,  1,-1,-1,  1,-1,1,  -1,-1,1,    // v7-v4-v3-v2
                      1,-1,-1,  -1,-1,-1,  -1,1,-1,  1,1,-1};   // v4-v7-v6-v5

// normal array
GLfloat normals[] = {0,0,1,  0,0,1,  0,0,1,  0,0,1,             // v0-v1-v2-v3
                     1,0,0,  1,0,0,  1,0,0, 1,0,0,              // v0-v3-v4-v5
                     0,1,0,  0,1,0,  0,1,0, 0,1,0,              // v0-v5-v6-v1
                     -1,0,0,  -1,0,0, -1,0,0,  -1,0,0,          // v1-v6-v7-v2
                     0,-1,0,  0,-1,0,  0,-1,0,  0,-1,0,         // v7-v4-v3-v2
                     0,0,-1,  0,0,-1,  0,0,-1,  0,0,-1};        // v4-v7-v6-v5

// color array
GLfloat colors[] = {1,1,1,  1,1,0,  1,0,0,  1,0,1,              // v0-v1-v2-v3
                    1,1,1,  1,0,1,  0,0,1,  0,1,1,              // v0-v3-v4-v5
                    1,1,1,  0,1,1,  0,1,0,  1,1,0,              // v0-v5-v6-v1
                    1,1,0,  0,1,0,  0,0,0,  1,0,0,              // v1-v6-v7-v2
                    0,0,0,  0,0,1,  1,0,1,  1,0,0,              // v7-v4-v3-v2
                    0,0,1,  0,0,0,  0,1,0,  0,1,1};             // v4-v7-v6-v5

// color array
GLfloat colors_single[] = {1,1,1,  1,1,1,  1,1,1,  1,1,1,       // v0-v1-v2-v3
                    0,1,1,  0,1,1,  0,1,1,  0,1,1,              // v0-v3-v4-v5
                    1,1,1,  0,1,1,  0,1,0,  1,1,0,              // v0-v5-v6-v1
                    1,1,0,  1,1,0,  1,1,0,  1,1,0,              // v1-v6-v7-v2
                    0,0,1,  0,0,1,  0,0,1,  0,0,1,              // v7-v4-v3-v2
                    0,0,1,  0,0,0,  0,1,0,  0,1,1};             // v4-v7-v6-v5

static const int bufSize=6;
static GLuint vboId[6];

// prototypes
void display(void);
void reshape(int w, int h);
void init(void);
void initVBO(void);

void
display(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  glPushMatrix();

  glTranslatef(-.5, -.5, -.5);

  // bind VBOs with IDs and set the buffer offsets of the bound VBOs
  // When buffer object is bound with its ID, all pointers in gl*Pointer()
  // are treated as offset instead of real pointer.

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  
  glBindBuffer(GL_ARRAY_BUFFER, vboId[0]);
  glBufferData(GL_ARRAY_BUFFER, 
	       sizeof(vertices), vertices, GL_STATIC_DRAW);
  glVertexPointer(3, GL_FLOAT, 0, 0);

  glBindBuffer(GL_ARRAY_BUFFER, vboId[1]);
  glBufferData(GL_ARRAY_BUFFER, 
	       sizeof(normals), normals, GL_STATIC_DRAW);
  glNormalPointer(GL_FLOAT, 0, 0);

  glBindBuffer(GL_ARRAY_BUFFER, vboId[2]);
  glBufferData(GL_ARRAY_BUFFER, 
	       sizeof(colors), colors, GL_STATIC_DRAW);
  glColorPointer(3, GL_FLOAT, 0, 0);

  glDrawArrays(GL_QUADS, 0, 24);

  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);

  glPopMatrix();

  glPushMatrix();

  glTranslatef(.5, .5, .5);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);

  glBindBuffer(GL_ARRAY_BUFFER, vboId[3]);
  glBufferData(GL_ARRAY_BUFFER, 
	       sizeof(colors_single), colors_single, GL_STATIC_DRAW);
  glColorPointer(3, GL_FLOAT, 0, 0);

  glDrawArrays(GL_QUADS, 0, 24);

  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);

  glPopMatrix();


  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glutSwapBuffers();
}
void 
reshape(int w, int h)
{
    // set viewport to be the entire window
    glViewport(0, 0, (GLsizei)w, (GLsizei)h);

    // set perspective viewing frustum
    // float aspectRatio = (float)w / h;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    //glFrustum(-aspectRatio, aspectRatio, -1, 1, 1, 100);
    // FOV, AspectRatio, NearClip, FarClip
    gluPerspective(60.0f, (float)(w)/h, 1.0f, 1000.0f); 


    // switch to modelview matrix in order to set scene
    glMatrixMode(GL_MODELVIEW);
}

void
init(void)
{
  glShadeModel(GL_SMOOTH);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);

  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glClearColor(0, 0, 0, 1);

  // set up light colors (ambient, diffuse, specular)
  GLfloat lightKa[] = {.2f, .2f, .2f, 1.0f};  // ambient light
  GLfloat lightKd[] = {.7f, .7f, .7f, 1.0f};  // diffuse light
  GLfloat lightKs[] = {1, 1, 1, 1};           // specular light
  glLightfv(GL_LIGHT0, GL_AMBIENT, lightKa);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, lightKd);
  glLightfv(GL_LIGHT0, GL_SPECULAR, lightKs);

  // position the light
  float lightPos[4] = {0, 0, 20, 1}; // positional light
  glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
  
  glEnable(GL_LIGHT0); // MUST enable each light source after configuration

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  int posX=0, posY=0, posZ=5;
  int targetX=0, targetY=0, targetZ=0;
  int upX=0, upY=1, upZ=0;
  gluLookAt(posX, posY, posZ, targetX, targetY, targetZ, upX, upY, upZ);

}

void
initVBO(void)
{

  // create vertex buffer objects, you need to delete them when program exits
  // Try to put both vertex coords array, vertex normal array and vertex color 
  //   in the same buffer object.
  // glBufferDataARB with NULL pointer reserves only memory space.
  // Copy actual data with 2 calls of glBufferSubDataARB, one for vertex coords 
  //   and one for normals.
  // target flag is GL_ARRAY_BUFFER_ARB, and usage flag is GL_STATIC_DRAW_ARB
  glGenBuffers(bufSize, vboId);
  printf ("glGenBuffers1 -> vboId = %d %d %d\n", vboId[0], vboId[1], vboId[2]);
  printf ("glGenBuffers2 -> vboId = %d %d %d\n", vboId[3], vboId[4], vboId[5]);


}

int
main(int argc, char **argv)
{

  glutInit(&argc, argv);

  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(800, 600);
  glutInitWindowPosition(400, 300);
  glutCreateWindow("color cube by vertex buffer object");
  printf ("glversion = %s\n", glGetString(GL_VERSION));
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);

  init();
  initVBO();

  glutMainLoop();
  return 0;             /* ANSI C requires main to return int. */
}
