///////////////////////////////////////////////////////////////////////////////
// main.cpp
// ========
// testing Pixel Buffer Object for unpacking (uploading) pixel data to PBO
// using GL_ARB_pixel_buffer_object extension
// It uses 2 PBOs to optimize uploading pipeline; application to PBO, and PBO to
// texture object.
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2007-10-22
// UPDATED: 2007-11-30
///////////////////////////////////////////////////////////////////////////////

// in order to get function prototypes from glext.h, define GL_GLEXT_PROTOTYPES before including glext.h
#define GL_GLEXT_PROTOTYPES

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include "glext.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include "glInfo.h"                             // glInfo struct
#include "Timer.h"

using std::stringstream;
using std::cout;
using std::endl;
using std::ends;


// GLUT CALLBACK functions
void displayCB();
void reshapeCB(int w, int h);
void timerCB(int millisec);
void idleCB();
void keyboardCB(unsigned char key, int x, int y);
void mouseCB(int button, int stat, int x, int y);
void mouseMotionCB(int x, int y);

void initGL();
int  initGLUT(int argc, char **argv);
bool initSharedMem();
void clearSharedMem();
void initLights();
void setCamera(float posX, float posY, float posZ, float targetX, float targetY, float targetZ);
void updatePixels(GLubyte* dst, int size);
void drawString(const char *str, int x, int y, float color[4], void *font);
void drawString3D(const char *str, float pos[3], float color[4], void *font);
void showInfo();
void showTransferRate();
void printTransferRate();


// constants
const int IMAGE_WIDTH = 1024;
const int IMAGE_HEIGHT = 1024;
const int CHANNEL_COUNT = 4;
const int DATA_SIZE = IMAGE_WIDTH * IMAGE_HEIGHT * CHANNEL_COUNT;
const GLenum PIXEL_FORMAT = GL_BGRA;

// global variables
void *font = GLUT_BITMAP_8_BY_13;
GLuint pboIds[2];                   // IDs of PBO
GLuint textureId;                   // ID of texture
GLubyte* imageData = 0;             // pointer to texture buffer
bool mouseLeftDown;
bool mouseRightDown;
float mouseX, mouseY;
float cameraAngleX;
float cameraAngleY;
float cameraDistance;
bool pboSupported;
int pboMode;
int drawMode = 0;
Timer timer, t1, t2;
float copyTime, updateTime;


// function pointers for PBO Extension
// Windows needs to get function pointers from ICD OpenGL drivers,
// because opengl32.dll does not support extensions higher than v1.1.
#ifdef _WIN32
PFNGLGENBUFFERSARBPROC pglGenBuffersARB = 0;                     // VBO Name Generation Procedure
PFNGLBINDBUFFERARBPROC pglBindBufferARB = 0;                     // VBO Bind Procedure
PFNGLBUFFERDATAARBPROC pglBufferDataARB = 0;                     // VBO Data Loading Procedure
PFNGLBUFFERSUBDATAARBPROC pglBufferSubDataARB = 0;               // VBO Sub Data Loading Procedure
PFNGLDELETEBUFFERSARBPROC pglDeleteBuffersARB = 0;               // VBO Deletion Procedure
PFNGLGETBUFFERPARAMETERIVARBPROC pglGetBufferParameterivARB = 0; // return various parameters of VBO
PFNGLMAPBUFFERARBPROC pglMapBufferARB = 0;                       // map VBO procedure
PFNGLUNMAPBUFFERARBPROC pglUnmapBufferARB = 0;                   // unmap VBO procedure
#define glGenBuffersARB           pglGenBuffersARB
#define glBindBufferARB           pglBindBufferARB
#define glBufferDataARB           pglBufferDataARB
#define glBufferSubDataARB        pglBufferSubDataARB
#define glDeleteBuffersARB        pglDeleteBuffersARB
#define glGetBufferParameterivARB pglGetBufferParameterivARB
#define glMapBufferARB            pglMapBufferARB
#define glUnmapBufferARB          pglUnmapBufferARB
#endif


///////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
    initSharedMem();

    // init GLUT and GL
    initGLUT(argc, argv);
    initGL();

    // get OpenGL info
    glInfo glInfo;
    glInfo.getInfo();
    //glInfo.printSelf();

    // init 2 texture objects
    glGenTextures(1, &textureId);
    glBindTexture(GL_TEXTURE_2D, textureId);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, IMAGE_WIDTH, IMAGE_HEIGHT, 0, PIXEL_FORMAT, GL_UNSIGNED_BYTE, (GLvoid*)imageData);
    glBindTexture(GL_TEXTURE_2D, 0);

#ifdef _WIN32
    // check PBO is supported by your video card
    if(glInfo.isExtensionSupported("GL_ARB_pixel_buffer_object"))
    {
        // get pointers to GL functions
        glGenBuffersARB = (PFNGLGENBUFFERSARBPROC)wglGetProcAddress("glGenBuffersARB");
        glBindBufferARB = (PFNGLBINDBUFFERARBPROC)wglGetProcAddress("glBindBufferARB");
        glBufferDataARB = (PFNGLBUFFERDATAARBPROC)wglGetProcAddress("glBufferDataARB");
        glBufferSubDataARB = (PFNGLBUFFERSUBDATAARBPROC)wglGetProcAddress("glBufferSubDataARB");
        glDeleteBuffersARB = (PFNGLDELETEBUFFERSARBPROC)wglGetProcAddress("glDeleteBuffersARB");
        glGetBufferParameterivARB = (PFNGLGETBUFFERPARAMETERIVARBPROC)wglGetProcAddress("glGetBufferParameterivARB");
        glMapBufferARB = (PFNGLMAPBUFFERARBPROC)wglGetProcAddress("glMapBufferARB");
        glUnmapBufferARB = (PFNGLUNMAPBUFFERARBPROC)wglGetProcAddress("glUnmapBufferARB");

        // check once again PBO extension
        if(glGenBuffersARB && glBindBufferARB && glBufferDataARB && glBufferSubDataARB &&
           glMapBufferARB && glUnmapBufferARB && glDeleteBuffersARB && glGetBufferParameterivARB)
        {
            pboSupported = true;
            pboMode = 1;    // using 1 PBO
            cout << "Video card supports GL_ARB_pixel_buffer_object." << endl;
        }
        else
        {
            pboSupported = false;
            pboMode = 0;    // without PBO
            cout << "Video card does NOT support GL_ARB_pixel_buffer_object." << endl;
        }
    }

#else // for linux, do not need to get function pointers, it is up-to-date
    if(glInfo.isExtensionSupported("GL_ARB_pixel_buffer_object"))
    {
        pboSupported = true;
        pboMode = 1;
        cout << "Video card supports GL_ARB_pixel_buffer_object." << endl;
    }
    else
    {
        pboSupported = false;
        pboMode = 0;
        cout << "Video card does NOT support GL_ARB_pixel_buffer_object." << endl;
    }
#endif

    if(pboSupported)
    {
        // create 2 pixel buffer objects, you need to delete them when program exits.
        // glBufferDataARB with NULL pointer reserves only memory space.
        glGenBuffersARB(2, pboIds);
        glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, pboIds[0]);
        glBufferDataARB(GL_PIXEL_UNPACK_BUFFER_ARB, DATA_SIZE, 0, GL_STREAM_DRAW_ARB);
        glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, pboIds[1]);
        glBufferDataARB(GL_PIXEL_UNPACK_BUFFER_ARB, DATA_SIZE, 0, GL_STREAM_DRAW_ARB);
        glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    }

    // start timer, the elapsed time will be used for updateVertices()
    timer.start();

    // the last GLUT call (LOOP)
    // window will be shown and display callback is triggered by events
    // NOTE: this call never return main().
    glutMainLoop(); /* Start GLUT event-processing loop */

    return 0;
}

///////////////////////////////////////////////////////////////////////////////
// initialize GLUT for windowing
///////////////////////////////////////////////////////////////////////////////
int initGLUT(int argc, char **argv)
{
    // GLUT stuff for windowing
    // initialization openGL window.
    // it is called before any other GLUT routine
    glutInit(&argc, argv);

    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_ALPHA); // display mode

    glutInitWindowSize(400, 300);               // window size

    glutInitWindowPosition(100, 100);           // window location

    // finally, create a window with openGL context
    // Window will not displayed until glutMainLoop() is called
    // it returns a unique ID
    int handle = glutCreateWindow(argv[0]);     // param is the title of window

    // register GLUT callback functions
    glutDisplayFunc(displayCB);
    //glutTimerFunc(33, timerCB, 33);             // redraw only every given millisec
    glutIdleFunc(idleCB);                       // redraw only every given millisec
    glutReshapeFunc(reshapeCB);
    glutKeyboardFunc(keyboardCB);
    glutMouseFunc(mouseCB);
    glutMotionFunc(mouseMotionCB);

    return handle;
}



///////////////////////////////////////////////////////////////////////////////
// initialize OpenGL
// disable unused features
///////////////////////////////////////////////////////////////////////////////
void initGL()
{
    //@glShadeModel(GL_SMOOTH);                    // shading mathod: GL_SMOOTH or GL_FLAT
    glShadeModel(GL_FLAT);                      // shading mathod: GL_SMOOTH or GL_FLAT
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);      // 4-byte pixel alignment

    // enable /disable features
    //@glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    glEnable(GL_DEPTH_TEST);
    //@glEnable(GL_LIGHTING);
    glDisable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_CULL_FACE);

     // track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    glEnable(GL_COLOR_MATERIAL);

    glClearColor(0, 0, 0, 0);                   // background color
    glClearStencil(0);                          // clear stencil buffer
    glClearDepth(1.0f);                         // 0 is near, 1 is far
    glDepthFunc(GL_LEQUAL);

    //@initLights();
    setCamera(0, 0, 3, 0, 0, 0);
}



///////////////////////////////////////////////////////////////////////////////
// write 2d text using GLUT
// The projection matrix must be set to orthogonal before call this function.
///////////////////////////////////////////////////////////////////////////////
void drawString(const char *str, int x, int y, float color[4], void *font)
{
    glPushAttrib(GL_LIGHTING_BIT | GL_CURRENT_BIT); // lighting and color mask
    glDisable(GL_LIGHTING);     // need to disable lighting for proper text color
    glDisable(GL_TEXTURE_2D);

    glColor4fv(color);          // set text color
    glRasterPos2i(x, y);        // place text position

    // loop all characters in the string
    while(*str)
    {
        glutBitmapCharacter(font, *str);
        ++str;
    }

    glEnable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
    glPopAttrib();
}



///////////////////////////////////////////////////////////////////////////////
// draw a string in 3D space
///////////////////////////////////////////////////////////////////////////////
void drawString3D(const char *str, float pos[3], float color[4], void *font)
{
    glPushAttrib(GL_LIGHTING_BIT | GL_CURRENT_BIT); // lighting and color mask
    glDisable(GL_LIGHTING);     // need to disable lighting for proper text color

    glColor4fv(color);          // set text color
    glRasterPos3fv(pos);        // place text position

    // loop all characters in the string
    while(*str)
    {
        glutBitmapCharacter(font, *str);
        ++str;
    }

    glEnable(GL_LIGHTING);
    glPopAttrib();
}



///////////////////////////////////////////////////////////////////////////////
// initialize global variables
///////////////////////////////////////////////////////////////////////////////
bool initSharedMem()
{
    mouseLeftDown = mouseRightDown = false;

    // allocate texture buffer
    imageData = new GLubyte[DATA_SIZE];
    memset(imageData, 0, DATA_SIZE);

    return true;
}



///////////////////////////////////////////////////////////////////////////////
// clean up shared memory
///////////////////////////////////////////////////////////////////////////////
void clearSharedMem()
{
    // deallocate texture buffer
    delete [] imageData;
    imageData = 0;

    // clean up texture
    glDeleteTextures(1, &textureId);

    // clean up PBOs
    if(pboSupported)
    {
        glDeleteBuffersARB(2, pboIds);
    }
}



///////////////////////////////////////////////////////////////////////////////
// initialize lights
///////////////////////////////////////////////////////////////////////////////
void initLights()
{
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

    glEnable(GL_LIGHT0);                        // MUST enable each light source after configuration
}



///////////////////////////////////////////////////////////////////////////////
// set camera position and lookat direction
///////////////////////////////////////////////////////////////////////////////
void setCamera(float posX, float posY, float posZ, float targetX, float targetY, float targetZ)
{
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(posX, posY, posZ, targetX, targetY, targetZ, 0, 1, 0); // eye(x,y,z), focal(x,y,z), up(x,y,z)
}



///////////////////////////////////////////////////////////////////////////////
// copy an image data to texture buffer
///////////////////////////////////////////////////////////////////////////////
void updatePixels(GLubyte* dst, int size)
{
    static int color = 0;

    if(!dst)
        return;

    int* ptr = (int*)dst;

    // copy 4 bytes at once
    for(int i = 0; i < IMAGE_HEIGHT; ++i)
    {
        for(int j = 0; j < IMAGE_WIDTH; ++j)
        {
            *ptr = color;
            ++ptr;
        }
        color += 257;   // add an arbitary number (no meaning)
    }
    ++color;            // scroll down
}



///////////////////////////////////////////////////////////////////////////////
// display info messages
///////////////////////////////////////////////////////////////////////////////
void showInfo()
{
    // backup current model-view matrix
    glPushMatrix();                     // save current modelview matrix
    glLoadIdentity();                   // reset modelview matrix

    // set to 2D orthogonal projection
    glMatrixMode(GL_PROJECTION);     // switch to projection matrix
    glPushMatrix();                  // save current projection matrix
    glLoadIdentity();                // reset projection matrix
    gluOrtho2D(0, 400, 0, 300);  // set to orthogonal projection

    float color[4] = {1, 1, 1, 1};

    stringstream ss;
    ss << "PBO: ";
    if(pboMode == 0)
        ss << "off" << ends;
    else if(pboMode == 1)
        ss << "1 PBO" << ends;
    else if(pboMode == 2)
        ss << "2 PBOs" << ends;

    drawString(ss.str().c_str(), 1, 286, color, font);
    ss.str(""); // clear buffer

    ss << std::fixed << std::setprecision(3);
    ss << "Updating Time: " << updateTime << " ms" << ends;
    drawString(ss.str().c_str(), 1, 272, color, font);
    ss.str("");

    ss << "Copying Time: " << copyTime << " ms" << ends;
    drawString(ss.str().c_str(), 1, 258, color, font);
    ss.str("");

    ss << "Press SPACE key to toggle PBO on/off." << ends;
    drawString(ss.str().c_str(), 1, 1, color, font);

    // unset floating format
    ss << std::resetiosflags(std::ios_base::fixed | std::ios_base::floatfield);

    // restore projection matrix
    glPopMatrix();                   // restore to previous projection matrix

    // restore modelview matrix
    glMatrixMode(GL_MODELVIEW);      // switch to modelview matrix
    glPopMatrix();                   // restore to previous modelview matrix
}



///////////////////////////////////////////////////////////////////////////////
// display transfer rates
///////////////////////////////////////////////////////////////////////////////
void showTransferRate()
{
    static Timer timer;
    static int count = 0;
    static stringstream ss;
    double elapsedTime;

    // backup current model-view matrix
    glPushMatrix();                     // save current modelview matrix
    glLoadIdentity();                   // reset modelview matrix

    // set to 2D orthogonal projection
    glMatrixMode(GL_PROJECTION);        // switch to projection matrix
    glPushMatrix();                     // save current projection matrix
    glLoadIdentity();                   // reset projection matrix
    gluOrtho2D(0, IMAGE_WIDTH, 0, IMAGE_HEIGHT); // set to orthogonal projection

    float color[4] = {1, 1, 0, 1};

    // update fps every second
    elapsedTime = timer.getElapsedTime();
    if(elapsedTime < 1.0)
    {
        ++count;
    }
    else
    {
        ss.str("");
        ss << std::fixed << std::setprecision(1);
        ss << "Transfer Rate: " << (count / elapsedTime) * DATA_SIZE / (1024 * 1024) << " MB" << ends; // update fps string
        ss << std::resetiosflags(std::ios_base::fixed | std::ios_base::floatfield);
        count = 0;                      // reset counter
        timer.start();                  // restart timer
    }
    drawString(ss.str().c_str(), 200, 286, color, font);

    // restore projection matrix
    glPopMatrix();                      // restore to previous projection matrix

    // restore modelview matrix
    glMatrixMode(GL_MODELVIEW);         // switch to modelview matrix
    glPopMatrix();                      // restore to previous modelview matrix
}



///////////////////////////////////////////////////////////////////////////////
// print transfer rates
///////////////////////////////////////////////////////////////////////////////
void printTransferRate()
{
    const double INV_MEGA = 1.0 / (1024 * 1024);
    static Timer timer;
    static int count = 0;
    static stringstream ss;
    double elapsedTime;

    // loop until 1 sec passed
    elapsedTime = timer.getElapsedTime();
    if(elapsedTime < 1.0)
    {
        ++count;
    }
    else
    {
        cout << std::fixed << std::setprecision(1);
        cout << "Transfer Rate: " << (count / elapsedTime) * DATA_SIZE * INV_MEGA << " MB/s. (" << count / elapsedTime << " FPS)\n";
        cout << std::resetiosflags(std::ios_base::fixed | std::ios_base::floatfield);
        count = 0;                      // reset counter
        timer.start();                  // restart timer
    }
}




//=============================================================================
// CALLBACKS
//=============================================================================

void displayCB()
{
    static int index = 0;
    int nextIndex = 0;                  // pbo index used for next frame

    if(pboMode > 0)
    {
        // increment current index first then get the next index
        // "index" is used to copy pixels from a PBO to a texture object
        // "nextIndex" is used to update pixels in a PBO
        index = (index + 1) % 2;
        if(pboMode == 1)        // with 1 PBO
            nextIndex = index;
        else if(pboMode == 2)   // with 2 PBO
            nextIndex = (index + 1) % 2;


        // start to copy from PBO to texture object ///////
        t1.start();

        // bind the texture and PBO
        glBindTexture(GL_TEXTURE_2D, textureId);
        glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, pboIds[index]);

        // copy pixels from PBO to texture object
        // Use offset instead of ponter.
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, IMAGE_WIDTH, IMAGE_HEIGHT, PIXEL_FORMAT, GL_UNSIGNED_BYTE, 0);

        // measure the time copying data from PBO to texture object
        t1.stop();
        copyTime = t1.getElapsedTimeInMilliSec();
        ///////////////////////////////////////////////////


        // start to modify pixel values ///////////////////
        t1.start();

        // bind PBO to update pixel values
        glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, pboIds[nextIndex]);

        // map the buffer object into client's memory
        // Note that glMapBufferARB() causes sync issue.
        // If GPU is working with this buffer, glMapBufferARB() will wait(stall)
        // for GPU to finish its job. To avoid waiting (stall), you can call
        // first glBufferDataARB() with NULL pointer before glMapBufferARB().
        // If you do that, the previous data in PBO will be discarded and
        // glMapBufferARB() returns a new allocated pointer immediately
        // even if GPU is still working with the previous data.
        glBufferDataARB(GL_PIXEL_UNPACK_BUFFER_ARB, DATA_SIZE, 0, GL_STREAM_DRAW_ARB);
        GLubyte* ptr = (GLubyte*)glMapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, GL_WRITE_ONLY_ARB);
        if(ptr)
        {
            // update data directly on the mapped buffer
            updatePixels(ptr, DATA_SIZE);
            glUnmapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB); // release pointer to mapping buffer
        }

        // measure the time modifying the mapped buffer
        t1.stop();
        updateTime = t1.getElapsedTimeInMilliSec();
        ///////////////////////////////////////////////////

        // it is good idea to release PBOs with ID 0 after use.
        // Once bound with 0, all pixel operations behave normal ways.
        glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    }
    else
    {
        ///////////////////////////////////////////////////
        // start to copy pixels from system memory to textrure object
        t1.start();

        glBindTexture(GL_TEXTURE_2D, textureId);
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, IMAGE_WIDTH, IMAGE_HEIGHT, PIXEL_FORMAT, GL_UNSIGNED_BYTE, (GLvoid*)imageData);

        t1.stop();
        copyTime = t1.getElapsedTimeInMilliSec();
        ///////////////////////////////////////////////////


        // start to modify pixels /////////////////////////
        t1.start();
        updatePixels(imageData, DATA_SIZE);
        t1.stop();
        updateTime = t1.getElapsedTimeInMilliSec();
        ///////////////////////////////////////////////////
    }


    // clear buffer
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    // save the initial ModelView matrix before modifying ModelView matrix
    glPushMatrix();

    // tramsform camera
    glTranslatef(0, 0, cameraDistance);
    glRotatef(cameraAngleX, 1, 0, 0);   // pitch
    glRotatef(cameraAngleY, 0, 1, 0);   // heading

    // draw a point with texture
    glBindTexture(GL_TEXTURE_2D, textureId);
    glColor4f(1, 1, 1, 1);
    glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);
    glTexCoord2f(0.0f, 0.0f);   glVertex3f(-1.0f, -1.0f, 0.0f);
    glTexCoord2f(1.0f, 0.0f);   glVertex3f( 1.0f, -1.0f, 0.0f);
    glTexCoord2f(1.0f, 1.0f);   glVertex3f( 1.0f,  1.0f, 0.0f);
    glTexCoord2f(0.0f, 1.0f);   glVertex3f(-1.0f,  1.0f, 0.0f);
    glEnd();

    // unbind texture
    glBindTexture(GL_TEXTURE_2D, 0);

    // draw info messages
    showInfo();
    //showTransferRate();
    printTransferRate();

    glPopMatrix();

    glutSwapBuffers();
}


void reshapeCB(int w, int h)
{
    // set viewport to be the entire window
    glViewport(0, 0, (GLsizei)w, (GLsizei)h);

    // set perspective viewing frustum
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    //glFrustum(-aspectRatio, aspectRatio, -1, 1, 1, 100);
    gluPerspective(60.0f, (float)(w)/h, 1.0f, 1000.0f); // FOV, AspectRatio, NearClip, FarClip

    // switch to modelview matrix in order to set scene
    glMatrixMode(GL_MODELVIEW);
}


void timerCB(int millisec)
{
    glutTimerFunc(millisec, timerCB, millisec);
    glutPostRedisplay();
}


void idleCB()
{
    glutPostRedisplay();
}


void keyboardCB(unsigned char key, int x, int y)
{
    switch(key)
    {
    case 27: // ESCAPE
        clearSharedMem();
        exit(0);
        break;

    case ' ':
        if(pboSupported)
            pboMode = ++pboMode % 3;
        cout << "PBO mode: " << pboMode << endl;
         break;

    case 'd': // switch rendering modes (fill -> wire -> point)
    case 'D':
        drawMode = ++drawMode % 3;
        if(drawMode == 0)        // fill mode
        {
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
            glEnable(GL_DEPTH_TEST);
            glEnable(GL_CULL_FACE);
        }
        else if(drawMode == 1)  // wireframe mode
        {
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
            glDisable(GL_DEPTH_TEST);
            glDisable(GL_CULL_FACE);
        }
        else                    // point mode
        {
            glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
            glDisable(GL_DEPTH_TEST);
            glDisable(GL_CULL_FACE);
        }
        break;

    default:
        ;
    }

    glutPostRedisplay();
}


void mouseCB(int button, int state, int x, int y)
{
    mouseX = x;
    mouseY = y;

    if(button == GLUT_LEFT_BUTTON)
    {
        if(state == GLUT_DOWN)
        {
            mouseLeftDown = true;
        }
        else if(state == GLUT_UP)
            mouseLeftDown = false;
    }

    else if(button == GLUT_RIGHT_BUTTON)
    {
        if(state == GLUT_DOWN)
        {
            mouseRightDown = true;
        }
        else if(state == GLUT_UP)
            mouseRightDown = false;
    }
}


void mouseMotionCB(int x, int y)
{
    if(mouseLeftDown)
    {
        cameraAngleY += (x - mouseX);
        cameraAngleX += (y - mouseY);
        mouseX = x;
        mouseY = y;
    }
    if(mouseRightDown)
    {
        cameraDistance += (y - mouseY) * 0.2f;
        mouseY = y;
    }

    //glutPostRedisplay();
}
