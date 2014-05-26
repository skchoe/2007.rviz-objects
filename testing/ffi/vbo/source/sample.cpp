#include "precomputed.h"
#include "vbo01.h"
#include "vbo02.h"
#include "vbo03.h"
#include "vbo04.h"
#include "vbo05.h"
#include "vbo06.h"
#include "vbo07.h"
#include <SDL/SDL.h>

SDL_Surface* Surface;

bool CreateWindow(const char* Name, int Width, int Height, bool Fullscreen)
{
	if(SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER) < 0)
		return false;

	unsigned int VideoFlags = SDL_OPENGL | SDL_DOUBLEBUF;
	if(Fullscreen)
		VideoFlags |= SDL_FULLSCREEN;

	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 0);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_RED_SIZE, 0);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_GREEN_SIZE, 0);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_BLUE_SIZE, 0);
    SDL_GL_SetAttribute(SDL_GL_ACCUM_ALPHA_SIZE, 0);

	if((Surface = SDL_SetVideoMode(Width, Height, 32, VideoFlags)) == 0)
		return false;

	SDL_WM_SetCaption(Name, Name);

    glewInit();

    return true;
}

void DeleteWindow()
{
	if(Surface)
		SDL_FreeSurface(Surface);
}

int main(int argc, char* argv[])
{
    CreateWindow("VBO test", 640, 480, false);

    ITest* Test = NULL;

    int TestIndex = 1;
    if(argc > 1)
        TestIndex = atoi(argv[1]);

    switch(TestIndex)
    {
    case 1:
        Test = new CTest01;
        break;
    case 2:
        Test = new CTest02;
        break;
    case 3:
        Test = new CTest03;
        break;
    case 4:
        Test = new CTest04;
        break;
    case 5:
        Test = new CTest05;
        break;
    case 6:
        Test = new CTest06;
        break;
    case 7:
        Test = new CTest07;
        break;
    }

    Test->Begin();

    bool Exit = false;
    while(!Exit)
	{
		SDL_Event event;
		while(SDL_PollEvent(&event))
		{
			switch(event.type)
			{
			case SDL_QUIT:
			case SDL_KEYUP:
				Exit = true;
				break;
			}
		}

        Test->Render();

        // Swap framebuffers
        SDL_GL_SwapBuffers();

	    int Error;
	    if((Error = glGetError()) != GL_NO_ERROR)
	    {
		    const char* Message = (const char*)gluErrorString(Error);
            fprintf(stderr, "OpenGL Error : %s\n", Message);
	    }
	}

    Test->End();
    delete Test;

    DeleteWindow();

	SDL_Quit();

	return 0;
}
