#ifndef __VBO01_H__
#define __VBO01_H__

#include "vbo.h"

struct CTest01 : public ITest
{
    virtual void Begin();
    virtual void End();
    virtual void Render();
};

#endif//__VBO01_H__
