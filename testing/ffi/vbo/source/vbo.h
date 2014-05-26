#ifndef __VBO_H__
#define __VBO_H__

struct ITest
{
    virtual void Begin() = 0;
    virtual void End() = 0;
    virtual void Render() = 0;
};

#endif//__VBO_H__
