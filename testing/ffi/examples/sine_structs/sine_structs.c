#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct
{ float in;
  float out;
} SineStruct;

float 
sine_value (float in)
{
    return sin(in);
}

float[] 
sine_array_nptr(int num, float [] ins)
{
  int i=0;
  float [] outs = malloc(sizeof(float) * num);
  for(i = 0 ; i < num ; i++)
    outs[i] = sine_value(ins[i]);

  return outs;
}

float* 
sine_array (int num, const float* ins)
{
  int i=0;
  float* outs = malloc(sizeof(float) * num);
  for(i = 0 ; i < num ; i++)
    outs[i] = sine_value(ins[i]);

  return outs;
}

SineStruct*
sine_struct (SineStruct* ss)
{
  SineStruct* sS = (SineStruct*) malloc (sizeof(SineStruct));
  float in = ss->in;
  sS->in = in;  
  sS->out = sine_value(in);

  return sS;
}

SineStruct**
sine_struct_array (int num, SineStruct** ss)
{
  int i = 0;
  SineStruct** outs = (SineStruct**)malloc(sizeof(SineStruct*) * num);
  for(i=0;i<num;i++)
  {
      outs[i] = (SineStruct*)malloc(sizeof(SineStruct));
      outs[i]->in = ss[i]->in;
      outs[i]->out = sine_value(ss[i]->in);
  }
      
  return outs;
}

SineStruct*
sine_struct_array_inlined(int num, SineStruct* ss)
{
    int i = 0; 
    SineStruct* outs = (SineStruct*) malloc(sizeof(SineStruct) * num);
    for(i=0;i<num;i++)
    {
	SineStruct sst;
	sst.in = ss[i].in;
	sst.out = ss[i].in * 10;

	outs[i] = sst;
    }

    return outs;
}

SineStruct*
gen_sine_struct(void)
{
  SineStruct* s = (SineStruct*)malloc(sizeof(SineStruct));
  s->in = 1.0;
  s->out = 1.0;
  return s;
}

int 
main(int argc, char** argv)
{
  int i=0;
  int num_input = 3;
  const float ins [] = {0.0f, 1.75f, 3.14f}; 
  float* outs = sine_array (num_input, ins);
  for(i = 0 ; i < num_input ; i++)
    printf ("out %f [] = %f\n", ins[i], outs[i]);

  SineStruct* ss = malloc(sizeof(SineStruct));
  ss->in = 1.75;

  ss = sine_struct(ss);

  printf ("ss->in = %f, out = %f\n", ss->in, ss->out);

  /* array of pointers */
  SineStruct** sarray = (SineStruct**)malloc(sizeof(SineStruct*)*num_input);
  for(i=0 ; i < num_input ; i++)
  {
      sarray[i] = (SineStruct*)malloc(sizeof(SineStruct));
      sarray[i]->in = ins[i];
  }

  SineStruct** sa_out = sine_struct_array(num_input, sarray);
  for(i=0 ; i < num_input ; i++)
  printf("arrayout %f [] = %f\n", sa_out[i]->in, sa_out[i]->out);

  SineStruct* s = gen_sine_struct();
  printf("gen'd str: in=%f, out=%f\n", s->in, s->out);

  /* array of SinStructs  */
  //SineStruct* sa_inl_in = (SineStruct*) malloc (sizeof(SineStruct)*num_input);
  SineStruct* sa_inl_in = (SineStruct*)malloc(sizeof(SineStruct)* num_input);

  for(i=0 ; i < num_input ; i++)
  {
      sa_inl_in[i].in = i;
      sa_inl_in[i].out = i*10;
  }

  SineStruct* sa_inl_out = sine_struct_array_inlined (num_input, sa_inl_in);
  for(i=0 ; i < num_input ; i++)
    printf("inlined arrayout %f [] = %f\n", sa_inl_out[i].in, sa_inl_out[i].out);
  return 0;
}






