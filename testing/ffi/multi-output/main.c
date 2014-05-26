#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "multi_out.h"

int main(int argc, char** argv)
{
  int n = save_char(argc, argv);
  int i=0;
  printf ("output  = %d\n", n);

  for(i=0;i<argc;i++)
    printf ("%s\n", argv[i]);


  int inint [] = {1,2,3,4,5};
  int m = save_int(5, inint);
  int j=0;
  for(j=0;j<5;j++)
    printf("%d\n", inint[j]);

  int a = 100;
  int l = update_int(&a);
  printf ("pair = %d, %d\n", a, l);

  CUcontext tt;
  int o = deleteContext(tt);
  printf ("context = %d\n", o);

  return 0;
}
