#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "multi_out.h"

int deleteContext(CUcontext ct)
{
  
  return 100;
}

int update_int (int* state)
{
  (*state) = *state * 100;
  return 99;

}

int save_int(int size, int* input)
{
  printf("input-sze=%d\n", size);

  int i, counter=0;
  for(i=0;i<size;i++)
  {
    input[i] = input[i] + 10;
    counter += i;
    printf ("input content = %d, %d\n", input[i], counter);
  }
  return counter;
}

int save_char(int size, char** input)
{
  printf("input size = %d\n", size);
  char** store = (char**) malloc((sizeof (char*)) * size);

  int count=0, i, j;
  char* str;

  for(i=0;i<size;i++)
  {
    str = input[i];
    printf ("%d: %s--%d\n", i, str, strlen(str));
    store[i] = (char*)malloc(sizeof(char)*strlen(str));
    for(j=0;j<strlen(str);j++)
    {
      store[i][j] = input[i][j];
      //input[i][j] = '*';
      //store[i][j] = '*';
      printf ("%d, %d: char = %c, count = %d\n", i, j, store[i][j], count);
      count++;
    }
  }

  return count;
}


