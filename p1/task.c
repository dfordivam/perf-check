#include <stdio.h>
#include <stdlib.h>

#define INT_ARRAY_SIZE 1000
#define NUM_OF_ARRAYS 1000000
#define NUM_ITERATIONS 100

void task1(int* ptr);

static struct taskData {
  int* intArrays[NUM_OF_ARRAYS];
} gl_taskData;

void runTasks()
{
    for (int j = 0; j < NUM_ITERATIONS ; j++){
        for (int i = 0; i < NUM_OF_ARRAYS ; i++)
        {
            task1(gl_taskData.intArrays[i]);
        }
    }
}

void runTasks_OpenMP()
{
  int chunk = 10;                    /* set loop iteration chunk size */
/*** Spawn a parallel region explicitly scoping all variables ***/
 #pragma omp parallel shared(chunk)
    for (int j = 0; j < NUM_ITERATIONS ; j++){
        #pragma omp for schedule (static, NUM_OF_ARRAYS/chunk) 
        for (int i = 0; i < NUM_OF_ARRAYS ; i++)
        {
            task1(gl_taskData.intArrays[i]);
        }
    }
}

void initTasks()
{
  printf("initTasks\n");
  for (int i = 0; i < NUM_OF_ARRAYS ; i++)
  {
    gl_taskData.intArrays[i] = 
      (int*) malloc(sizeof(int)*INT_ARRAY_SIZE);
  }
}

// Task with an array of integers
void task1(int* ptr)
{
  for (int i = 0; i < INT_ARRAY_SIZE; i++)
  {
    ptr[i] = ptr[i] + ptr[i+1];
  }
}
