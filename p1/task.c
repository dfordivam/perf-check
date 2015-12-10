#include <stdio.h>
#include <stdlib.h>

#define INT_ARRAY_SIZE 10000
#define NUM_OF_ARRAYS 10000

void task1(int* ptr);

static struct taskData {
  int* intArrays[NUM_OF_ARRAYS];
} gl_taskData;

void runTasks()
{
  for (int i = 0; i < NUM_OF_ARRAYS ; i++)
  {
    task1(gl_taskData.intArrays[i]);
  }
}

void runTasks_OpenMP()
{
  #pragma omp for schedule (auto)
  for (int i = 0; i < NUM_OF_ARRAYS ; i++)
  {
    task1(gl_taskData.intArrays[i]);
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
