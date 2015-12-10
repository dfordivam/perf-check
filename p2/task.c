#include <stdio.h>
#include <stdlib.h>

#define INT_ARRAY_SIZE 100
#define NUM_OF_ARRAYS 10000
#define NUM_ITERATIONS 10000

void task1(int* ptr);

static int task_exec_count = 0;

void clearCount()
{
  task_exec_count = 0;
}

void printCount()
{
  printf("Task Exec Count = %d\n", task_exec_count);
}

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
  int chunk = 4;                    /* set loop iteration chunk size */
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

// 4 Tasks working on different arrays
void runTasks_1()
{
  for (int i = 0; i < NUM_OF_ARRAYS/4 ; i++)
  {
    task1(gl_taskData.intArrays[i]);
  }
}

void runTasks_2()
{
  for (int i = NUM_OF_ARRAYS/4; i < NUM_OF_ARRAYS/2 ; i++)
  {
    task1(gl_taskData.intArrays[i]);
  }
}
void runTasks_3()
{
  for (int i = NUM_OF_ARRAYS/2; i < 3*(NUM_OF_ARRAYS/4) ; i++)
  {
    task1(gl_taskData.intArrays[i]);
  }
}
void runTasks_4()
{
  for (int i = 3*(NUM_OF_ARRAYS/4); i < NUM_OF_ARRAYS ; i++)
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
  task_exec_count++;
  for (int i = 0; i < INT_ARRAY_SIZE; i++)
  {
    ptr[i] = ptr[i] + ptr[i+1];
  }
}
