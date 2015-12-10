#include <stdio.h>
#include <stdlib.h>

//#define USE_FOR_PRAGMA 1
// Design details
// We need to do 'NUM_TASKS' tasks in each iteration.
// After each iteration the threads need to synchronize.

// What is a task
// It takes an int 'value', and it does some simple operations
// on the memory intArrays[value]
// It will not read memory outside the indexed

// In an iteration the list of tasks is specified by 
// a list of int in execTasks
// This list is populated with random number before each iteration
#define NUM_TASKS 1000
#define NUM_ITERATIONS 10000

// Variables to set Memory
#define INT_ARRAY_SIZE 100
#define NUM_OF_ARRAYS 10000

#define HEAVY_LOOP 10000

static struct taskData {
  int* intArrays[NUM_OF_ARRAYS];
  unsigned int execTasks[NUM_TASKS];
} gl_taskData;

// Counter to give some idea about task execution count
// Also shows the effect of not doing lock based coding
static int task_exec_count = 0;

void task1(unsigned int index);
void generateExecTasks();

void runTasks_1();
void runTasks_2();
void runTasks_3();
void runTasks_4();

// Single threaded execution
void runTasks()
{
    for (int j = 0; j < NUM_ITERATIONS ; j++){
      generateExecTasks();
      runTasks_1();
      runTasks_2();
      runTasks_3();
      runTasks_4();
    }
}

// OpenMP execution
void runTasks_OpenMP()
{
  int chunk = 1;                    /* set loop iteration chunk size */
/*** Spawn a parallel region explicitly scoping all variables ***/
 #pragma omp parallel shared(chunk)
    for (int j = 0; j < NUM_ITERATIONS ; j++){
      generateExecTasks();
#ifdef USE_FOR_PRAGMA
        #pragma omp for schedule (static, NUM_TASKS/chunk) 
        for (int i = 0; i < NUM_TASKS ; i++)
        {
            task1(gl_taskData.execTasks[i]);
        }
#else
#pragma omp single 
      {
        #pragma omp task
        runTasks_1();

        #pragma omp task
        runTasks_2();

        /* #pragma omp taskwait */

      /* } */
/* #pragma omp single */ 
      /* { */
        #pragma omp task
        runTasks_3();

        #pragma omp task
        runTasks_4();

        #pragma omp taskwait
      }
#endif
    }
}

void heavyTask()
{
  int number = 7;
  for (int i = 0; i < HEAVY_LOOP ; i++)
  {
    number = number * number;
  }
}

// Threads for haskell
// 4 Tasks working on different arrays
void runTasks_1()
{
  for (int i = 0; i < NUM_TASKS/4 ; i++)
  {
    // 10% chance
    int isHeavy = (rand() % 10) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasks[i]);
  }
}

void runTasks_2()
{
  for (int i = NUM_TASKS/4; i < NUM_TASKS/2 ; i++)
  {
    // 20% chance
    int isHeavy = (rand() % 5) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasks[i]);
  }
}

void runTasks_3()
{
  for (int i = NUM_TASKS/2; i < 3*(NUM_TASKS/4) ; i++)
  {
    // 5% chance
    int isHeavy = (rand() % 20) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasks[i]);
  }
}

void runTasks_4()
{
  for (int i = 3*(NUM_TASKS/4); i < NUM_TASKS ; i++)
  {
    // 2% chance
    int isHeavy = (rand() % 50) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasks[i]);
  }
}

void generateExecTasks()
{
  for (int i = 0; i < NUM_TASKS ; i++)
  {
    gl_taskData.execTasks[i] = rand() % NUM_OF_ARRAYS;
  }
}

// Initializes arrays on the heap
void initArrays()
{
  printf("initArrays\n");
  for (int i = 0; i < NUM_OF_ARRAYS ; i++)
  {
    gl_taskData.intArrays[i] = 
      (int*) malloc(sizeof(int)*INT_ARRAY_SIZE);
  }
}

// Task with an array of integers
void task1(unsigned int value)
{
  int* ptr = gl_taskData.intArrays[value];
  task_exec_count++;
  for (int i = 0; i < INT_ARRAY_SIZE; i++)
  {
    ptr[i] = ptr[i] + ptr[i+1];
  }
}

// -------------------------------------------------
// MISC APIs
void clearCount()
{
  task_exec_count = 0;
}

void printCount()
{
  printf("Task Exec Count = %d\n", task_exec_count);
}

