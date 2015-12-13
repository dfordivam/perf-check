#include <stdio.h>
#include <stdlib.h>
#include "main_stub.h"

// Design details
// We need to do 'NUM_TASKS' tasks in each iteration.
// After each iteration the threads need to synchronize.

// What is a task
// It takes an int 'value', and it does some simple operations
// on the memory intArrays[value]
// It will not read/write memory outside the intArray[value]

// In an iteration the list of tasks is specified by 
// a list of int in execTasks
// This list is populated with random numbers.
// So distribution of tasks to threads is random

// Number of tasks in each delta
#define NUM_TASKS 1000
// Number of deltas
#define NUM_ITERATIONS 10000

// Variables to set Memory
#define INT_ARRAY_SIZE 1000
#define NUM_OF_ARRAYS 200000

#define HEAVY_LOOP 1000

// Controls How many arrays are accessed by task
// This is to change how much memory is accessed
#define TASK_LOCALIZATION 2

static struct taskData {
  HsStablePtr globalDataPtr;

  int* intArrays[NUM_OF_ARRAYS];

  int referenceResult[NUM_OF_ARRAYS];
  int taskResult[NUM_OF_ARRAYS];

  unsigned int execTasksCurrent[NUM_TASKS];
  unsigned int execTasks[NUM_ITERATIONS][NUM_TASKS];

  int counterValue;
  void (*counterFunction)(HsStablePtr ptr, int val);
} gl_taskData;

// Counter to give some idea about task execution count
// Also shows the effect of not doing lock based coding
static int task_exec_count = 0;

void task1(unsigned int index);
void generateExecTasks();

// Functions which execute equal number of tasks, but include
// some heavyTask in between.
void runTasks_1(int);
void runTasks_2(int);
void runTasks_3(int);
void runTasks_4(int);

// Counters to see the effect of doing atmoic operation
// after every task execution
void counterFunctionCSide(HsStablePtr ptr, int val);
void counterFunctionLockedCSide(HsStablePtr ptr, int val);

// Single threaded execution
void runTasks()
{
    for (int j = 0; j < NUM_ITERATIONS ; j++){
      initExecTaskCurrent(j);
      runTasks_1(j);
      runTasks_2(j);
      runTasks_3(j);
      runTasks_4(j);
    }
  static int getReferenceResult = 1;
  if (getReferenceResult) {
    // Sample the results to reference
    memcpy(gl_taskData.referenceResult, 
        gl_taskData.taskResult, NUM_OF_ARRAYS);
    getReferenceResult = 0;
  }
}

// OpenMP execution
void runTasks_OpenMP()
{
  int chunk = 4;                    /* set loop iteration chunk size */
/*** Spawn a parallel region explicitly scoping all variables ***/
 #pragma omp parallel shared(chunk)
  for (int j = 0; j < NUM_ITERATIONS ; j++){
      initExecTaskCurrent(j);

 #pragma omp single 
      {
        #pragma omp task
        runTasks_1(j);

        #pragma omp task
        runTasks_2(j);

        #pragma omp task
        runTasks_3(j);

        #pragma omp task
        runTasks_4(j);

        #pragma omp taskwait
      }
  }
}

// This is computation heavy task
void heavyTask()
{
  static int number = 7;
  for (int i = 0; i < HEAVY_LOOP ; i++)
  {
    number = number * number;
  }
  // Some random code so that it does not get optimized
  if (number > task_exec_count) number = 9;
}

// Threads for haskell
// 4 Tasks working on different arrays
void runTasks_1(int iteration)
{
  for (int i = 0; i < NUM_TASKS/4 ; i++)
  {
    // 10%
    int isHeavy = (i % 10) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasksCurrent[i]);
  }
}

void runTasks_2(int iteration)
{
  for (int i = NUM_TASKS/4; i < NUM_TASKS/2 ; i++)
  {
    // 50%
    int isHeavy = (i % 2) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasksCurrent[i]);
  }
}

void runTasks_3(int iteration)
{
  for (int i = NUM_TASKS/2; i < 3*(NUM_TASKS/4) ; i++)
  {
    // 5%
    int isHeavy = (i % 20) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasksCurrent[i]);
  }
}

void runTasks_4(int iteration)
{
  for (int i = 3*(NUM_TASKS/4); i < NUM_TASKS ; i++)
  {
    // 2%
    int isHeavy = (i % 50) == 0;
    if (isHeavy) heavyTask();
    task1(gl_taskData.execTasksCurrent[i]);
  }
}

void generateExecTasks()
{
  printf("generateExecTasks\n");
  for (int j = 0; j < NUM_ITERATIONS ; j++){
    for (int i = 0; i < NUM_TASKS ; i++)
    {
      //gl_taskData.execTasks[i] = (NUM_OF_ARRAYS * i)/NUM_TASKS;
      gl_taskData.execTasks[j][i] = ((rand() % NUM_OF_ARRAYS)/TASK_LOCALIZATION)*TASK_LOCALIZATION;
    }
  }
}

// Initialize array with value 1
static int intArrayInitValue[INT_ARRAY_SIZE] = { [0 ... (INT_ARRAY_SIZE-1)] = 1};

// Allocate arrays and set global variable values
void doInitialization(HsStablePtr ptr)
{
  gl_taskData.counterFunction = counterFunctionLockedCSide;
  gl_taskData.globalDataPtr = ptr;
  printf("doInitialization\n");
  for (int i = 0; i < NUM_OF_ARRAYS ; i++)
  {
    gl_taskData.intArrays[i] = 
      (int*) malloc(sizeof(int)*INT_ARRAY_SIZE);
  }
  generateExecTasks();
  initArrays();
}

// Initializes arrays with initValue
void initArrays()
{
  printf("Init Arrays\n");
  for (int i = 0; i < NUM_OF_ARRAYS ; i++)
  {
    memcpy(gl_taskData.intArrays[i], intArrayInitValue, sizeof(int)*INT_ARRAY_SIZE);
  }
}

void initExecTaskCurrent(int j)
{
  memcpy(gl_taskData.execTasksCurrent, gl_taskData.execTasks[j],
      NUM_TASKS*sizeof(int));
}

// Task - This reads a part of the array
// And produces a result by adding
void task1(unsigned int value)
{
  int* ptr = gl_taskData.intArrays[value];
  task_exec_count++;
  int result = 0;
  int end = (INT_ARRAY_SIZE/10);
  for (int i = 0; i < end; i++)
  {
    ptr[i] = ptr[i] + ptr[end-i-1];
    result += ptr[i];
  }

  gl_taskData.taskResult[value] = result;

  (*(gl_taskData.counterFunction))(gl_taskData.globalDataPtr, ptr[0]);
}

void compareTaskResultWithReference()
{
  int val = memcmp(gl_taskData.referenceResult,
      gl_taskData.taskResult,
      NUM_OF_ARRAYS);
  if (val) {
    printf("\tMismatch in result\n");
  } else {
    printf("\tResult match!\n");
  }
}

void counterFunctionLockedCSide(HsStablePtr ptr, int val)
{
#pragma omp atomic
  gl_taskData.counterValue += 1;
}

void counterFunctionCSide(HsStablePtr ptr, int val)
{
  gl_taskData.counterValue += 1;
}

// -------------------------------------------------
// MISC APIs
void clearCount()
{
  gl_taskData.counterValue = 0;
  task_exec_count = 0;
}

void printCount()
{
  /* printf("Task Exec Count = %d\n", task_exec_count); */
  /* printf("Locked Task Exec Count = %d\n", gl_taskData.counterValue); */
}

