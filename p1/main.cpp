#include <stdio.h>
#include <time.h>

void initTasks();
void runTasks_OpenMP();
void runTasks();

void profileTask(void (*ptr)());

int main()
{
  initTasks();
  profileTask(runTasks_OpenMP);
  profileTask(runTasks);
  printf("Done\n");
}

void profileTask(void (*ptr)())
{
  clock_t t;
  t = clock();
  (*ptr)();
  t = clock() -t;
  printf("Profile :%f secs\n", ((float)t/CLOCKS_PER_SEC));
}
