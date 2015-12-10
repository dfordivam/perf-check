#include <stdio.h>
#include <iostream>
#include <chrono>
#include <time.h>

void initTasks();
void runTasks_OpenMP();
void runTasks();

void profileTask(void (*ptr)());

int main()
{
  initTasks();
  profileTask(runTasks);
  profileTask(runTasks_OpenMP);
  printf("Done\n");
}

void profileTask(void (*ptr)())
{
  std::chrono::time_point<std::chrono::system_clock> start, end;
  start = std::chrono::system_clock::now();
  clock_t t;
  t = clock();
  (*ptr)();
  t = clock() -t;
  end = std::chrono::system_clock::now();
  std::chrono::duration<double> elapsed_seconds = end-start;
  printf("Profile :%f cpu secs\n", ((float)t/CLOCKS_PER_SEC));
  std::cout << "elapsed time: " << elapsed_seconds.count() << "s\n";
}
