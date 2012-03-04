#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdlib.h>

int pin_pthread(int cpu) {
  cpu_set_t cs;
  pthread_t thread;

  thread = pthread_self();

  CPU_ZERO(&cs);
  CPU_SET(cpu, &cs);
  return pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cs);
}

int pin_process_multi(int cpus[], int num) {
  cpu_set_t cs;
  int i;
  size_t size;

  size = CPU_ALLOC_SIZE(CPU_COUNT(&cs));

  CPU_ZERO(&cs);
  for (i=0; i<num; i++) {
    CPU_SET(cpus[i], &cs);
  }

  return sched_setaffinity(0, size, &cs);
}
