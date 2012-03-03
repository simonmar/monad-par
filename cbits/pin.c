#define _GNU_SOURCE
#include <pthread.h>
#include <stdlib.h>

int pin_pthread(int cpu) {
  cpu_set_t cs;
  pthread_t thread;

  thread = pthread_self();

  CPU_ZERO(&cs);
  CPU_SET(cpu, &cs);
  return pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cs);

}
