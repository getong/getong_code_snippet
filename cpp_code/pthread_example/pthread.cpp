#include <iostream>
#include <pthread.h>

using namespace std;

void *DoWork(void *data) {
  // const int loop_count = (int) data;
  const int loop_count = (uintptr_t) data;
  for (int i = 0; i < loop_count; ++i) {
    cout << "Hello world " << i << endl;
  }
  pthread_exit(NULL);
}

int main() {
  pthread_t worker_thread;
  int result = pthread_create(&worker_thread, NULL, DoWork, (void *) 100);
  // Wait for the thread to end
  result = pthread_join(worker_thread, NULL);
}
