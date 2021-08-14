#include <iostream>
#include <thread>
#include <mutex>

using namespace std;

mutex data_guard;
int result = 0;

void DoWork(int loop_count) {
  for (auto i = 0; i < loop_count; ++i) {
    lock_guard<mutex> guard(data_guard);
    result += 1;
  }
}

int main() {
  thread worker1(DoWork, 100);
  thread worker2(DoWork, 150);

  worker1.join();
  worker2.join();

  cout << "result = " << result << endl;
}
