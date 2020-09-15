#include <stdio.h>

int fib(int n) {
  if (n <= 2) {
    return 1;
  }
  return fib(n-1) + fib(n-2);
}

int main() {
  int i;
  for (i = 1; i <= 10; i++) {
    printf("fib(%d) = %d\n", i, fib(i));
  }
  return 0;
}
