#include <stdio.h>
#include <stddef.h>

struct S {
  char c;
  long p;
  int x;
};

int main() {
  printf("%lu\n", offsetof(struct S, x));
  return 0;
}
