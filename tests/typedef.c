#include <stdio.h>

int main() {
  typedef int S;

  struct S {
    int x;
    int y;
  };

  printf("S: %lu\n", sizeof(S));
  printf("struct S: %lu\n", sizeof(struct S));
  return 0;
}
