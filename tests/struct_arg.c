#include <stdio.h>

struct S {
  int x;
  int y;
};

struct S gs = {100, 200};

int f(struct S s) {
  return s.y;
}

int main() {
  struct S s = {3, 4};
  printf("f(s) = %d\n", f(s));
  printf("f(gs) = %d\n", f(gs));
  return 0;
}
