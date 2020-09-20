#include <stdio.h>

void f() {
  printf("f\n");
}
void g() {
  printf("g\n");
}

int main() {
  f();
  g();
  1 ? f() : (void) 999;
  g();
  0 ? f() : (void) 9999;
  return 0;
}
