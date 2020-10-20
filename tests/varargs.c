#include <stdio.h>
#include <stdarg.h>

void g2(va_list ap) {
  printf("%d %s %d\n", va_arg(ap, int), va_arg(ap, char *), va_arg(ap, int));
}

void g(int n, ...) {
  va_list ap;
  va_start(ap, n);
  g2(ap);
  va_end(ap);
}

void f(int n, ...) {
  va_list ap;
  va_start(ap, n);
  printf("%d %s %d\n", va_arg(ap, int), va_arg(ap, char *), va_arg(ap, int));
  va_end(ap);
}

int main() {
  f(0, 99, "hello", 42);
  g(0, 13, "world", 71);
  return 0;
}
