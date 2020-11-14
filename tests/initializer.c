#include <stddef.h>

typedef void (*FP)(void);

struct S {
  const char *s;
  FP fp;
};

void f() {}

static const struct S ss[] = {
  {"f", f},
  {"f", NULL},
  {NULL, NULL},
};

int main() {
  return 0;
}
