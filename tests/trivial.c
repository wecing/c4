#include <stdio.h>

#define NULL ((void *) 0)

struct S {
  int x, y;
};

static struct S s1;
struct S s2;

int *np = 0;
char s[10] = "hi";

void f() {}
void *f2() {
  return 0;
}

int complex_ternary_expr() {
  void *p = NULL;
  return (p == NULL) ? 0 : (p && p);
}

int main() {
  int i;
  struct S *sp = &s1;

  switch (0) {
  case 1:
    break;
  case 2:
    f();
  case 3:
    f();
  }

  if (np == 0) {
  }

  do {
  } while (0);

  for (i = 10; i >= 0; i--) {}

  printf("\"hello\"[0] = '%c'\n", "hello"[0]);

  return 0;
}
