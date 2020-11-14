#include <stddef.h>

extern int printf(char *s, ...);

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

union U1 {
  int a;
  char b;
} u1 = {0xDEADBEEF};

// TODO: fix this case
//
// union U2 {
//   char b;
//   int a;
// } u2 = {0x7E};

int main() {
  printf("u1.a = 0x%X\n", u1.a);
  printf("u1.b = 0x%X\n", u1.b & 0xFF);
  // printf("u2.a = 0x%x\n", u2.a);
  // printf("u2.b = 0x%x\n", u2.b & 0xFF);
  return 0;
}
