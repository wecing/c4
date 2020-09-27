extern int printf(const char *s, ...);

void f() {
  printf("f\n");
}

static void f2(void);

typedef void (*FP)(void);

int main() {
  FP fp = f;
  fp();

  fp = f;
  fp();
  fp = &f;
  fp();

  (*fp)();

  fp = f2;
  fp();

  return 0;
}

static void f2() {
  printf("f2\n");
}
