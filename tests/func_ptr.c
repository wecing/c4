extern int printf(const char *s, ...);

void f() {
  printf("f\n");
}

typedef void (*FP)(void);

int main() {
  FP fp = f;
  fp();

  fp = f;
  fp();
  fp = &f;
  fp();

  (*fp)();

  return 0;
}
