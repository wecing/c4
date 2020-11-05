extern int printf(const char *s, ...);

typedef union U {
  long x;
  float y;
} U;

U f1(int n) {
  U u;
  u.x = n;
  return u;
}

typedef struct S {
  long x;
  float y;
  long z;
} S;

S f2(int n) {
  S s;
  s.x = n;
  return s;
}

int main() {
  if (1) {
    U u;
    u = f1(42);
    printf("u.x = %ld\n", u.x);
  }
  if (1) {
    S s;
    s = f2(99);
    printf("s.x = %ld\n", s.x);
  }
  return 0;
}
