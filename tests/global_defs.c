extern int printf(const char *s, ...);

extern const char *const names[];

const char *const names[] = {
  "aaa", "bbb", "ccc", "ddd",
};

int n;

int f() {
  return n;
}

int n = 5;
int n;

int main() {
  printf("n = %d\n", n); // 5
  printf("f() = %d\n", f()); // 5
  return 0;
}
