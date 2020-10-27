struct S {
  int x, y;
};

static struct S s1;
struct S s2;

int *np = 0;
char s[10] = "hi";

void f() {}

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

  return 0;
}
