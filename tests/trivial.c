struct S {
  int x, y;
};

static struct S s1;
struct S s2;

int *np = 0;

void f() {}

int main() {

  switch (0) {
  case 1:
    break;
  case 2:
    f();
  case 3:
    f();
  }

  do {
  } while (0);

  return 0;
}
