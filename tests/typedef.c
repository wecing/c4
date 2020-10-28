#include <stdio.h>

int main() {
  if (1) {
    typedef int S;

    struct S {
      int x;
      int y;
    };

    printf("S: %lu\n", sizeof(S));
    printf("struct S: %lu\n", sizeof(struct S));
  }

  if (1) {
    typedef struct S2 S2;

    struct S2 {
      int x;
      int y;
    };

    S2 v1;
    printf("S2: %lu\n", sizeof(S2));
    printf("v1: %lu\n", sizeof(v1));

    {
      S2 v2;
      printf("v2: %lu\n", sizeof(v2));
    }

    printf("struct S2: %lu\n", sizeof(struct S2));
  }

  if (1) {
    typedef union U U;

    typedef union {
      U *up;
    } Value;

    union U {
      int h;
    };

    Value v;
    U u;

    u.h = 42;
    v.up = &u;

    printf("v.up->h = %d\n", v.up->h);
  }
  return 0;
}
