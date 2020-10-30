int main() {
  if (1) {
    struct S {
      struct S *prev;
    };

    struct S s;
    s.prev = &s;
    s.prev->prev = &s;
  }

  if (1) {
    typedef struct S T;
    struct S { int x; };
    if (1) {
      T t;
    }
  }

  if (1) {
    struct S;
    struct P { struct S *sp; } p;
    struct S { int n; } s;
    p.sp = &s;
    p.sp->n;
  }
  return 0;
}
