#include <stdio.h>

char *s =
#if 0
#if 1
"fail"
#elif 1
"fail"
#else
"fail"
#endif
#else
"pass"
#endif
;

int main() {
  fprintf(stdout, "%s\n", s);
  return 0;
}
