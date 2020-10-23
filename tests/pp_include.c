extern int printf(const char *s, ...);

#include "pp_include_x.h"

#define INCLUDE_Y "pp_include_y.h"
#include INCLUDE_Y

#define INCLUDE_Z "pp_include_z.h"
#define concat(a,b) a ## b
#include concat(INCLUDE, _Z)

// P is not defined.
#ifdef P
#include P
#endif

int main() {
  printf("X = %d\n", X);
  printf("Y = %d\n", Y);
  printf("Z = %d\n", Z);
  return 0;
}
