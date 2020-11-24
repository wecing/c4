#include <stdio.h>

int main() {
  char *s;

  s =
#include "multi-include-optimization-1.h"
#include "multi-include-optimization-1.h"
#include "multi-include-optimization-1.h"
    ;
  printf("%s\n", s);

  s =
#include "multi-include-optimization-2.h"
#include "multi-include-optimization-2.h"
#include "multi-include-optimization-2.h"
    ;
  printf("%s\n", s);
  return 0;
}
