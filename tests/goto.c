#include <stdio.h>

int main() {
  int n = 0;
start:
  if (!n++) {
    printf("n is zero\n");
    goto start;
  }
  printf("n = %d\n", n); // 2

  if (n == 2) {
    goto end;
  } else {
    goto start;
  }

end:
  printf("the end\n");
  return 0;
}
