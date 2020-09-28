#include <stdio.h>

#include <fcntl.h>
#include <sys/types.h>
#include <utime.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/times.h>

int main() {
#ifdef __USE_POSIX
  printf("posix\n");
#else
  printf("no posix\n");
#endif
  return 0;
}
