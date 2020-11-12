extern int printf(char *s, ...);

int main() {
  char *s = "你好";
  char *s2 = "\xEF\xBB\xBF";
  printf("%s means hello\n", s);
  printf("utf-8 header is ");
  while (*s2) {
    printf("\\x%X", *s2++ & 0xff);
  }
  printf("\n");
  return 0;
}
