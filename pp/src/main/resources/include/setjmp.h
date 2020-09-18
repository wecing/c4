#ifndef _SETJMP_H
#define _SETJMP_H

typedef long int __jmp_buf[8];
typedef struct
{
  unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
} __sigset_t;
struct __jmp_buf_tag
  {
    __jmp_buf __jmpbuf;
    int __mask_was_saved;
    __sigset_t __saved_mask;
  };
typedef struct __jmp_buf_tag jmp_buf[1];

extern int setjmp(jmp_buf env);
extern void longjmp(jmp_buf env, int val);

#endif
