#ifndef _STDARG_H_
#define _STDARG_H_

struct __va_list_struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    union {
        unsigned int overflow_offset;
        char *overflow_arg_area;
    } overflow;
    char *reg_save_area;
};

typedef struct __va_list_struct va_list;

#define va_start(ap, p)   __builtin_va_start(ap, p)
#define va_arg(ap, tp)    __builtin_va_arg(ap, tp)
#define va_end(ap)        __builtin_va_end(ap)

// C99 defines va_copy
#define va_copy(dst, src) __builtin_va_copy(dst, src)

#endif
