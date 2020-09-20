#ifndef _ASSERT_H_
#define _ASSERT_H_

#ifdef NDEBUG
# define assert(expr) ((void) 0)
#else

extern void __assert_fail(
    const char *s, const char *file, int line, const char *func);

# define assert(expr) \
    (expr) ? \
    ((void) 0) : \
    __assert_fail(#expr, __FILE__, __LINE__, (const char *) 0)

#endif

#endif
