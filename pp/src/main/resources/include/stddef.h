#ifndef _STDDEF_H_
#define _STDDEF_H_

typedef long ptrdiff_t;
typedef unsigned long size_t;
typedef short wchar_t; // this is int on linux x64

// TODO: offsetof does not work correctly because we do not do constant folding
//       for member access exprs. we probably need to implement a builtin here.
#define NULL ((void *) 0)
#define offsetof(TYPE, MEMBER) \
    ((size_t) ((void *) &(((TYPE *) NULL)->MEMBER) - NULL))

#endif
