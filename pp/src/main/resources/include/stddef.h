#ifndef _STDDEF_H_
#define _STDDEF_H_

typedef long ptrdiff_t;
typedef unsigned long size_t;
typedef short wchar_t; // this is int on linux x64

typedef long __off_t;
typedef long __off64_t;

#define NULL ((void *) 0)
#define offsetof(TYPE, MEMBER) \
    ((size_t) ((void *) &(((TYPE *) NULL)->MEMBER) - NULL))

#endif
