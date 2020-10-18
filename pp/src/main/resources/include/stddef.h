#ifndef _STDDEF_H_
#define _STDDEF_H_

typedef long ptrdiff_t;
typedef unsigned long size_t;
typedef short wchar_t; // this is int on linux x64

#define NULL ((void *) 0)
#define offsetof(TYPE, MEMBER) __builtin_offsetof(TYPE, MEMBER)

#endif
