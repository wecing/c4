#ifndef _STDIO_H_
#define _STDIO_H_

#include <stddef.h>

// for __USE_POSIX, _POSIX_C_SOURCE
#include <features.h>

// for __off_t and __off64_t
#include <bits/types.h>

struct _IO_FILE;
struct _IO_marker;
struct _IO_codecvt;
struct _IO_wide_data;
typedef struct _IO_FILE FILE;
typedef void _IO_lock_t;
struct _IO_FILE {
  int _flags;
  char *_IO_read_ptr;
  char *_IO_read_end;
  char *_IO_read_base;
  char *_IO_write_base;
  char *_IO_write_ptr;
  char *_IO_write_end;
  char *_IO_buf_base;
  char *_IO_buf_end;
  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;
  struct _IO_marker *_markers;
  struct _IO_FILE *_chain;
  int _fileno;
  int _flags2;
  __off_t _old_offset;
  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];
  _IO_lock_t *_lock;
  __off64_t _offset;
  struct _IO_codecvt *_codecvt;
  struct _IO_wide_data *_wide_data;
  struct _IO_FILE *_freeres_list;
  void *_freeres_buf;
  size_t __pad5;
  int _mode;
  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];
};

typedef struct {
  int __count;
  union {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
typedef struct _G_fpos_t {
  __off_t __pos;
  __mbstate_t __state;
} __fpos_t;
typedef __fpos_t fpos_t;

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2
#define BUFSIZ 8192
#define EOF -1

#define FOPEN_MAX 16
#define FILENAME_MAX 4096
#define L_tmpnam 20
#define TMP_MAX 238328

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

extern int remove(const char *filename);
extern int rename(const char *old, const char *new);
extern FILE *tmpfile(void);
extern char *tmpnam(char *s);
extern int fclose(FILE *stream);
extern int fflush(FILE *stream);
extern FILE *fopen(const char *filename, const char *mode);
extern FILE *freopen(const char *filename, const char *mode, FILE *stream);
extern void setbuf(FILE *stream, char *buf);
extern int setvbuf(FILE *stream, char *buf, int mode, size_t size);
extern int fprintf(FILE *stream, const char *format, ...);
extern int fscanf(FILE *stream, const char *format, ...);
extern int printf(const char *format, ...);
extern int scanf(const char *format, ...);
extern int sprintf(char *s, const char *format, ...);
extern int sscanf(const char *s, const char *format, ...);
// TODO: struct arg passing not yet implemented using correct ABI
// extern int vfprintf(FILE *stream, const char *format, va_list arg);
// extern int vprintf(const char *format, va_list arg);
// extern int vsprintf(char *s, const char *format, va_list arg);
extern int fgetc(FILE *stream);
extern char *fgets(char *s, int n, FILE *stream);
extern int fputc(int c, FILE *stream);
extern int fputs(const char *s, FILE *stream);
extern int getc(FILE *stream);
extern int getchar(void);
extern char *gets(char *s);
extern int putc(int c, FILE *stream);
extern int putchar(int c);
extern int puts(const char *s);
extern int ungetc(int c, FILE *stream);
extern size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
extern int fgetpos(FILE *stream, fpos_t *pos);
extern int fseek(FILE *stream, long int offset, int whence);
extern int fsetpos(FILE *stream, const fpos_t *pos);
extern long int ftell(FILE *stream);
extern void rewind(FILE *stream);
extern void clearerr(FILE *stream);
extern int feof(FILE *stream);
extern int ferror(FILE *stream);
extern void perror(const char *s);

#ifdef __USE_POSIX
extern int fileno(FILE *stream);
extern FILE *fdopen(int fd, const char *mode);
#endif

#if _POSIX_C_SOURCE >= 2
extern FILE *popen(const char *command, const char *type);
extern int pclose(FILE *stream);
#endif

#endif
