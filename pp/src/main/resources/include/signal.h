#ifndef _SIGNAL_H_
#define _SIGNAL_H_

typedef int sig_atomic_t;

typedef void (*__sighandler_t) (int);

#define SIG_ERR ((__sighandler_t) -1)
#define SIG_DFL ((__sighandler_t) 0)
#define SIG_IGN ((__sighandler_t) 1)

#define SIGINT    2
#define SIGILL    4
#define SIGABRT   6
#define SIGFPE    8
#define SIGSEGV   11
#define SIGTERM   15

#define SIGHUP    1
#define SIGQUIT   3
#define SIGTRAP   5
#define SIGKILL   9
#define SIGBUS    10
#define SIGSYS    12
#define SIGPIPE   13
#define SIGALRM   14

#define SIGURG    16
#define SIGSTOP   17
#define SIGTSTP   18
#define SIGCONT   19
#define SIGCHLD   20
#define SIGTTIN   21
#define SIGTTOU   22
#define SIGPOLL   23
#define SIGXCPU   24
#define SIGXFSZ   25
#define SIGVTALRM 26
#define SIGPROF   27
#define SIGUSR1   30
#define SIGUSR2   31

#define SIGWINCH  28

#define SIGIO     SIGPOLL
#define SIGIOT    SIGABRT
#define SIGCLD    SIGCHLD

extern __sighandler_t signal(int sig, __sighandler_t handler);
extern int raise(int sig);

#endif
