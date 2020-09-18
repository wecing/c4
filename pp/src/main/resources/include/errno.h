#ifndef _ERRNO_H_
#define _ERRNO_H_

#define EDOM            33      /* Math argument out of domain of func */
#define ERANGE          34      /* Math result not representable */

extern int *__errno_location(void);
#define errno (*__errno_location())

#endif
