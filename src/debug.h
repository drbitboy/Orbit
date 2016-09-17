/* debug.h */

#ifndef _DEBUG_H_
#define _DEBUG_H_
#ifndef DEBUG
#define DPR(A)
#define DPR1(A)
#else
#include <stdio.h>
#define DPR(A) fprintf A, fflush(stderr)
#define DPR1(A) DPR( (stderr,A))
#endif
#endif /* #ifndef _DEBUG_H_ */

/* end debug.h */
