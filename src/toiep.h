/* toiep.h */

#include <stdio.h>
#include "orbit3d.h"
#include "spudshap.h"

/* macro to read next frame from frames file or other kind of file
   0 for all items read
  -1 for end of file, nothing read
   >0 read error - current istate | errorBIT
   xyzBIT for error reading a line of type xyz e.g. a second line of that type
 */
#define TIMBIT 1L
#define SCBIT (TIMBIT<<1)
#define SUNBIT (SCBIT<<1)
#define P5BIT (SUNBIT<<1)
#define CPCOUNTBIT (P5BIT<<1)
#define CPSTARTBIT (CPCOUNTBIT<<1)
#define CPSTOPBIT (CPSTARTBIT<<1)
#define ALLBITS (TIMBIT|SCBIT|SUNBIT|P5BIT|CPCOUNTBIT|CPSTARTBIT|CPSTOPBIT)
#define OBSPARMBIT (CPSTOPBIT<<1)
#define INCOMPLETE (OBSPARMBIT<<1)
#define NOT3 (INCOMPLETE<<1)
#define NOT1 (NOT3<<1)
#define CAMPTS (NOT1<<1)
#define DOUBLELINE (CAMPTS<<1)
#define READERR (DOUBLELINE<<1)

#define TESTBIT(B) if (istate & B) return(DOUBLELINE | istate)
#define SETBIT(B) istate |= B

#define READVEC( C, V, B) \
  TESTBIT(B); \
  if ( sscanf( C, "%lf %lf %lf", V, V+1, V+2) != 3) return(NOT3 | istate); \
  SETBIT(B)

#define BADVAL -1e300

typedef long (*TOIEPREADNEXT)( FILE *f, char *timch, VEC vsc, VEC vsun, VEC p5
                             , long *fovcount, VEC campts[], char *metch
                             , double *obsParm, MTX cam2Abf, double *miscVals);

TOIEPREADNEXT toIepGetReadnextFn( int argc, char **argv);

void toIep_SetSpudv( SPUDV *);

typedef TOIEPREADNEXT (*TOIEPINIT)( int argc, char **argv);

/***************
 * macros for each *2iep_{init,readnext}() functions
 *
 * TOIEPINITDECLARE(INITFN, TAG, TAGEXP) *2iep_init fn name declaration + tags
 * TOIEPLISTCHECK list its own name + some optional expanded info (TAGEXP)
 * TOIEPREADNEXTDECLARE(READNEXTFN) *2iep_readnext function name declaration
 */

/******************************************
 * *2iep_readnext function name declaration
 * - must be followed by an open-curly-brace to start the function
 * - does not needs a semicolon suffix
 */
#define TOIEPREADNEXTDECLARE( READNEXTFN) \
long \
READNEXTFN( FILE *f, char *timch, VEC vsc, VEC vsun, VEC p5 \
                 , long *fovcount, VEC campts[], char *metch \
                 , double *obsParm)

/********************************************
 * *2iep_init function name declaration + tag
 * - needs a semicolon suffix
 */
#define TOIEPINITDECLARE( INITFN, TAG, EXPANDEDINFO ) \
TOIEPREADNEXT \
INITFN( int argc, char **argv) { \
static char myTag[] = { TAG }; \
static char myTagExpanded[] = { EXPANDEDINFO }

/*************************************************
 * list its own name + some optional expanded info
 * - needs a semicolon suffix
 */
#define TOIEPLISTCHECK \
  if ( !strcmp(*argv,"list")) { \
    fprintf( stderr, "%s\n", myTag); \
    return 0; \
  } \
  if ( !strcmp(*argv,"listfull")) { \
    fprintf( stderr, "%s%s\n\n", myTag, myTagExpanded ? myTagExpanded : ""); \
    return 0; \
  } \
  if ( strcmp(*argv,myTag) ) return 0

/* end toiep.h */
