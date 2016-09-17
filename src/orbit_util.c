#include <stdio.h>
#include <string.h>
#include <math.h>
#include "debug.h"
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <errno.h>
#include <libgen.h>

#ifdef vms
char *malloc();
char *realloc();
#else
#include <malloc.h>
#endif

#include "orbit_util.h"

/*****************************************************************************/
/* return dynamically allocated strings w/current hostname, time & user info */

void
orbit_utilGetHostCtimeGecos( char **hostnm, char **ct, char **gecos) {
struct utsname buf;
int unameRtn;

time_t t;

struct passwd *pwd;

char *cPtr;

  unameRtn = uname( &buf);                                        /* hostname */
  *hostnm = strdup( !unameRtn ? buf.nodename : "UnknownHost");
  cPtr = strchr( *hostnm, '\n');
  if ( cPtr) *cPtr = '\0';

  t = time( (time_t *) NULL);                                 /* current time */
  *ct = strdup( ctime( &t));
  cPtr = strchr( *ct, '\n');
  if ( cPtr) *cPtr = '\0';

  pwd = getpwuid( getuid());                                     /* user info */
  *gecos = strdup( pwd ? pwd->pw_gecos : "GECOSFailed");
  cPtr = strchr( *gecos, '\n');
  if ( cPtr) *cPtr = '\0';

  return;
}

#define WGT(PTR) ( ((xidx-idx0)*PTR[2*idx1]) + ((idx1-xidx)*PTR[2*idx0]) )

/*****************************************************************************/
/* evaluates two limiting functions
 */
long
orbit_evalLimits( long *limVals, long delT, long t, long *retVals) {
double xidx = ((double) t) / delT;
long idx0 = floor( xidx);
long idx1 = idx0+1;
long *hiVal = limVals+1;
long *loVal = limVals+2;

  if ( idx0 < 0 || idx1 >= *limVals) return -1;

  retVals[0] = (long) floor( WGT(hiVal));
  retVals[1] = (long) ceil( WGT(loVal));

  return 0;
}

/*****************************************************/
/* analyze which (separator) characters are in a string
 * - returns SEP_* of unique separator i.e. type of single non-ws char
 *   present (excluding ws), else -1
 * - set corresponding bit & increment counter
 * - also check for quotes so this routine can be used to test strings that
 *   are not separators
 */

long
orbit_analyzeSeparator( char *sep, long sepTotals[SEP_COUNT], long *bits) {
long i;

  *bits = 0;

  for ( i=0; i<SEP_COUNT; ++i) sepTotals[i] = 0;

  while ( *sep) {
    switch ( *sep) {

#   define SEP_CASE(C,BITPOS) \
    case C: sepTotals[BITPOS]++;  *bits |= SEPBIT(BITPOS); break;

    SEP_CASE( ',', SEP_COMMAS);
    SEP_CASE( '"', SEP_QUOTES);
    SEP_CASE( '(', SEP_OPENS);
    SEP_CASE( ')', SEP_CLOSES);
    SEP_CASE( ' ':
        case '\t':
        case '\r':
        case '\n', SEP_WS);

    SEP_CASE( '\0':
           default, SEP_OTHER);           /* use '\0' as a dummy for default */

    } /* switch *sep */
    sep++;
  }
  for ( i=0; i<SEP_COUNT; ++i) {
    if ( i != SEP_WS) if ( SEPBIT(i) == ((*bits)&(~SEPBIT(SEP_WS))) ) {
      return i;
      break;
    }
  }
  return -1;
}

/*****************************************************************************/
/* returns non-0 if one point of a given scan is outside two limiting functions
 */
long
orbit_ptOutsideLimits( long *limVals, long delT, long t, long val) {
long eVals[2];

  if ( orbit_evalLimits( limVals, delT, t, eVals) == -1 ) return -1;

  if ( val > eVals[0]) return 1;
  if ( val < eVals[1]) return 1;

  return 0;
}

/*************************************************************************/
/* see how many scans of a given size fit in between two limiting functions
 */
long
orbit_fitScans( long *limVals, long delT, long startTime
              , long val, long scanLen) {
long nScans = 0;

  while ( !orbit_ptOutsideLimits( limVals, delT, startTime, val) ) {
    nScans++;
    startTime += scanLen;
  }
  return nScans;
}

/*************************************************************************/
/* fit the best scan in between two limiting functions
 * returns the value that ends the latest
 *
 * m & b, along with iVal, define the scan length: (m*iVal + b) e.g. m is the 
 * number of observation of length iVal, and b is the per-scan total of 
 * seconds between observations & seconds between scans
 */
long
orbit_bestFitScan( long *limVals, long delT, long startTime, long m, long b) {
long bestVal;
long bestEndTime = startTime;
long endTime;
long iVal, scanLen;
long eVals[2];

  if ( orbit_evalLimits( limVals, delT, startTime, eVals) == -1 ) return -1;

  for ( bestVal=iVal=eVals[0]; iVal >= eVals[1]; --iVal) {
    scanLen = m * iVal + b;
    endTime = startTime + scanLen * orbit_fitScans( limVals, delT, startTime
                                                  , iVal, scanLen);
    if ( endTime > bestEndTime) {
      bestVal = iVal;
      bestEndTime = endTime;
    }
  }
  return bestVal;
}

/****************************************************/
/* read delta time from sting in one of these formats:
 *   [+-]\HH:MM:SS\      hours, minutes, seconds
 *   [+-]\SS\            seconds
 *   [+-]SS              seconds
 * on success, return total seconds
 * on error, return ORBIT_BAD_DTIME (typically -100 hours i.e. -360000 s)
 */
long
orbit_readDtime( char *intok) {
int nread, i;
long l[3];
long sign;
char c[4][10];

  if ( *intok == '-') {
    sign = -1L;
    intok++;
  } else {
    sign = +1L;
    if ( *intok == '+') intok++;
  }

  /* read "\SS\" */

  if ( 3 == (nread=sscanf( intok, "%1[\\]%ld%1[\\]", c[0], l, c[1])) ) {
    return sign * l[0];
  }

  /* read "\HH:MM:SS\" */
  if ( 7 == (nread=sscanf( intok, "%1[\\]%ld%1[:]%ld%1[:]%ld%1[\\]"
                         , c[0], l, c[1], l+1, c[2], l+2, c[3])) ) {
    if ( l[0] < 0) return ORBIT_BAD_DTIME;
    if ( l[1] < 0  || l[1] > 59) return ORBIT_BAD_DTIME;
    if ( l[2] < 0  || l[2] > 59) return ORBIT_BAD_DTIME;

    DPR(( stderr, "%ldh %ldm %lds\n", l[0], l[1], l[2]));

    return sign * (l[2] + (60L * (l[1] + (60 * l[0]))));
  }

  /* read "SS" */
  if ( 1 == (nread=sscanf(intok,"%ld",l)) ) {
    return sign * l[0];
  }

  return ORBIT_BAD_DTIME;
}

/****************************/
/* parse string into tokens between commas, return number of tokens,
 * put pointer at start of each token into char *ptr[], 
 * convert commas to nulls (i.e. string terminators)
 */
int orbit_commaParse( char *inl, char *ptr[]) {
char tmpc[255], lcl1024[1024], *p;
char *lcl;
int ntok, j, commafound;
static char nullStr[1];           /* this should not be necessary:  = { "" }; */

  if ( *nullStr) *nullStr = '\0';            /* but here is belt & suspenders */

  ptr[0] = nullStr;/* ensure first token is null string if no tokens returned */

  if ( p=strchr( inl,'\n')) *p = '\0';
  if ( strlen(inl) < 1024) {
    strcpy( lcl=lcl1024, inl);
  } else {
    lcl = strdup( inl);
  }
# ifdef RTN
# undef RTN
# endif
# define RTN(RETVAL) if ( lcl && lcl!=lcl1024) free( lcl); return RETVAL

  if ( p=strchr( inl,';')) *p = '\0';

  for ( (p=inl), (commafound=1), (ntok=0); *p && commafound < 2; ) {
  char *lp;
    if ( *p == ',') {
      commafound++;
      *p = '\0';
      ++p;
    } else if ( *p == '\t' || *p == ' ') {
      ++p;
    } else {
      j = sscanf( p, "%254[^,]", tmpc);
      if ( j != 1) {
        commafound = 3;
      } else {
        ptr[ntok] = p;
        p += strlen( tmpc);
        lp = p - 1;
        while ( (*lp==' ' || *lp=='\t') && lp > ptr[ntok]) *(lp--) = '\0';
        ntok++;
        commafound = 0;
      }
    }
  }
  if ( commafound > 1 || (ntok && commafound)) {
    fprintf( stderr
         , "ORBIT_COMMAPARSE:  Error in >%s< at or before >%s<, position=%ld\n"
         , lcl ? lcl : "<unknown-strdup error>"
         , lcl ? lcl+(p-inl) : "<unknown>"
         , (long) (p-inl));
    RTN( -1);
  }
  RTN( ntok);
}

/**********************************************************************/
/* how to read one line at a time from files that have an include 
 * feature i.e. read "SPK: a.bsp", then "TK: nearcam-93.tpc", then "#comment"
 *  from the following two files x.x & ../y/y.y:
 *
 * Contents of file x.x:
 *  
 *  SPK: a.bsp
 *  INCLUDE: ../y/y.y
 *  #comment
 *
 * Contents of file ../y/y.y:
 *
 *  TK: nearcam-93.tpc
 *
 */

typedef struct {
  char *_prevWD;
  FILE *_f;
  void *_prev;
} READFILE;

/**********************************************************************/
/* return malloc'ed string of current working directory
 * - return (char *) 0 on failure (getcwd or malloc)
 * - cd to newWD if no failure && newWD is non-null && newWD is non-empty
 *   - set newWD[0] to \0 if cd fails
 */

#define FREE(A) if (A) free( (char *) A)

#ifdef ERRRTN
#undef ERRRTN
#endif
#define ERRRTN return (char *) 0
#define INITIAL_BUF_SIZE 256

char *
orbit_utilGetCWD( char *newWD) {
int sz = INITIAL_BUF_SIZE;
char *buf = malloc( sz);
char *retval;
int savErrno = errno;

  if ( !buf) ERRRTN;                                 /* initial malloc failed */

  while( !getcwd( buf, sz) ) {                        /* !getcwd() => failure */
    free( buf);
    if ( errno != ERANGE) ERRRTN;     /* return if cause is not buf too small */
    if ( !(buf = malloc(sz *= 2))) ERRRTN; /* increase buf, return on failure */
    errno = savErrno;
  }
  if ( retval = strdup( buf)) free( buf);    /* allocate new string, free buf */
  else retval = buf;                            /* if allocate fails, use buf */

  if (newWD) if (*newWD) {
  int result = chdir(newWD);                /* cd to newWD if non-null/-empty */
    if ( result) *newWD = '\0';              /* - modify newWD[0] if cd fails */
    errno = savErrno;
  }
  return retval;
}

/**********************************************************************/
/* return malloc'ed string of absolute path name
 * - return (char *) 0 on failure
 */

#ifdef CLEANUP
#undef CLEANUP
#endif

#define CLEANUP { \
  FREE(dbuf); FREE(lclFN); FREE(fbuf); FREE(buf); \
  if ( savWD) { chdir( savWD); free(savWD); } \
}

#ifdef ERRRTN
#undef ERRRTN
#endif

#define ERRRTN { retval = (char *) 0; RTN }

#ifdef RTN
#undef RTN
#endif

#define RTN { \
  CLEANUP \
  return retval; \
}

/**********************************************************************/

char *
getAbsolutePath( char *filnam) {
char *savWD = (char *) 0;
char *lclFN = (char *) 0;
char *cPtr;
char *dbuf = (char *) 0;
char *fbuf = (char *) 0;
char *buf = (char *) 0;
char *retval = (char *) 0;
int bufsiz = INITIAL_BUF_SIZE;
int rlsiz;

  if ( !filnam) return (char *) 0;

# define IFERR(LVAL,FUNC)  if ( !(LVAL = FUNC)) ERRRTN


  IFERR( lclFN, strdup(filnam))                   /* local copy for dirname() */
  IFERR( dbuf, strdup(dirname(lclFN)))                  /* dbuf holds dirname */
  IFERR( savWD, orbit_utilGetCWD( dbuf))     /* save current dir, move to new */
  if ( !*dbuf) ERRRTN                                /* could not move to new */
  free( dbuf);
  IFERR( dbuf, orbit_utilGetCWD( (char *) 0))             /* get full new dir */
  strcpy( lclFN, filnam);                      /* in case dirname mungs lclFN */
  IFERR( cPtr, basename(lclFN))
  if ( *cPtr == '/') cPtr++;                   /* crude hack for xpg_basename */
  IFERR( fbuf, strdup( cPtr))                          /* fbuf holds basename */

  IFERR( buf, malloc(bufsiz))

  while ( (rlsiz=readlink(fbuf, buf, bufsiz)) > 0 ) {  /* fbuf is symlink ... */
    if ( rlsiz < bufsiz) {                       /* - symlink path fit in buf */
      buf[rlsiz] = '\0';                           /* - - terminate with null */
      retval = getAbsolutePath( buf);                          /* - - recurse */
      RTN                                              /* - - & return retval */
    }
    free(buf); buf = (char *) 0;            /* - symlink path bigger than buf */
    IFERR( buf, malloc(bufsiz *= 2))                 /* - - expand buf & loop */
  }

  if ( rlsiz == -1 && errno == EINVAL ) {            /* fbuf is not a symlink */
  int ld = strlen(dbuf);
  int lf = strlen(fbuf);
    IFERR( retval, malloc(ld+lf+2))           /* allocate for dbuf + fbuf + 2 */
    strcpy( retval, dbuf);                                          /* - dbuf */
    cPtr = retval + ld;
#   ifndef vms                                                 /* - separator */
#   ifdef msdos
    *(cPtr++) = '\\';                                           /* - - msdos? */
#   else
    *(cPtr++) = '/';                                       /* - - Eunuchs(tm) */
#   endif
#   endif
    strcpy( cPtr, fbuf);                                            /* - fbuf */
    RTN
  }

  ERRRTN;            /* readlink (rlsiz==-1) returned errno other than EINVAL */
}

/**********************************************************************/

#ifdef ERRRTN
#undef ERRRTN
#endif
#define ERRRTN \
  if ( lclFN) free( lclFN); \
  if ( rf) { \
    if ( rf->_f) fclose( rf->_f); \
    if ( rf->_prevWD) free( rf->_prevWD); \
    free( rf); \
  } \
  if ( newWD) free( newWD); \
  return (void *) 0

void *
initReadFiles( char *filnam, void *v) {
char *lclFN = (char *) 0;
READFILE *rf = (READFILE *) 0;
char *newWD = (char *) 0;

  if ( !filnam) { ERRRTN; }           /* check for null filnam string pointer */

  if ( !(lclFN = strdup(filnam))) { ERRRTN; }/* make local copy for dirname() */

  /* allocate READFILE structure */
  if ( !(rf = (READFILE *) malloc( sizeof(READFILE)))) {
    ERRRTN;
  }

# define SETC0(MBR) SET0(MBR,char)
# define SET0(MBR,TYP) rf->MBR = (TYP *) 0

  SET0( _f, FILE);
  SETC0( _prevWD);
  SET0( _prev, void);

  if ( !(newWD = strdup(dirname( lclFN)))) { ERRRTN; }   /* get new directory */
  if ( !( rf->_f = fopen( filnam, "r"))) { ERRRTN; }         /* open the file */
  if ( !(rf->_prevWD = orbit_utilGetCWD( newWD))) { ERRRTN; }  /* get/set cwd */
  if ( !*newWD) { ERRRTN; }                             /* was not able to cd */

  rf->_prev = v;                      /* save link back to previous structure */
  free( lclFN);                                                    /* cleanup */
  free( newWD);                                                    /* cleanup */

  return (void *) rf;
}

/**********************************************************************/

void *
closeReadFiles( void *pv) {
READFILE *rf;
void *retval = (void *) 0;
  if ( !pv) return retval;
  rf = (READFILE *) pv;
  if ( rf->_f) fclose( rf->_f);                                 /* close file */
  if ( chdir( rf->_prevWD)) {                               /* cd to prev dir */
    fprintf( stderr
           , "closeReadFiles:  Error in chdir(\"%s\")\n"
           , rf->_prevWD);
  }
  retval = rf->_prev;               /* save previous structure ptr for return */
  free( rf->_prevWD);                                          /* free memory */
  free( rf);
  return retval;
}

/**********************************************************************/

/* fgetsReadFiles():  fgets()-equivalent for multiply included files
 * - call with null pointer s to close current file
 */

#define INCSTR "INCLUDE:"
#define LIS strlen(INCSTR)
#define MAXLENFILNAM 256

char *
fgetsReadFiles( char *s, int size, void **ppv) {
READFILE *rf;
void *rfNew;
char *retval = (char *) 0;
char *cptr;
char includ[LIS*2];
char newfilnam[MAXLENFILNAM];
static char fmt1[20];
static char fmt2[20];

  if ( !*fmt1) {
    sprintf( fmt1, "%%%lds", sizeof(includ) - 1);
    sprintf( fmt2, "%%s %%%lds", sizeof(newfilnam) - 1);
  }

  if ( !ppv) return retval;
  if ( !*ppv) return retval;
  rf = *((READFILE **)ppv);

  /* read a line IF s is not a null pointer
   * ***N.B. if s is null, then retval stays null => close current level
   */

  if ( s)  retval = fgets( s, size, rf->_f);

  /*  - if retval is null, 
   *    - EITHER caller's s is a null pointer
   *    - OR we are at eof of rf->_f
   *    - in either case, we need to go up a level
   */

  if ( !retval ) {

    *ppv = closeReadFiles( *ppv);               /* close current level (*ppv) */

    if ( !s) return s;           /* return if "close current level" requested */

    if ( *ppv) { /* if there's a level one up from where we are, read from it */
      return fgetsReadFiles( s, size, ppv);
    }

    return retval;  /* else rf was top level, so we're done:  return null ptr */
  }

  /* we read (past tense) a line, so ... */

  if ( cptr = strchr(s,'\n') ) *cptr = '\0';               /* - strip newline */

  /* - then return line as is if ... */

  if ( strncmp( s, INCSTR, LIS) )           /* - it doesn't start with INCSTR */
    return retval;
  if ( 1 != sscanf( s, fmt1, includ) )  /* - no tokens in first LIS*2-1 chars */
    return retval;
  if ( strcmp( includ, INCSTR))                /* - first token is not INCSTR */
    return retval;
  if ( 2 != sscanf(s,fmt2,includ,newfilnam))   /* - can't read 2 tokens */
    return retval;
  if ( !(rfNew=initReadFiles(newfilnam,*ppv)) )   /* - new rf fails */
    return retval;

  /* new rf structure created => successful open of & chdir to new file
   * - set *ppv to point to it
   * - and return line read from new file
   */
  *ppv = rfNew;
  return fgetsReadFiles( s, size, ppv);
}
