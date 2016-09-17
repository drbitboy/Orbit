/* orbit_util.h 
 * orbit utility routines - NON-X window related 
 */

#ifndef _ORBIT_UTIL_H_
#define _ORBIT_UTIL_H_

static char sepChars[] = { ",\"() ." };

enum {
  SEP_COMMAS=0L
, SEP_QUOTES         /* double quotes i.e. ") */
, SEP_OPENS          /* open parenthesis */
, SEP_CLOSES         /* close " */
, SEP_WS             /* whitespace (space, tab, carriage return, newline) */
, SEP_OTHER          /* anything else */
, SEP_COUNT
};

#define SEPBIT(BITPOS) (1L<<(BITPOS))

long orbit_analyzeSeparator( char *str, long septotals[SEP_COUNT], long *bits);

int orbit_commaparse( char inl[], char *ptr[]);
long orbit_readDtime( char *inTok);

long orbit_evalLimits( long *limVals, long delT, long t, long *retVals);
long orbit_ptOutsideLimits( long *limVals, long delT, long t, long val);

long orbit_fitScans( long *limVals, long delT
                   , long startTime , long val, long scanLen);

long orbit_bestFitScan( long *limVals, long delT
                      , long startTime, long m, long b);

void orbit_utilGetHostCtimeGecos( char **hostnm, char **ct, char **gecos);

char *orbit_utilGetCWD( char *newWD);
char *getAbsolutePath( char *filnam);
void *initReadFiles( char *filnam, void *v);
void *closeReadFiles( void *pv);
char *fgetsReadFiles( char *s, int size, void **ppv);

#define ORBIT_BAD_DTIME (-3600L*100)

#endif /* _ORBIT_UTIL_H_ */
