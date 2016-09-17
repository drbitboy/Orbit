#include <stdio.h>
#include <string.h>
#include <malloc.h>

#include "orbit_stats.h"
#include "debug.h"

#define MAXLINELEN 1023
#define TRUNCATCHAR(STR,CHR) if ( (cPtr=strchr( STR, CHR))) *cPtr = '\0'
#define FIXNEWLINE TRUNCATCHAR( inpLine, '\n')

/*******************************************************/
/* write per-plate statistics from an open file stream
 * ctlStat is the controlling statistic e.g. min incidence angle
 * statVals is [nFace][nStats]
 */

void 
orbit_writeStatFile( FILE *fOut, long nFace, int ctlStat, double *statVals) {
long iFace, iStat;

  /* first line:  size of array and controllin statistic */

  fprintf( fOut
  , "%ld %ld %s  ! # of plates (lines),  # of statistics, control statistic\n"
  , nFace, (long) STAT_COUNT, stats[ctlStat]._name);

  /* second line:  statistic names */

  for ( iStat=0; iStat<STAT_COUNT; ++iStat) {
    fprintf( fOut, "%s%s", iStat ? " " : "", stats[iStat]._name);
  }
  fprintf( fOut, "\n");

  for ( iFace=0; iFace<nFace; ++iFace) {
    for ( iStat=0; iStat<STAT_COUNT; ++iStat) {
      fprintf( fOut, " %15.8lg", *(statVals++));
    }
    fprintf( fOut, "\n");
  }
  return;
}


/******************************************************/
/* read per-plate statistics from an open file stream 
 * - allocate space for them
 */

int 
orbit_readStatFile( FILE *fInp, long *nFacePtr, double **statValsPtr) {
char inpLine[MAXLINELEN];
char fmt[MAXLINELEN];
long i;
long iFace;
long nStats;                                          /* number of statistics */
long iStat;
long nVals;                                                 /* nStats * nFace */
char ctlStatName[30];                        /* name of controlling statistic */
int iCtlStat;                                   /* # of controlling statistic */
char statNames[STAT_COUNT+1][30];                      /* names of statistics */
int iStats[STAT_COUNT+1];                             /* _types of statistics */
double *statVals;
double *dP[STAT_COUNT+1];
double dumyDbl;
#define NUMFREES 20
void *toFree[NUMFREES];
int iFree;
int iStatValFree;                               /* skip freeing of this value */
int nFree = 0;
char *cPtr;

  for ( iFree=0; iFree<NUMFREES; ++iFree) toFree[iFree] = (void *) NULL;

# define DOMALLOC( PTR, TYP, COUNT) \
  if ( ! (toFree[nFree]=(void *) malloc( COUNT * sizeof(TYP))) ) { \
    RTN( "error in malloc", -1); \
  } \
  PTR = (TYP *) toFree[nFree++]

#define RTN( MSG, RTNVAL) \
  if ( MSG) { \
    fprintf( stderr, "*** orbit_readStatFile:  %s\n", MSG ? MSG : "<null>"); \
    fflush( stderr); \
  } \
  for ( iFree=0; iFree<NUMFREES; ++iFree) \
    if ( toFree[iFree]) free( toFree[iFree]); \
  return RTNVAL

# define GET1LINE(A) \
  if ( !fgets( inpLine, MAXLINELEN, fInp)) { \
    RTN( A, -1); \
  } \
  FIXNEWLINE

  GET1LINE( "empty file");

  TRUNCATCHAR( inpLine, '!');

  /* first line:  #plates, #stats, typeOfMainStat */

  if ( 3 != sscanf( inpLine, "%ld %ld %s %s"
                  , nFacePtr, &nStats, ctlStatName, ctlStatName)){
    RTN( "bad header line", -1);
  }

  if ( nStats < 1 || nStats > STAT_COUNT) {
    RTN( "bad header line or old code", -1);
  }

  if ( -1 == (iCtlStat=frm2_getArgInt( stats, ctlStatName)) ) {
    RTN( "bad control stat", -1);
  }

  /* second line:  the statistic names */

  GET1LINE( "bad 2nd line (statistic names)");
  TRUNCATCHAR( inpLine, '!');

  cPtr = inpLine;
  i = 0;

  *statNames[i] = '\0';
  while ( 1==sscanf( cPtr, "%s", statNames[i])) {
  char *cP2;
    cP2 = strstr( cPtr, statNames[i]);
    if ( *statNames[i]) {
      if ( i==nStats) {
        RTN( "too many statistic names in 2nd line", -1);
      } else if ( -1 == (iStats[i]=frm2_getArgInt(stats,statNames[i])) ) {
        RTN( "bad statistic name in 2nd line", -1);
      }
    } 
    if ( !cP2 && i<nStats) {
      RTN( "program error interpreting statistic names", -1);
    } else if ( cP2) {
      cPtr = cP2 + strlen( statNames[i]);
    }
    if ( ++i > nStats) break;
    *statNames[i] = '\0';
  }

  if ( i != nStats) {
    RTN( "too many or too few statistics in 2nd line", -1);
  }

  /* allocate statistic array (STAT_COUNT * nFace);
   * init array to null values
   * init pointers & format string for statistics to first plate
   */

  nVals = STAT_COUNT * (*nFacePtr);
  iStatValFree = nFree;                                 /* save toFree[nFree] */
  DOMALLOC( statVals, double, nVals);

  *statValsPtr = statVals;
  for ( i=0; i<nVals; ++i) *(statVals++) = FRMNULLVAL;
  statVals = *statValsPtr;

  *fmt = '\0';
  for ( i=0; i<=nStats; ++i) {                                /* do one extra */
    dP[i] = statVals + iStats[i];
    strcat( fmt, "%lf");
  }
  dP[nStats] = &dumyDbl;                               /* reset extra pointer */

  /* read 1 line per face, put values into appropriate location */

  for ( iFace=0; iFace<(*nFacePtr); ++iFace) {
    GET1LINE( "too few lines");
    /* ***N.B.  the number of dP's in the next statement must be > STAT_COUNT */
    if ( nStats != sscanf( inpLine, fmt
                         , dP[ 0], dP[ 1], dP[ 2], dP[ 3], dP[ 4]
                         , dP[ 5], dP[ 6], dP[ 7], dP[ 8], dP[ 9]
                         , dP[10], dP[11], dP[12], dP[13], dP[14]
                         , dP[15], dP[16], dP[17], dP[18], dP[19]
                         , dP[20], dP[21], dP[22], dP[23], dP[24]
                         , dP[25], dP[26], dP[27], dP[28], dP[29]
                         ) ) {
    char dumy[MAXLINELEN];
      sprintf( dumy, "error reading statistics for plate %ld", iFace+1);
      RTN( dumy, -1);
    }
    for ( i=0; i<nStats; ++i) dP[i] += STAT_COUNT;
  }

  toFree[iStatValFree] = (void *) 0;            /* don't let RTN free statVal */
  RTN( (char *) NULL, iCtlStat);                                   /* SUCCESS */

} /* orbit_readStatFile( FILE *fInp, long *nFacePtr, double **statValsPtr) */

/********************************/
/* do the above from a filename
 * - write
 */
void 
orbit_writeStatFileName( char *filename, long nFace, int ctlStat
                       , double *statVals) {
FILE *f = fopen( filename, "w");

  if ( !f) {
    fprintf( stderr, "orbit_writeStatFileName:  can't write '%s'\n", filename);
    fflush( stderr);
    return;
  }

  orbit_writeStatFile( f, nFace, ctlStat, statVals);

  fclose( f);

  return;
}

/**********************************/
/* - read
 */
int 
orbit_readStatFileName( char *filename, long *nFacePtr, double **statValsPtr) {
FILE *f = fopen( filename, "r");
int rtnVal;

  if ( !f) {
    fprintf( stderr, "orbit_readStatFileName:  can't read '%s'\n", filename);
    fflush( stderr);
    return -1;
  }

  rtnVal = orbit_readStatFile( f, nFacePtr, statValsPtr);
  fclose( f);
  return rtnVal;
}
