/* orbdb.c - orbit dbm routines
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "orbdb.h"

#define FPMETERRSFX( MSG) \
        FP1(MSG); \
        FPERRFULL( (stderr, " for %ld. MET key (%lf %ld)\n" \
                          , i, mK->_metKeyVal, mK->_instr) )

#define FPPLTERRSFX( MSG) \
        FP1(MSG); \
        FPERRFULL( (stderr, " for %ld. PLT key (%ld)\n" \
                          , i, orbdb_unloadPltKey( pK)) )

#define FPERR( S) FPERRFULL((stderr, "\n***%s\n", S))
#define FPERRFULL( A) FP( A); if ( ofs) orbdb_close( ofs); return -1
#define FPDOT FP1(".")
#define FPOK FP1(" ok\n")
#define FP1(S) FP((stderr, "%s", S))
#define FP(A) fprintf A; fflush(stderr)

/********************************/

int
main(int argc, char **argv) {

ORBDB_FILES *ofs = (ORBDB_FILES *) NULL;

ORBDB_STATE mState;
metCONTENT *mC;
metKEY *mK, *mK2;

ORBDB_STATE pState;
pltCONTENT *pC;
pltKEY *pK, *pK2;

long i, j, numPlates;

char *pfx;

  switch ( argc) {
  case 2:
    pfx = argv[1];
    break;
  default:
    FPERR( "USAGE:  orbdbDump DB-prefix\n\
  (dumps orbit database)");
    break;
  }
    
  if ( !(ofs=orbdb_openForRead( pfx, (ORBDB_FILES *) NULL)) ) {
    FPERR( "PROBLEM OPENING OLD FILES FOR READ");
  }
    
  /***********************************************/

  numPlates = orbdb_getNumPlates( ofs);
  if ( numPlates < 0) {
    FPERR( "PROBLEM GETTING NUMBER OF PLATES");
  }
  FP(( stderr, "%ld plates = expected size of plate model\n", numPlates));
  FP(( stdout, "%ld plates = expected size of plate model\n", numPlates));

  i = 0;
  mK = orbdb_metFirstkey( ofs, &mState);
  while ( mK) {
    ++i;
    mC = orbdb_fetchMetContentByKey( ofs, mK);
    if ( !mC) { 
      FPMETERRSFX( "***No content returned by orbdb_fetchMetContentByKey()");
    }

    if ( mC->_dSize < (sizeof(metCONTENT) + (sizeof(pltKEY)*mC->_nVisPlates)) ){
      FPMETERRSFX( "***Content too small for nVisPlates");
    }

#   define P(F,V) fprintf( stdout, F, V);
#   define PM(F,M) P(F,mC->M)
#   define PMC(F,M,C) for (j=0; j<C; ++j) { P(F,mC->M[j]); }

    P( " %lf", mK->_metKeyVal);    /* KEY:  kmet */
    P( " %ld", mK->_instr);    /* KEY:  kmet */
    PM(" %ld", _instr);           /* instrument */
    PM(" %lf", _met);           /* real SPICE MET */
    PMC(" %lf", _sunVec,3);     /* unit vector to sun */
    PMC(" %lf", _scVec,3);      /* space craft position wrt body center */
    PMC(" %lf", _eulCam2Abf,3);/*euler angles "camera" to ABF (SPICE), radians*/
    PM(" %lf", _resolution);    /* avg pixel/km */
    PMC(" %lf", _miscVals,5);   /* misc values, init to -999.0 */
    PM(" %lf", _mu0);           /* avg cos(inc), avg cos(emi) */
    PM(" %lf", _mu);            /* avg cos(inc), avg cos(emi) */
    PM(" %lf", _alpha);         /* boresight cos(phase) */
    PM(" %ld", _instrPar); /*instrument parameter e.g. mirror position, filter*/
    PM(" %ld", _nVisPlates);      /* # of plates visible in observation */

    P("%s","\n");

    for ( pK=mC->_visPlates, j=0; j<mC->_nVisPlates; ++pK, ++j) {
      P(" %ld", orbdb_unloadPltKey( pK));
    }
    P("%s","\n");

    free( mC);

    mK2 = mK;
    mK = orbdb_metNextkey( ofs, &mState);
    free( mK2);
  } /* while mK */

  P( "%s", "PLATES:\n");

  i = 0;
  pK = orbdb_pltFirstkey( ofs, &pState);
  while ( pK) {
    ++i;
    pC = orbdb_fetchPltContentByKey( ofs, pK);
    if ( !pC) { 
      FPPLTERRSFX( "***No content returned by orbdb_fetchPltContentByKey()");
    }

    if ( pC->_dSize < (sizeof(pltCONTENT) + (sizeof(pltKEY)*pC->_nMetKeys)) ){
      FPPLTERRSFX( "***Content too small for nMetKeys");
    }

#   undef PM
#   undef PMC

#   define PM(F,MBR) P(F,pC->MBR)
#   define PMC(F,MBR,C) for (j=0; j<C; ++j) { P(F,pC->MBR[j]); }

    P(" %ld", orbdb_unloadPltKey( pK));  /* plate number */
    PM(" %ld", _nMetKeys);      /* # of plates visible in observation */

    for ( mK=pC->_metKeys, j=0; j<pC->_nMetKeys; ++mK, ++j) {
      P(" %lf", mK->_metKeyVal);
      P(" %ld", mK->_instr);
    }
    P("%s","\n");

    free( pC);

    pK2 = pK;
    pK = orbdb_pltNextkey( ofs, &pState);
    free( pK2);
  } /* while pK */

  orbdb_close(ofs);

  return 0;

} /* main */
