/* orbdb.c - orbit dbm routines
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define _ORBDB_PRIVATE_H_
#include "orbdb.h"

#define FPERR( S) FPERRFULL((stdout, "\n***%s\n", S))
#define FPERRFULL( A) FP( A); if ( ofs) orbdb_close( ofs); return -1
#define FPDOT FP1(".")
#define FPOK FP1(" ok\n")
#define FP1(S) FP((stdout, "%s", S))
#define FP(A) fprintf A; fflush(stdout)

/********************************/

int
main(int argc, char **argv) {

ORBDB_FILES *ofs = (ORBDB_FILES *) NULL;
ORBDB_STATE mState, pState;

metCONTENT *mC;
metKEY *mK, *mK2;

pltCONTENT *pC;
pltKEY *pK, *pK2;

long i, j;
long nPltKeys, nMetKeys;
long iUP, notEqual;
long numPlates, highestPlateNum;
unsigned int iTest = 100;
unsigned int iseed;
long tested;
time_t ttt;
long zeroes;

char pfx[1024];

  switch ( argc) {
  case 4:
  case 3:
    if ( 1 != sscanf( argv[2], "%ld", &iTest)) iTest = 100;
    if ( iTest > 100) iTest = 100;
    else if ( iTest < 1) iTest = 1;
    iseed = 0;
    if ( argc > 3) sscanf(argv[3], "%ld", &iseed);
    if ( iseed == 0) {
      ttt = time( &ttt);
      iseed = (unsigned int) (ttt | 1);
    }

    FP((stdout
      , "Will perform approx %ld%% of possible consistency checks%s%ld\n"
      , iTest, "; seed = ", iseed));

    srand( iseed);
  case 2:
    iTest = ( (((double) RAND_MAX) * iTest) + 50.0) / 100;
    strncpy( pfx, argv[1], 1023);
    pfx[1023] = '\0';
    break;
  case 1:
    *pfx = '\0';
    break;
  default:
    FPERR( "USAGE:\n\
  orbdbck [DB-prefix [%% consist. checks]]\n\
  (performs consistency check on orbit database; test=1-100, default=100)");
    break;
  }
    
  /***********************************************/
  FP((stdout, "Testing orbdb files (prefix='%s') open & close routines\n  Open "
            , pfx));

  if ( (ofs=orbdb_openForRead( pfx, (ORBDB_FILES *) NULL)) ) {
    FPDOT;
  } else { 
    FPERR( "PROBLEM OPENING OLD FILES FOR READ");
  }
  FPOK;
    
  /***********************************************/
  FP((stdout, "Testing Number of plates\n  "));

  numPlates = orbdb_getNumPlates( ofs);
  if ( numPlates < 0) {
    FPERR( "PROBLEM GETTING NUMBER OF PLATES");
  }
  if ( numPlates) {
    FP(( stdout, "%ld plates = expected size of plate model", numPlates));
  } else {
    FP(( stdout, "WARNING:  size of plate model not yet stored in ORBDB"));
  }
  FPOK;

  /***************************************************/
  FP1("Testing all keys in MET database\n  ");

  nMetKeys=0;
  mK = orbdb_metFirstkey( ofs, &mState);
  while ( mK) {
    if ( !((nMetKeys++)%100)) { FPDOT; }
    mK2 = mK;
    mK = orbdb_metNextkey( ofs, &mState);
    free( mK2);
  }
  FP((stdout, " %ld keys found;", nMetKeys));
  FPOK;

  /***************************************************/
  FP1("Testing all keys in PLT database\n  ");

  highestPlateNum = nPltKeys = 0;
  pK = orbdb_pltFirstkey( ofs, &pState);
  while ( pK) {
    if ( orbdb_unloadPltKey(pK) > highestPlateNum)
      highestPlateNum = orbdb_unloadPltKey(pK);
    if ( !((nPltKeys++)%100)) { FPDOT; }
    pK2 = pK;
    pK = orbdb_pltNextkey( ofs, &pState);
    free( pK2);
  }
  FP((stdout, " %ld keys found; highest plate number = %ld"
            , nPltKeys, highestPlateNum));
  if ( numPlates) if ( numPlates < highestPlateNum) {
    FP((stdout, "\n  WARNING:  highest plate number > stored model size"));
  }
  FPOK;

  /***************************************************/
  FP1("Testing orbdb MET=>PLT=>MET consistency\n  ");

# ifdef FPERRSFX
# undef FPERRSFX
# endif
# define FPERRSFX(MSG) \
  FP1(MSG); \
  FPERRFULL( (stdout, " for %ld. MET key (%lf %ld)\n" \
           , i, mK->_metKeyVal, mK->_instr) );

# define NESET(VAL1,VAL2) if ( (VAL1) != (VAL2)) notEqual |= (1<<j); j++

  zeroes = tested = i = 0;
  mK = orbdb_metFirstkey( ofs, &mState);
  while ( mK) {
    if ( !((i++)%100)) { FPDOT; }

    if ( (iTest<RAND_MAX) && !(i%1000)) {
      FP((stdout, "%ld", (long)((tested*0.1) + 0.5)));
      tested = 0;
    }
      
    if ( rand() <= iTest) {
    tested++;

#   if 0
    FP((stdout, " %lf", mK->_metKeyVal));
#   endif

    mC = orbdb_fetchMetContentByKey( ofs, mK);
    if ( !mC) { 
      FPERRSFX( "***No content returned by orbdb_fetchMetContentByKey()");
    }

    if ( mC->_dSize < (sizeof(metCONTENT) + (sizeof(pltKEY)*mC->_nVisPlates)) ){
      FPERRSFX( "***Content too small for nVisPlates");
    }

    if ( !mC->_nVisPlates) zeroes++;

    j = notEqual = 0;
    NESET( mK->_metKeyVal, TRUNCMET_NEAR(mC->_met));
    NESET( mK->_instr, mC->_instr);
    for ( pK=mC->_visPlates; pK<(mC->_visPlates+mC->_nVisPlates); ++pK) {
      pC = orbdb_fetchPltContentByKey( ofs, pK);
      if ( !pC) {
        FP((stdout, "***No plate content found for plate # %ld"
                  , orbdb_unloadPltKey(pK)));
        FPERRSFX( "" );
      }
      if ( pC->_dSize < (sizeof(pltCONTENT)+(sizeof(metKEY)*pC->_nMetKeys)) ){
        FP((stdout, "***Content too small for plate # %ld"
                  , orbdb_unloadPltKey(pK)));
        FPERRSFX( "" );
      }
      for ( mK2=pC->_metKeys; mK2<(pC->_metKeys+pC->_nMetKeys); ++mK2) {
        if ( orbdb_compareMetKeys( mK2, mK) >= 0) break;
      }
      if ( mK2 == (pC->_metKeys+pC->_nMetKeys)) {
        FP((stdout, "***MET key not found in content of plate # %ld"
                  , orbdb_unloadPltKey(pK)));
        FPERRSFX( "");
      }
      if ( orbdb_compareMetKeys( mK2, mK) > 0) {
        for ( ++mK2; mK2<(pC->_metKeys+pC->_nMetKeys); ++mK2) {
          if ( orbdb_compareMetKeys( mK2, mK) == 0) break;
        }
        if ( mK2 == (pC->_metKeys+pC->_nMetKeys)) {
          FP((stdout, "***MET key not found in content of plate # %ld"
                    , orbdb_unloadPltKey(pK)));
        } else {
          FP((stdout, "***MET keys out of order in content of plate # %ld"
                  , orbdb_unloadPltKey(pK)));
        }
        FPERRSFX( "");
      }
      free( pC);
    } /* for pK */
    if ( notEqual) {
      FP((stdout, "***MET content not consistent with key\n  "));
      FP((stdout, "   Content:  met=%lf; instr=%ld\n  ", mC->_met, mC->_instr));
      FPERRSFX("  ");
    }
    free( mC);

    } /* if rand() */

    mK2 = mK;
    mK = orbdb_metNextkey( ofs, &mState);
    free( mK2);
  } /* while mK */

  FP((stdout, "%ld MET%s w/zero pltKEYs;", zeroes, (zeroes==1) ? "" : "s"));
  FPOK;

  /***************************************************/
  FP1("Testing orbdb PLT=>MET=>PLT consistency\n  ");

# ifdef FPERRSFX
# undef FPERRSFX
# endif
# define FPERRSFX(MSG) \
  FP1(MSG); \
  FPERRFULL( (stdout, " for plate # %ld.\n", orbdb_unloadPltKey) )

  zeroes = tested=i=0;
  pK = orbdb_pltFirstkey( ofs, &pState);
  while ( pK) {
    if ( !((i++)%100)) { FPDOT; }

    if ( (iTest<RAND_MAX) && !(i%1000)) {
      FP((stdout, "%ld", (long)((tested*0.1) + 0.5)));
      tested = 0;
    }

    if ( rand() <= iTest) {
    tested++;

#   if 0
    FP((stdout, " %ld", orbdb_unloadPltKey(pK)));
#   endif

    pC = orbdb_fetchPltContentByKey( ofs, pK);
    if ( !pC) { 
      FPERRSFX( "***No plate content found");
    }

    if ( pC->_dSize < (sizeof(pltCONTENT) + (sizeof(metKEY)*pC->_nMetKeys)) ){
      FPERRSFX( "***Content too small for nMetKeys");
    }

    if ( !pC->_nMetKeys) zeroes++;

    j = notEqual = 0;
    for ( mK=pC->_metKeys; mK<(pC->_metKeys+pC->_nMetKeys); ++mK) {
      mC = orbdb_fetchMetContentByKey( ofs, mK);
      if ( !mC) {
        FP((stdout, "***No met content found for MET key (%lf %ld)"
                  , mK->_metKeyVal, mK->_instr));
        FPERRSFX( "" );
      }
      for ( pK2=mC->_visPlates; pK2<(mC->_visPlates+mC->_nVisPlates); ++pK2) {
        if ( orbdb_comparePltKeys( pK2, pK) >= 0) break;
      }
      if ( pK2 == (mC->_visPlates+mC->_nVisPlates)) {
        FP((stdout, "***PLT key not found in content of met key (%lf %ld)"
                  , mK->_metKeyVal, mK->_instr));
        FPERRSFX( "");
      }
      if ( orbdb_comparePltKeys( pK2, pK) > 0) {
        for ( ++pK2; pK2<(mC->_visPlates+mC->_nVisPlates); ++pK2) {
          if ( orbdb_comparePltKeys( pK2, pK) == 0) break;
        }
        if ( pK2 == (mC->_visPlates+mC->_nVisPlates)) {
          FP((stdout, "***PLT key not found in content of met key (%lf %ld)"
                  , mK->_metKeyVal, mK->_instr));
        } else {
          FP((stdout, "***PLT key out of order in content of met key (%lf %ld)"
                  , mK->_metKeyVal, mK->_instr));
        }
        FPERRSFX( "");
      }
      free( mC);
    } /* for pK */

    } /* if rand() */

    pK2 = pK;
    pK = orbdb_pltNextkey( ofs, &pState);
    free( pK2);
  } /* while pK */
  FP((stdout, "%ld plate%s w/zero metKEYs;", zeroes, (zeroes==1) ? "" : "s"));
  FPOK;

  orbdb_close(ofs);

  return 0;

} /* main */
