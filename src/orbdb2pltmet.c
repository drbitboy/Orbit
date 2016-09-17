/* orbdb2plt2met <orbdb prefix>
 * - create <orbdb prefix>.pltmet raw file for binner
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "orbdb.h"

int
main(int argc, char **argv) {

ORBDB_FILES *ofs = (ORBDB_FILES *) NULL;

metCONTENT *mC;
metKEY *mK, *endMK;

char *pltmetFn;
FILE *pltmetF;

pltCONTENT *pC;
pltKEY *pK;

long plateNum;
long *metPtr;
long nPltKeys, nMetKeys;
long numPlates;

#define orbdbPfx argv[1]

#define USAGE \
fprintf( stderr, \
"Usage:  orbdb2plt2met <orbdb prefix>\n   - create <orbdb prefix>.pltmet raw file for binner\n\n");

  if ( argc != 2) {
    USAGE;
    exit( -1);
  }

  if ( !(ofs=orbdb_openForRead( orbdbPfx, (ORBDB_FILES *) NULL)) ) {
    fprintf( stderr, "ERROR OPENING ORBIT DATABASE\n");
    USAGE;
    exit(-1);
  }

  if ( 1 > (numPlates=orbdb_getNumPlates(ofs)) ) {
    orbdb_close( ofs);
    fprintf( stderr, "ERROR IN SIZE OF MODEL (%ld plates)\n", numPlates);
    USAGE;
    exit( -1);
  }

  metPtr = (long *) malloc( (numPlates+1) * sizeof( long));

  if ( !metPtr) {
    orbdb_close( ofs);
    fprintf( stderr, "ERROR ALLOCATING MEMORY FOR MET POINTERS\n", numPlates);
    USAGE;
    exit( -1);
  }

  *metPtr = 0;
  for ( plateNum=1; plateNum<=numPlates; ++plateNum) {
    pC = orbdb_fetchPltContent( ofs, (long) plateNum);
    if ( pC) {
      metPtr[plateNum] = metPtr[plateNum-1] + pC->_nMetKeys;
      free( pC);
    } else metPtr[plateNum] = metPtr[plateNum-1];
  }

  pltmetFn = (char *) malloc( strlen(orbdbPfx) + 20);
  strcpy( pltmetFn, orbdbPfx);
  strcat( pltmetFn, ".pltmet");
  pltmetF = fopen( pltmetFn, "w");
  free( pltmetFn);
  if ( !pltmetF) {
    orbdb_close( ofs);
    fprintf( stderr, "ERROR OPENING %s%s\n", orbdbPfx, ".pltmet");
    USAGE;
    exit( -1);
  }

  fwrite( &numPlates, 1, sizeof(long), pltmetF);
  fwrite( metPtr+numPlates, 1, sizeof(long), pltmetF);
  fwrite( metPtr, 1, sizeof(long)*(numPlates+1), pltmetF);

  for ( plateNum=1; plateNum<=numPlates; ++plateNum) {
    pC = orbdb_fetchPltContent( ofs, (long) plateNum);
    if ( pC) {
      mK = pC->_metKeys;
      endMK = mK + pC->_nMetKeys;
      while ( mK < endMK) {
        mC = orbdb_fetchMetContentByKey( ofs, mK++);
        if ( mC) {
          fwrite( &mC->_met, 1, sizeof( double), pltmetF);
          free( mC);
        } else {
          orbdb_close( ofs);
          fclose( pltmetF);
          fprintf( stderr, "ERROR - INCONSISTENT ORBDB, RUN orbdbck\n");
          free( pC);
          exit( -1);
        }
      }
      free( pC);
    }
  }

  orbdb_close( ofs);
  fclose( pltmetF);
  return 0;
}
