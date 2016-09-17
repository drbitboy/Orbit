#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "spudshap.h"

double
plate_getPlateCenter( SPUDF *spudf, long pltNum, VEC vReturn) {
double vReturnLen;
double *vPtr;
  vPtr = spudf->Rxyz + (3 * spudf->faceapices[pltNum]);
  CPYVEC( vPtr, vReturn);
  vPtr = spudf->Rxyz + (3 * spudf->oe[spudf->faceoeidx[pltNum]]);
  VADD2( vReturn, vPtr, vReturn);
  vPtr = spudf->Rxyz + (3 * spudf->oe[spudf->faceoeidx[pltNum] + 1]);
  VADD2( vReturn, vPtr, vReturn);
  vReturnLen = 1.0 / 3.0;
  VSCAL( vReturnLen, vReturn);
  vReturnLen = VLEN( vReturn);
  return vReturnLen;
}

/*************************************************/
int main( argc, argv) unsigned int argc; char **argv; {
double rOffset;
long lonOffset;
double dlon;
long jLonOffset, jLonLen;
char *fnSpudTopo;
char *fnPlt;
char *rtnFnPlt;
SPUDR spudrTopo;
SPUDF spudfTopo;
SPUDR spudr;
SPUDF *spudf;
long j, i, ip;
double *pC;
long *lclNFace;
double lclRmodel[SPUDRNLON];
double maxR;
void spudprint_plate(SPUDF*);

  if ( argc < 5) {
    fprintf( stderr
      , "Usage:  %s rOffset lonOffset spudTopoFilename plateFilename\n"
      , argv[0]);
    exit(-1);
  }

  rOffset = (argc>1) ? atof( *argv[1] ? argv[1] : "0") : 0.0;
  lonOffset = (argc>2) ? atoi( *argv[2] ? argv[2] : "0") : 0;
  fnSpudTopo = (argc > 3) ? argv[3] : (char *) 0;
  fnPlt = (argc > 4) ? argv[4] : (char *) 0;

  spudrTopo.nlatR = SPUDRNLAT;
  spudrTopo.nlonR = SPUDRNLON;
  spudrTopo.eastlon = getviewByname( spudrTopo.Rmodel[0]
                                   , &spudrTopo.nlatR, &spudrTopo.nlonR
                                   , fnSpudTopo, (char **) 0);
  if ( spudrTopo.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudrTopo.eastlon) spudrTopo.eastlon = -1;

  dlon = 360.0 / (spudrTopo.nlonR - 1);
  jLonOffset = lonOffset / dlon;
  if ( ((double)lonOffset) != (jLonOffset * dlon) ) {
    fprintf( stderr, "Roundoff error; exiting\n");
    exit( -2);
  }

  while ( jLonOffset < 0) jLonOffset += spudrTopo.nlonR;

  if ( jLonOffset > (spudrTopo.nlonR-1)) {
    fprintf( stderr, "Offset error; exiting\n");
    exit( -3);
  }

  jLonOffset %= (spudrTopo.nlonR - 1);
  jLonLen = (spudrTopo.nlonR-1) - jLonOffset;

  for ( maxR=i=0; i<spudrTopo.nlatR; ++i) {

    memcpy( lclRmodel+jLonOffset
          , spudrTopo.Rmodel[i]
          , (jLonLen+1) * sizeof(double));   /* add one for lon=360 */

    if ( jLonOffset > 0) 
      memcpy( lclRmodel
            , spudrTopo.Rmodel[i] + jLonLen
            , jLonOffset * sizeof(double));

    for ( j=0; j<spudrTopo.nlonR; ++j) {
      if ( (spudrTopo.Rmodel[i][j]=(lclRmodel[j]+rOffset)) > maxR) 
        maxR = spudrTopo.Rmodel[i][j];
    }
  }

  maxR *= 3;

  rmod2face( &spudrTopo, &spudfTopo);

  spudf = getplatByname( &spudr, fnPlt, &rtnFnPlt);

  for ( ip=0; ip<spudf->nface; ++ip) {
  VEC cf, uvToOrigin;
  double cfRad, scalToMaxR, dist;
  long pltNumIntersected;
    cfRad = 1.0 / plate_getPlateCenter( spudf, ip, cf);
    scalToMaxR = maxR * cfRad;
    VSCAL2( -cfRad, cf, uvToOrigin);
    VSCAL( scalToMaxR, cf);
    spudf_intersect( &spudfTopo, cf, uvToOrigin, &dist, &pltNumIntersected);
    spudf->platecolor[ip] = maxR - dist;
  }

  spudprint_plate( spudf);

  return 0;
}
