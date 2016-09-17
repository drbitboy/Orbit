#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#ifdef vms
char *malloc();
#else
#include <malloc.h>
#endif

#include "debug.h"
#include "spudshap.h"
#include "orbit_spice_names.h"

#define nvert spudf->nv
#define np spudf->nface
#define spudf spudv->_spudf
#define plateVerts spudf2->_plateVerts
#define adjPlates spudf2->_adjPlates
#define coneOSilence spudf2->_coneOSilence
#define coneOVis spudf2->_coneOVis

#define UNORM( RECIP, V) RECIP = 1.0 / VLEN(V); VSCAL( RECIP, V)

int
main( int argc, char **argv) {
char *fnShape = (argc>1) ? argv[1] : (char *) 0;
SPUDV *spudv;
SPUDF2 *spudf2;
double sumtwos=0.0, sumcos=0.0;
double frac;
unsigned long i, iv, iv2, ip;
double maxSilence=-1.0, maxVis=-1.0;
double r2d = 180.0 / acos(-1.0);
VEC testVec;
double *vXyz, *v2Xyz, r, *coneV, cosAng, ang, *coneS;
unsigned long *pV, *aP;

  spudv = getSpudvByname( fnShape);
  if ( !spudv) return 0;
  fprintf( stderr, "nv = %ld ", spudf->nv);
  spudf2 = newSpudf2( spudf);
  if ( !spudf2) return 0;

  for ( iv=0; iv<nvert; ++iv) {
    sumtwos += 2;
    sumcos += (coneOSilence[3+(iv*4)] + coneOVis[3+(iv*4)]);
    if ( maxSilence<coneOSilence[3+(iv*4)]) maxSilence = coneOSilence[3+(iv*4)];
    if ( maxVis<coneOVis[3+(iv*4)]) maxVis = coneOVis[3+(iv*4)];

    vXyz = spudf->Rxyz + (3*iv);
    coneV = spudf2->_coneOVis + (4*iv);
    v2Xyz = spudf->Rxyz;
    for ( iv2=0; iv2<iv; ++iv2, v2Xyz+=3) {
      VMINUS2( v2Xyz, vXyz, testVec);
      UNORM( r, testVec);
      cosAng = VDOT( testVec, coneV);
      if ( (cosAng-coneV[3]) > 1e-10) {
        fprintf( stderr
        , "Vert %ld inside cone for vert %ld (delta = %lg; fract delta = %lg)\n"
        , iv2, iv, cosAng - coneV[3]
        , (cosAng-coneV[3])/(coneV[3]==0.0?fabs(cosAng):fabs(coneV[3])));
    } }

    v2Xyz += 3;
    for ( ++iv2; iv2<nvert; ++iv2, v2Xyz+=3) {
      VMINUS2( v2Xyz, vXyz, testVec);
      UNORM( r, testVec);
      cosAng = VDOT( testVec, coneV);
      if ( (cosAng-coneV[3]) > 1e-10) {
        fprintf( stderr
        , "Vert %ld inside cone for vert %ld (delta = %lg; fract delta = %lg)\n"
        , iv2, iv, cosAng - coneV[3]
        , (cosAng-coneV[3])/(coneV[3]==0.0?fabs(cosAng):fabs(coneV[3])));
    } }
  }

  frac = sumcos / sumtwos;
  fprintf( stderr, "frac = %lg; 1/frac = %lg\n"
         , frac, frac!=0.0 ? 1.0/frac : 0.0);
  fprintf( stderr, "min cone angles Vis/Silence = %lf / %lf deg\n"
        , r2d*acos(maxVis), r2d*acos(maxSilence));

  fprintf( stdout, "SPUDF2\n%ld\n", nvert);
  coneV = spudf2->_coneOVis;
  coneS = spudf2->_coneOSilence;
  for ( iv=0; iv<nvert; ++iv) {
    for ( i=0; i<4; ++i) fprintf( stdout, " %.12lg", *(coneV++));
    for ( i=0; i<4; ++i) fprintf( stdout, " %.12lg", *(coneS++));
    fprintf( stdout, "\n");
  }
  pV = plateVerts;
  aP = adjPlates;
  for ( ip=0; ip<np; ++ip) {
    for ( i=0; i<3; ++i) fprintf( stdout, " %ld", *(pV++));
    for ( i=0; i<3; ++i) fprintf( stdout, " %ld", *(aP++));
    fprintf( stdout, "\n");
  }

  return 0;
}
