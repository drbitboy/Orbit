#include <stdio.h>
#include <string.h>
#include <math.h>
#include <malloc.h>
#include <stdlib.h>

#include "spudshap.h"

#ifndef M_PI
#define M_PI		3.14159265358979323846
#endif

#define deg		*M_PI/180.
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

#define RTNU(I) { USAGE; return(I); }
#define USAGE \
fprintf( stderr, "Usage:  %s <deltaLat> <model1.plt> <model2.plt>\n", *argv)


int main( argc, argv) unsigned int argc; char **argv; {
SPUDR spudrArr[2];
SPUDF *spudfArr[2];
VEC surfPtArr[2];
double distArr[2];
unsigned long hitPltArr[2];
int iSpud;

#define spudr spudrArr[iSpud]
#define spudf spudfArr[iSpud]
#define dist distArr[iSpud]
#define hitPlt hitPltArr[iSpud]

VEC sc, bore;
double latdeg, londeg, sinTopLat, lat, lon, dLatLon, dLat, dLon, radius, maxR;

int i, j, k;
unsigned long nEAP, nAtLat, nLatLon[2];

#define nLon nLatLon[0]
#define nLat nLatLon[1]

double outArr[5];  /* lat, lon, R0, R1, normalDistFromR0ToModel1 */
double coslat, sinlat;
double *dPtr;

  if ( argc < 4) RTNU(-99)
  if ( 1 != sscanf(argv[1], "%lf", &dLatLon)) RTNU(-98)

  for ( maxR=iSpud=0; iSpud<2; ++iSpud) {                /* load plate models */
    spudf = getplatByname( &spudr, argv[iSpud+2], (char **) 0);
    if ( !spudf) {
      fprintf( stderr, "Failed to load shape model %s\n", argv[iSpud+2]);
      RTNU(-(iSpud+2))
    }

    dPtr = spudf->Rxyz;                                     /* find max radius */
    for ( j=0; j<spudf->nv; ++j, dPtr+=3) {
      radius = VDOT(dPtr,dPtr);
      if ( radius > maxR) maxR = radius;
    }
  }

  maxR = 3.0 * sqrt(maxR);

  /* find how many points to do */

  /*
# define NATLAT (int)(0.5 + (6 * sin(lat) / sinTopLat)
  sinTopLat   = sin( (90.0-dLatLon) deg);

  for ( latdeg=(90.0-(dLatLon/2)); latdeg > 0.0; latdeg -= dLatLon) {
    lat = latdeg deg;
    nEAP += NATATLAT;
  }
  nEAP *= 2;
  /**/

  nLat = (int)(0.5 + (180.0/dLatLon)); nLon = (int)(0.5 + (360.0/dLatLon));
  dLat = 180.0 / nLat;                 dLon = 360.0 / nLon;

  nEAP = nLat * nLon;

  fwrite( nLatLon, sizeof( unsigned long), 2, stdout);

  /* put sc at lat, lon, R=maxR, point bore at nadir */

  for ( i=0; i<nLat; ++i) {
    lat = (outArr[0] = (((i + 0.5) * dLat) -90.0)) deg;
    coslat = cos(lat);
    sinlat = sin(lat);
    for ( j=0; j<nLon; ++j) {
      lon = (outArr[1] = (j + 0.5) * dLon) deg;
      bore[0] = -coslat * cos(lon);
      bore[1] = -coslat * sin(lon);
      bore[2] = -sinlat;
      VSCAL2(-maxR,bore,sc);
      /* find intersections with both models */
      for ( iSpud=2; iSpud; ) {
        --iSpud;
        spudf_intersect( spudf, sc, bore, &dist, &hitPlt);
        outArr[iSpud+2] = (hitPlt<spudf->nface) ? (maxR - dist) : (maxR/3);
      }
      /* find distance along model 0's intersected plate's normal to model 1 
       * - go out maxR first and look back along normal
       */
      iSpud = 0;
      if ( hitPlt<spudf->nface) {
        vmxpb( dist, bore, sc, sc);   /* sc to plate intersection */
        vmxpb( maxR, spudf->uvnorms+(3*hitPlt), sc, sc);
        VNEG2( spudf->uvnorms+(3*hitPlt), bore);
        spudf_intersect( spudfArr[1], sc, bore, &dist, &hitPlt);
        outArr[4] = (hitPlt<spudfArr[1]->nface) ? (maxR-dist) : (maxR/3);
      } else outArr[4] = maxR/3;
      fwrite( outArr, sizeof( double), 5, stdout);
    } /* for j<nLon */
    fprintf( stderr, " %.1lf", lat*180.0/M_PI);
  } /* for i<nLat */
  fprintf( stderr, "\n");
  return(0);
}
