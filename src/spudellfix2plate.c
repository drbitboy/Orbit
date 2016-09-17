#include <stdio.h>
#include <string.h>
#include <math.h>

#include "spudshap.h"

int main( argc, argv) unsigned int argc; char **argv; {
SPUDR spudr;
SPUDF spudf;
VEC ABC, scalevec;
double scale, minscale, maxscale;
double *Rxyz;
unsigned long iv, i;
void spudprint_plate(SPUDF*);

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;
  spudr.eastlon = getview( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR); /**/
  if ( spudr.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;

  rmod2face( &spudr, &spudf);

  /* find max X, Y, Z, use them as semi major axes
   * - set values near 0 to zero
   * - ignore values < 0.0 because shape should be ellipse i.e. ellipse
   */

  ABC[0] = ABC[1] = ABC[2] = 0.0;
  Rxyz = spudf.Rxyz;
  for ( iv=0; iv<spudf.nv; ++iv) for ( i=0; i<3; ++i, ++Rxyz) {
    if ( fabs( *Rxyz) < 1e-12) *Rxyz = 0.0;
    else if ( ABC[i] < *Rxyz) ABC[i] = *Rxyz;
  }

  /* scale values to ellipse:  1 = (x/A)^2 + (y/B)^2 + (z/C)^2 */

  fprintf( stderr, VEC3OUT( "semi-major axes:  ", ABC));

  minscale = maxscale = 1.0;

  Rxyz = spudf.Rxyz;
  for ( iv=0; iv<spudf.nv; ++iv, Rxyz+=3) {
    for ( i=0; i<3; ++i) scalevec[i] = Rxyz[i] / ABC[i];
    scale = VDOT( scalevec, scalevec);
    if ( scale != 1.0) {
      scale = 1.0 / sqrt( scale);
      if ( scale > maxscale) maxscale = scale;
      else if ( scale < minscale) minscale = scale;
      VSCAL( scale, Rxyz);
    }
  }

  fprintf( stderr, "scale max/min = 1 +%lg/-%lg\n"
         , maxscale-1.0, 1.0-minscale);

  spudprint_plate( &spudf);

  return 0;
}
