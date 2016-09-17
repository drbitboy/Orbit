/* spud2stl.c
 * - convert (plate or spud) shape model file to ascii STL file format
 */
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

#define np spudf->nface
#define spudf spudv->_spudf

#define USAGE fprintf( stderr, "USAGE:\n  %s%s\n" \
, "spud2stl [<-h|spud|plate filename> [km/inch [shell thickness, inches " \
, "[polar dimpling, %]]]]")

#define ERREXI(CODE) if ( i != 1) ERREX(CODE)
#define ERREX(CODE) { USAGE; exit(CODE); }

int
main( int argc, char **argv) {
char *fnShape = (argc>1) ? (*argv[1] ? argv[1] : (char *) 0) : (char *) 0;
double kmperinch, shellthick, dimplePct, *xyzptr;
SPUDV *spudv;
unsigned long i, nv3;
int iargc;
double northMin, southMin;
double *zNorth, *zSouth;
void spudprint_stl(SPUDF*,double);


  if ( fnShape ) if ( !strcmp( fnShape, "-h")) ERREX(0)

  /* scale, model units (name assumes kilometers, but can be anything) per inch
   */
  kmperinch = 1.0;
  if ( argc>2) {
    i = sscanf( argv[2], "%lg", &kmperinch);
    ERREXI(-2)
  }
  if ( kmperinch <= 0.0) kmperinch = 1.0;

  /* shell thickness:
   * == 0    => solid body
   * > 0     => shell of specified thickness; outer surface is model surface
   * < 0     => shell of specified thickness; inner surface is model surface
   */
  shellthick = 0.0;
  if ( argc>3) {
    i = sscanf( argv[3], "%lg", &shellthick);
    ERREXI(-3)
  }

  /* dimple:  shorten Z values nearest poles by a percentage
   */
  dimplePct = 0.0;
  if ( argc>4) {
    i = sscanf( argv[4], "%lg", &dimplePct);
    ERREXI(-4)
  }
  dimplePct = 1.0 - (dimplePct/100.0);

  /* read spud or plate model
  */
  if ( !(spudv = getSpudvByname( fnShape)) ) return 0;
  fprintf( stderr, "np = %ld\n", np);

  /* scale all vertices
   */
  nv3 = spudf->nv * 3;
  for ( xyzptr = spudf->Rxyz+(i=0) ; i<nv3; ++i) *(xyzptr++) /= kmperinch;

  /* find closest point to either pole for dimpling
   * - save Z pointers in zNorth & zSouth
   */
  zSouth = zNorth = (double *) 0;
  if ( dimplePct != 1.0) {
    northMin = southMin = 0.0;
    for ( xyzptr = spudf->Rxyz+(i=0) ; i<spudf->nv; ++i, xyzptr += 3) {
    double xy2;

      if ( xyzptr[2] != 0.0) {
        xy2 = (xyzptr[0] * xyzptr[0]) + (xyzptr[1] * xyzptr[1]);
        if ( xyzptr[2] < 0.0) {
          if ( !zSouth || (xy2 < southMin)) {
            zSouth = xyzptr + 2;
            southMin = xy2;
          }
        } else if ( xyzptr[2] > 0.0) {
          if ( !zNorth || (xy2 < northMin)) {
            zNorth = xyzptr + 2;
            northMin = xy2;
          }
        }
      }
    }
  }

  /* dimple polar points
   */
  if ( zSouth) *zSouth *= dimplePct;
  if ( zNorth) *zNorth *= dimplePct;

  /* print STL file to stdout
   */
  spudprint_stl( spudf, shellthick);

  return 0;
}
