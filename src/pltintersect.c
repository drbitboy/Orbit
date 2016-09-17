/* pltintersect.c
 * - Find intersection of a vector with a plate model
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

#define USAGE fprintf( stderr, "USAGE:\n  %s\n" \
, "pltintersect [<-h|spud|plate filename> [RLatWLon] Xpos Ypos Zpos [Xdir Ydir Zdir]" \
)

#define ERREXI(CODE) if ( i != 1) ERREX(CODE)
#define ERREX(CODE) { USAGE; exit(CODE); }

int
main( int argc, char **argv) {
char *fnShape = (argc>1) ? (*argv[1] ? argv[1] : (char *) 0) : (char *) 0;
double kmperinch, shellthick, dimplePct, *xyzptr;
SPUDV *spudv;
int iargc;
double vlen;
VEC pos;
VEC dir;
double dist;
unsigned long ipHit;
int i;
int dargc;
double mydpr = 180.0 / acos(-1.0);
double myrpd = 1.0 / mydpr;
double r, lat, wlon;

  if ( fnShape ) if ( !strcmp( fnShape, "-h")) ERREX(0)

  /* if third argument is  "RLatWLon" 
   * then arguments are body-fixed * R, Lat (deg), WLon (degW)
   */

  dargc = (argc<3) ? 0 : strcmp(argv[2],"RLatWLon") ? 0 : 1;

  /* Get vectors
   */
  if ( argc!=(8+dargc) && argc != (5+dargc)) { ERREX(-99) }

  for ( iargc=(2+dargc); iargc<argc; ++iargc)
  {
    i = sscanf( argv[iargc], "%lg"
                , (iargc<(5+dargc)) 
                  ? (pos + iargc-(2+dargc)) 
                  : (dir + iargc-(5+dargc))
               );
    ERREXI( 1-iargc);
  }

  if (dargc) {
    r = pos[0];
    lat = pos[1] * myrpd;
    wlon = pos[2] * myrpd;
    pos[0] = cos(-wlon) * cos(lat);
    pos[1] = sin(-wlon) * cos(lat);
    pos[2] = sin(lat);
    VSCAL( r, pos);

    if ( argc = 9)
    {
      r = dir[0];
      lat = dir[1] * myrpd;
      wlon = dir[2] * myrpd;
      dir[0] = cos(-wlon) * cos(lat);
      dir[1] = sin(-wlon) * cos(lat);
      dir[2] = sin(lat);
      VSCAL( r, dir);
    }
  }

  /* Point nadir if no dir
   */
  if ( iargc == (5+dargc) ) { VNEG2( pos, dir); }

  /* Make dir a unit vector
   */
  vlen = sqrt(VDOT(dir,dir));
  VSCAL( 1.0/vlen, dir);
  
  /* read spud or plate model
  */
  if ( !(spudv = getSpudvByname( fnShape)) ) return 0;
  fprintf( stderr, "np = %ld\n", np);

  spudf_intersect( spudf, pos, dir, &dist, &ipHit);
  if ( ipHit < spudf->nface)
  {
  VEC viewed, uvToViewer;
  double emiss;
  double *uvNormal = spudf->uvnorms + (3 * ipHit);
    vmxpb( dist, dir, pos, viewed);
    VMINUS2( pos, viewed, uvToViewer);
    vhat( uvToViewer, uvToViewer);
#   define ACOSWLIM(B) ((B>1.0) ?0.0 :((B<-1.0) ? 180.0 : (acos(B) * mydpr)))
    emiss = VDOT( uvToViewer, uvNormal);
    emiss = ACOSWLIM(emiss);

#   define RLATWLON(V) \
      r = VLEN(V); \
      lat = asin( V[2] / r) * mydpr; \
      wlon = (360.0 - atan2( V[1], V[0]) * mydpr); \
      if ( wlon > 360.0) wlon -= 360.0

    RLATWLON(dir);
    printf( "\nVector direction XYZ = %lf %lf %lf\n", dir[0], dir[1], dir[2]);
    printf( " = R, Lat, Wlon, deg = %lf %lf %lf\n", r, lat, wlon);
    printf( "\nAt distance          = %lf\n", dist);

    RLATWLON(pos);
    printf( "\nFrom position XYZ =    %lf %lf %lf\n", pos[0], pos[1], pos[2]);
    printf( " = R, Lat, Wlon, deg = %lf %lf %lf\n", r, lat, wlon);

    RLATWLON(viewed);
    printf( "\nIntersected plate %lu\n", ipHit);
    printf( "  at XYZ =             %lf %lf %lf\n", viewed[0], viewed[1], viewed[2]);
    printf( " = R, Lat, Wlon, deg = %lf %lf %lf\n", r, lat, wlon);

    RLATWLON(uvNormal);
    printf( "\nNormal XYZ =           %lf %lf %lf\n", uvNormal[0], uvNormal[1], uvNormal[2]);
    printf( " = R, Lat, Wlon, deg = %lf %lf %lf\n", r, lat, wlon);
    printf( "\nAngle btw Dir & Norm = %lf\n", emiss);
  }
  else
  {
    printf( "\nNo intersection; %s exiting ...\n", argv[0]);
  }
  printf( "\n");

  return 0;
}
