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

#define USAGE fprintf( stderr, "USAGE:\n  %s\n" \
, "spudxlate [<-h|spud|plate filename> [XYZ] ROffset LatOffset WLonOffset" \
)

#define ERREXI(CODE) if ( i != 1) ERREX(CODE)
#define ERREX(CODE) { USAGE; exit(CODE); }

int
main( int argc, char **argv) {
char *fnShape = (argc>1) ? (*argv[1] ? argv[1] : (char *) 0) : (char *) 0;
char *fnAct;
SPUDR spudr;
int iargc;
VEC pos;
VEC offset;
VEC dir;
VEC ddir;
double dist;
unsigned long ipHit;
int i;
int dargc;
double mydpr = 180.0 / acos(-1.0);
double myrpd = 1.0 / mydpr;
double r, lat, wlon;

double dlat, dlon, rmax, vl, xlat, xlon;
double xlatradians, xlonradians, coslat, sinlat, coslon, sinlon;
double *vtx0;
long ilat, jlon, iv;
long misses;

  if ( fnShape ) if ( !strcmp( fnShape, "-h")) ERREX(0)

  /* if third argument is  "XYZ" 
   * then arguments are body-fixed * R, Lat (deg), WLon (degW)
   */

  dargc = (argc<3) ? 0 : strcmp(argv[2],"XYZ") ? 0 : 1;

  /* Get vectors
   */
  if ( argc!=(5+dargc)) { ERREX(-99) }

  for ( iargc=(2+dargc); iargc<argc; ++iargc)
  {
    i = sscanf( argv[iargc], "%lg", offset + iargc-(2+dargc) );
    ERREXI( 1-iargc);
  }

  if (!dargc) {
    r = offset[0];
    lat = offset[1] * myrpd;
    wlon = offset[2] * myrpd;
    offset[0] = cos(-wlon) * cos(lat);
    offset[1] = sin(-wlon) * cos(lat);
    offset[2] = sin(lat);
    VSCAL( r, offset);
  }

  /* read plate model
  */
  fnAct = fnShape;

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;

  spudr.eastlon =
    getviewByname(spudr.Rmodel[0],&spudr.nlatR,&spudr.nlonR,fnShape,&fnAct);

  if (spudr.eastlon == -1)
  {
    fprintf( stderr, "Not a radial model; exiting ...\n\n");
    ERREX(-98)
  }

  fprintf( stderr, "Actual shape/plate model filename = ");
  fflush( stderr);
  fprintf( stderr, "%s\n", fnAct ? fnAct : "<null>");
  fflush( stderr);

  if ( !spudr.eastlon) spudr.eastlon = -1;

  dlat = 180.0 / (spudr.nlatR-1.0);
  dlon = 360.0 / (spudr.nlonR-1.0);

  // Find radius of longest vertex

  rmax = 0.0;
  for ( ilat=0; ilat<spudr.nlatR; ++ilat)
    for ( jlon=0; jlon<spudr.nlonR; ++jlon)
      if ( spudr.Rmodel[ilat][jlon] > rmax) rmax = spudr.Rmodel[ilat][jlon];

  fprintf( stderr, "dlat, dlon, rmax = %lf, %lf, %lf\n", dlat, dlon, rmax);
  fprintf( stderr, "offset[X,Y,Z] = %lf, %lf, %lf\n"
         , offset[0], offset[1], offset[2]);

  // ... and add offset length to that, then triple it

  rmax += VLEN(offset);
  rmax *= 3;
  
  for ( misses=ilat=0; ilat<spudr.nlatR; ++ilat)
  {
  double scalemisses;
    xlat = (ilat * dlat) - 90.0;
    xlatradians = xlat * myrpd;
    coslat = cos(xlatradians);
    sinlat = sin(xlatradians);
    for ( jlon=0; jlon<spudr.nlonR; ++jlon)
    {
    double reciplendir;
    double newrad, tmprad, delta, tol;
    double oldlat, oldlon, oldrad;
      xlon = jlon * dlon;
      xlonradians = xlon * myrpd * spudr.eastlon;
      coslon = cos(xlonradians);
      sinlon = sin(xlonradians);
      LOADVEC(coslon*coslat,sinlon*coslat,sinlat, dir); // direction from offset

      tol = rmax * 1e-7;
      for ( delta=(newrad=(rmax/2)/4); delta>1e-7 || delta>tol; delta /= 2.0)
      {
        vmxpb( newrad, dir, offset, pos);        // position wrt old center

        tmprad = VLEN(pos);                      // distance from old center

        oldlat = asin( pos[2] / VLEN(pos)) * mydpr;
        if ( pos[0] != 0.0 || pos[1] != 0.0)
        {
          oldlon = atan2( spudr.eastlon * pos[1], pos[0]) * mydpr;
        }
        else
        {
          oldlon = 0.0;
        }
        oldrad = lltor1( &spudr, oldlat, oldlon);

        if  (oldrad==tmprad) break;

        if ( oldrad > tmprad )                      // pos is inside shape
          newrad += delta;
        else
          newrad -= delta;
      }
      scalemisses = 1e-12;

      printf( "%10.4lf%10.4lf%10.4lf\n", xlat, xlon, newrad);
    }
  }
  if ( spudr.eastlon==1) printf( "eastlon\n");
  if ( misses) fprintf( stderr, "%ld misses\n", misses);

  return 0;
}
