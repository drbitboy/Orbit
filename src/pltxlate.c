/* pltxlate.c
 * - Interpolate plate model to shape model
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

#define nv spudf->nv
#define np spudf->nface
#define spudf spudv->_spudf

#define USAGE fprintf( stderr, "USAGE:\n  %s\n" \
, "pltxlate [<-h|spud|plate filename> [XYZ] ROffset LatOffset WLonOffset" \
)

#define ERREXI(CODE) if ( i != 1) ERREX(CODE)
#define ERREX(CODE) { USAGE; exit(CODE); }

int
main( int argc, char **argv) {
char *fnShape = (argc>1) ? (*argv[1] ? argv[1] : (char *) 0) : (char *) 0;
char *fnAct;
SPUDV *spudv;
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

  /* read spud or plate model
  */
  if ( !(spudv = getSpudvByname( fnShape)) ) return 0;
  fprintf( stderr, "np = %ld\n", np);

  fnAct = fnShape;

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;

  spudr.eastlon =
    getviewByname(spudr.Rmodel[0],&spudr.nlatR,&spudr.nlonR,fnShape,&fnAct);

  fprintf( stderr, "Actual shape/plate model filename = ");
  fflush( stderr);
  fprintf( stderr, "%s\n", fnAct ? fnAct : "<null>");
  fflush( stderr);

  if (spudr.eastlon == -1)
  {
    fprintf( stderr, "Not a radial model\n");
    spudr.nlatR = 91;
    spudr.nlonR = 181;
    spudr.eastlon = 0;
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;

  dlat = 180.0 / (spudr.nlatR-1.0);
  dlon = 360.0 / (spudr.nlonR-1.0);


  // Find radius of longest vertex

  rmax = 0.0;
  for ( vtx0=spudf->Rxyz+(iv=0); iv<nv; ++iv, vtx0+=3) {
    if ( (vl=VLEN(vtx0)) > rmax) {
      rmax = vl;
    }
  }
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
    VEC viewed;
      xlon = jlon * dlon;
      xlonradians = xlon * myrpd * spudr.eastlon;
      coslon = cos(xlonradians);
      sinlon = sin(xlonradians);
      LOADVEC(coslon*coslat,sinlon*coslat,sinlat, pos); // direction from offset
      VSCAL2(-1.0, pos, dir);                         // direction toward offset
      VSCAL(rmax, pos);                        // scale position beyond surface
      VADD2( offset, pos, pos);                // position wrt origin

      spudf_intersect( spudf, pos, dir, &dist, &ipHit);   // find intersection

      scalemisses = 1e-12;

      while ( ipHit>=spudf->nface )  // if no intersection
      {
      VEC dpos;
      double reciplendpos;
        scalemisses *= 1.0000001;
        ++misses;
        if ( misses==99 || misses==999 || misses==9999 || misses==99999 
           ||  (misses % 1000000)==999999)
        {
          fprintf( stderr, "%ld misses and counting ...\n", misses);
        }
        LOADVEC( rand(), rand(), rand(), dpos);      // make random delta vector
        reciplendpos = scalemisses * rmax / VLEN(dpos);
        VSCAL(reciplendpos, dpos);                  // scale it to to rmax/10^12
        VADD2( pos, dpos, pos);                     // add it to pos
        spudf_intersect( spudf, pos, dir, &dist, &ipHit);  // find intersection
      }
      vmxpb( dist, dir, pos, viewed);               // point of intersection
      VMINUS2( viewed, offset, viewed);             // ... wrt offset
      printf( "%10.4lf%10.4lf%10.4lf\n", xlat, xlon, VLEN(viewed));
    }
  }
  if ( spudr.eastlon==1 ) printf( "EASTLON\n");
  if ( misses) fprintf( stderr, "%ld misses\n", misses);

  return 0;
}
