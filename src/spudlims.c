#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <malloc.h>

#include "spudshap.h"
#include "orbit3d.h"

#ifndef PI
static double PI;
#endif

#define deg *PI/180.
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

void display_3d();

int main( argc, argv) unsigned int argc; char **argv; {
SPUDR spudr;
SPUDF spudf;
int iv, i;
#define NABC 9
int ia, ib, ic;
int iia, iib, iic;
double minvol2, maxvol2;
double maxxyz2[3], maxr2, minr2, maxr6, minr6, r2, loabc2[3], hiabc2[3];
double *lxyz;
double  *xyz2, *lxyz2, *xyzabc2, *l2;
double abc2prod, abc2[3][NABC], limxyzabc2[NABC][NABC][NABC];
double xlatdeg, xlondeg, dlatdeg, dlondeg, cldll, dlatdlon, volmodel;

#ifndef PI
  PI = acos( -1.0);
#endif

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;
  spudr.eastlon = getview( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR); /**/
  if ( spudr.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;

  /* calculate model volume */
  dlatdlon = ( dlatdeg=180.0/(spudr.nlatR-1) ) deg 
           * ( dlondeg=360.0/(spudr.nlonR-1) ) deg;
  for (volmodel=0.0, xlatdeg=(dlatdeg/2)-90.0; xlatdeg<90; xlatdeg+=dlatdeg) {
    cldll = cos(xlatdeg deg) * dlatdlon / 3.0;
    for ( xlondeg = dlondeg/2; xlondeg<360; xlondeg += dlondeg) {
      /* volume of pyramid whose base is grid square 
       *  = R * R*cos(lat)*dlon * R*dlat / 3 
       */
      volmodel += pow( (double) lltor1( &spudr, xlatdeg, xlondeg), 3.0) * cldll;
    }
  }
  printf( "Radius model volume ~ %lg\n", volmodel);

  rmod2face( &spudr, &spudf);

  xyz2 = (double *) malloc( spudf.nv * 3 * sizeof(double));
  xyzabc2 = (double *) malloc( spudf.nv * 3 * sizeof(double));

  /* find min & max r^2, x^2, y^2, z^2 in model; save x^2, y^2, z^2 */
  maxr2 = minr2 = VDOT( spudf.Rxyz, spudf.Rxyz);
  for ( i=0; i<3; ++i) maxxyz2[i] = spudf.Rxyz[i] * spudf.Rxyz[i];
  for ( iv=0, lxyz2=xyz2, lxyz=spudf.Rxyz; iv<spudf.nv; ++iv) {
    for (r2=i=0; i<3; ++i, ++lxyz2, ++lxyz) {
      r2 += ( *lxyz2 = (*lxyz) * (*lxyz)); 
      if ( maxxyz2[i] < *lxyz2) maxxyz2[i] = *lxyz2;
    }
    if ( maxr2 < r2) maxr2 = r2;
    else if ( minr2 > r2) minr2 = r2;
  }
  /* spheres that enclose/fit inside model */
  maxr6 = pow( maxr2, 3.0);
  minr6 = pow( minr2, 3.0);
  /* lower limits of axes of enclosing ellipse are max xyz */
  /* calculate upper & lower limits */
  for ( abc2prod=1.0, i=0; i<3; ++i) abc2prod *= (loabc2[i]=maxxyz2[i]);
  for ( i=0; i<3; ++i) hiabc2[i] = maxr6 * loabc2[i] / abc2prod;

  /* calculate logarithmic range of values for abc */
  for ( ia=0; ia<NABC; ++ia) for ( i=0; i<3; ++i) {
  double ppp;
    ppp = ia / (NABC - 1.0);
    abc2[i][ia] = pow( loabc2[i], ppp) * pow( hiabc2[i], 1.0-ppp);
  }

#define IA 0
#define IB 1
#define IC 2

  /* find the max value of (x/a)^2+(y/b)^2+(z/c)^2 for all vertices & axes */
  for ( ia=0; ia<NABC; ++ia) {
    for ( lxyz2=xyz2, l2=xyzabc2, iv=0; iv<spudf.nv
        ; ++iv, lxyz2 += 3, l2 += 3) {
      *l2 = *lxyz2 / abc2[IA][ia]; 
    }
    for ( ib=0; ib<NABC; ++ib) {
      for ( lxyz2=xyz2+IB, l2=xyzabc2+IB, iv=0; iv<spudf.nv
          ; ++iv, lxyz2 += 3, l2 += 3) { 
        *l2 = *lxyz2 / abc2[IB][ib]; 
      }
      for ( ic=0; ic<NABC; ++ic) {
        limxyzabc2[ia][ib][ic] = 
          xyzabc2[IA] + xyzabc2[IB] + (xyz2[IC] / abc2[IC][ic]);
        for ( lxyz2=xyz2+IC, l2=xyzabc2+IC, iv=0; iv<spudf.nv
            ; ++iv, lxyz2 += 3, l2 += 3) { 
          *l2 = l2[-2] + l2[-1] + (*lxyz2 / abc2[IC][ic]); 
          if ( limxyzabc2[ia][ib][ic] < *l2) limxyzabc2[ia][ib][ic] = *l2;
        }
      }
    }
  }

  /* correct all ellipses by largest ((x/a)^2+...), choose smallest */

  maxvol2 = maxr6;
  for ( ia=0; ia<NABC; ++ia) {
    for ( ib=0; ib<NABC; ++ib) {
    double ab2;
      ab2 = abc2[IA][ia] * abc2[IB][ib];
      for ( ic=0; ic<NABC; ++ic) {
      double vol2;
        vol2 = ab2 * abc2[IC][ic] * pow( limxyzabc2[ia][ib][ic], 3.0);
        if ( vol2 < maxvol2) {
          maxvol2 = vol2;
          iia = ia;
          iib = ib;
          iic = ic;
        }
      }
    }
  }

#define OUTABC(A) \
  sqrt( abc2[A][(A==IA)?iia:((A==IB)?iib:iic)] * limxyzabc2[iia][iib][iic])
  printf( "Min volume ellipse enclosing shape model has semi-major axes of\n");
  printf( "%g %g %g\n", OUTABC(IA), OUTABC(IB), OUTABC(IC));
  printf( " Volume = %g\n", 4.0*PI*OUTABC(IA) * OUTABC(IB) * OUTABC(IC) / 3.0);
  return(0);
}
