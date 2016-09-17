/* spudtest.c - read in a spud shape model and test some things about it */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

/*
#ifndef DEBUG
#define DEBUG
#endif
/**/
#include "debug.h"
#include "orbit3d.h"
#include "spudshap.h"

int main( argc, argv) unsigned int argc; char **argv; {
SPUDR		spudr;
SPUDF		spudf;
unsigned long ip;
double *vtx, *mp;
VEC r;
double r2;
double maxr2=0.0;
double minr2=1e10;
double sumr, countr;
unsigned long *fa;
long *foidx;
double rnd[3], rndsum;
VEC sc, ubs;
unsigned long intplate; 
double dist;
unsigned int ttt, rrr;

double c0, c1, dt, rate;
unsigned long bad=0;

  ttt = 655357; /* time( &ttt); /**/
  rrr = (unsigned int) (ttt|1);
  srand(rrr);

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;
  spudr.eastlon = getview( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR); /**/
  if ( spudr.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;

  rmod2face( &spudr, &spudf);

#define CALC_old1(S,I) \
  /* test r2 calc */ \
  vtx = (spudf.Rxyz + (3*(S))); \
  VMINUS2( vtx, mp, r); \
  r2 = VDOT(r,r)/ spudf.r2[ip]; \
  if ( r2 > maxr2) maxr2 = r2; \
  if ( r2 < minr2) minr2 = r2; \
  sumr += r2; \
  countr++; \
  DPR(( stderr, "vertex = %9.2lf %9.2lf %9.2lf", vtx[0], vtx[1], vtx[2])); \
  DPR(( stderr, " ip=%5ld:  r = %9.2lf\n", ip, sqrt(r2) )); \
  /* test spudf_intersect() */ \
  vmxpb( 10*rnd[I], vtx, sc, sc) /* put s/c at 10 radii out */

#define CALC(S,I) \
  vtx = (spudf.Rxyz + (3*(S))); \
  /* test spudf_intersect() */ \
  vmxpb( 10*rnd[I], vtx, sc, sc) /* put s/c at 10 radii out */

  mp = spudf.midpts;
  fa=spudf.faceapices;
  foidx = spudf.faceoeidx;

  c0 = (double) clock();

  for ( sumr=countr=ip=0; ip<spudf.nface; ++ip, ++fa, ++foidx, mp+=3) {
    sc[0] = sc[1] = sc[2] = 0.0;
    rndsum = rnd[0] = ((double) rand()) / RAND_MAX;
    rndsum += (rnd[1] = ((double) rand()) / RAND_MAX);
    rndsum += (rnd[2] = ((double) rand()) / RAND_MAX);
    rnd[0] /= rndsum; 
    rnd[1] /= rndsum; 
    rnd[2] /= rndsum; 
    CALC( *fa, 0);
    CALC( spudf.oe[ *foidx], 1);
    CALC( spudf.oe[ (*foidx) + 1], 2);

    DPR(( stderr, " midpt = %9.2lf %9.2lf %9.2lf\n", *mp, mp[1], mp[2]));

    rndsum = -1.0 / VLEN( sc);                            /* get s/c distance */
    VSCAL2( rndsum, sc, ubs);   /* unit vector boresight at model center */
    spudf_intersect( &spudf, sc, ubs, &dist, &intplate);

    /* test for bad intersection */

    if ( intplate != ip || fabs(dist+(0.9/rndsum)) > (-1e6/rndsum)) {
    unsigned long intplate2;
    double dist2;
      ++bad;
      printf( "error%6ld=ip %.4lf %.4lf %.4lf=sc"
            , ip, sc[0], sc[1], sc[2]);
      if ( intplate != spudf.nface || dist != 0.0) {
        printf( "\n  result/err:  %ld/%ld=plate %.2lf/%.2lf=dist; shifting"
              , intplate, intplate-ip, dist, dist+(0.9/rndsum));
      } else {
        printf( "; shifting");
      }

      /* shift weights & try again for debug */
      CALC( *fa, 1);
      CALC( spudf.oe[ *foidx], 2);
      CALC( spudf.oe[ (*foidx) + 1], 0);
      rndsum = -1.0 / VLEN( sc);
      VSCAL2( rndsum, sc, ubs);
      spudf_intersect( &spudf, sc, ubs, &dist2, &intplate2);
      if ( intplate2 != ip || fabs(dist2+(0.9/rndsum)) > (-1e6/rndsum)) {
        bad++;
        printf( "\n ... error on shift:  %ld/%ld=plate %.2lf/%.2lf=dist\n"
              , intplate, intplate-ip, dist, dist2+(0.9/rndsum));
      } else {
        printf( " ... ok\n");
      }
    }
  }
  c1 = (double) clock();
  dt = (c1 - c0) / CLOCKS_PER_SEC;
  rate = (spudf.nface+bad) / dt;

  printf( "dT=%.2lf s;  rate:  %.0lf ints/s; %.2lf bad/1000.  # facets = %ld\n"
        , dt, rate, (1000.0*bad)/spudf.nface, spudf.nface);
/*
  printf( "min/max/avg r2 ratio = %lf %lf %lf  # facets = %lf\n"
        , minr2, maxr2, sumr/countr, countr/3, spudf.nface);
/**/
  return 0;
}
