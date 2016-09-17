#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#include "spudshap.h"

#define deg		*3.14159265358979323846/180.
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

double seknds();

#define VR spudv->_VR
#define SR spudv->_SR

int main( argc, argv) unsigned int argc; char **argv; {
SPUDR spudr;
SPUDF spudf;
SPUDV *spudv;
double R1[4][4], dR[4][4];
double ang;
double t0, t1, ttot, tsubtot;
int i, j, k;
unsigned int rrr;
time_t ttt;
double inpScale = 0.0;
double maxR, r;
double *dPtr;

extern double sv0StatPtsArr[10], sv0StatPlates;
extern double sv1StatPtsArr[10], sv1StatPlates;

  sv0StatPlates = sv0StatPlates =  0;
  for ( i=0; i<10; ++i) { sv0StatPtsArr[i] = sv0StatPtsArr[i] = 0; }

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

  spudv = newspudv( &spudf);
  spudv->_hidden = 1;
  spudv->_hidden = SPUDV_SHADHID;

  dPtr = spudf.Rxyz;                                /* max radius */
  for ( maxR=j=0; j<spudf.nv; ++j, dPtr+=3) {
    r = VDOT(dPtr,dPtr);
    if ( r > maxR) maxR = r;
  }
  maxR = sqrt(maxR);

  if ( argc == 2) {                            /* s/c range = max rad * scale */
    sscanf( argv[1], "%lf", &inpScale);
  }

for ( tsubtot=ttot=j=0; j< ((inpScale>0) ? 100 : 100); ) {

  IDENT3D( R1);
  R1[0][0] = R1[1][1] = 0.;
  R1[1][0] = -1.;
  R1[0][1] = 1.;

#define randrange(LO,HI) ((LO) + ((HI)-(LO))*(((double)rand())/RAND_MAX))

  ang = randrange(0.,360.);         /* sub-s/c west longitude */
  ROTANG( R1, dR, VR, ang, 2)
  /* printf( "%6.2f", ang); /**/
  ang = randrange( -90., 90.);    /* sub-s/c latitude */
  ROTANG( VR, dR, R1, ang, 0)
  /* printf( "%7.2f", ang); /**/
  ang = randrange( 0., 360.);       /* Noraz */
  ROTANG( R1, dR, VR, ang, 1)
  /* printf( "%7.2f", ang);
  /* printf( " %d %d =theta/phi/Noraz/faces/vert\n", spudf.nface, spudf.nv);
  /**/

  ang = randrange(0.,360.);
  ROTANG( VR, dR, R1, ang, 2)
  /* printf( "%6.2f", ang); /**/
  ang = randrange( -90., 90.);
  ROTANG( R1, dR, SR, ang, 0)
  /* printf( "%7.2f", ang);
  /* printf( " =theta/phiSunwrtSC\n");
  /**/

  spudv->_range = inpScale * maxR;

  VSCAL2( (((j/10) % 2) ?0 :-spudv->_range), VR[1], spudv->_scAbf);

  t0 = seknds( 0.0);
  spudviewPersp( spudv);
  t1 = seknds( t0);
  ttot += t1;
  tsubtot += t1;

  /* printf( " %9.3lf =spudview time\n", t1); /**/
  fprintf( stdout, "."), fflush( stdout);

  if ( !(++j % 10)) {
    printf( " %8.2lf %8.2lf s = spudview 10- & %3d-point avg time\n"
          , tsubtot/10, ttot/j, j);
    tsubtot = 0.0;
  }

} /* for j */

# define PRT(SVPTSARR,SVPLATES,DESCR) \
  printf( "%s:  %lf plates\n", DESCR, SVPLATES); \
  for ( j=0; (SVPTSARR[j]>0) && (j<10); ++j) { \
    printf( "  %6d:  %10lf points => %lf points/plate\n" \
          , j \
          , SVPTSARR[j] \
          , ((double)SVPTSARR[j])/SVPLATES); \
  }

  PRT( sv0StatPtsArr, sv0StatPlates, "Orthogonal ")
  PRT( sv1StatPtsArr, sv1StatPlates, "Perspective")

  return(0);
}
