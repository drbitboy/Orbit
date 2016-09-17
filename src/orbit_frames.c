#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#include "orbit3d.h"
#include "pointing.h"
#include "orbit_cas.h"


#include "debug.h"

#define px pt[ix]
#define py pt[iy]
#define v1x vtx1[ix]
#define v1y vtx1[iy]
#define v2x vtx2[ix]
#define v2y vtx2[iy]

double orbit_fov_calcScanOverlap( IMGFRM *, IMGFRM *, IMGFRM *, IMGFRM *);

/**********************************************************************/
/* find if a point is inside a polygon
 *
 * INPUTS
 * pt      is the point to test
 * poly[]  contains the vertices of the polygon
 * npoly   is the number of vertices (& sides)
 * ix      is the index into the vector of the "x" values
 * iy      is the index into the vector of the "y" values
 *
 * OUTPUTS
 * <NONE>
 *
 * RETURN
 * 1       if pt is inside the polygon
 * 0       if pt is NOT inside the polygon
 */

static int 
inside_lcl( VEC pt, VEC poly[], int npoly, int ix, int iy) {
double *vtx1, *vtx2;
int ipoly;
double frac, pxe;
int islefts;
double maxx, minx, maxy, miny;

  /* step through sides connecting vertices of polygon
   * - increment islefts for each time a +x vector from pt crosses a side of
   *     the polygon
   * - return(1) if pt is ON the side of the polygon
   * - don't count a crossing if it is at the top of a vector so only one
   *     side is counted as crossed if pt is even with a vertex
   */

  vtx2 = poly[0];
  islefts = 0;

  for ( ipoly = 1; ipoly <= npoly; ipoly++) {
    vtx1 = vtx2;
    vtx2 = poly[ipoly%npoly];

    if ( v1y < v2y) { maxy = v2y; miny = v1y; }   /* find side's min/max y */
    else { maxy = v1y; miny = v2y; }

    if ( py > maxy ) ;                            /* pt above side */
    else if ( py < miny) ;                        /* below */
    else {

      if ( v1x < v2x) { maxx = v2x; minx = v1x; } /* find side's min/max x */
      else { maxx = v1x; minx = v2x; }

      if ( px > maxx) ;                             /* right */
      else if ( px < minx && py < maxy) islefts++;  /* left of side, below top*/
      else if ( px < minx) ;                        /* left of side, at top */
      else if ( v2y == v1y) return(1);              /* on horizontal side */
      else {
        frac = (py-v1y) / (v2y - v1y);  /* frac of side where pt is vertically*/
        pxe = frac * (v2x - v1x) + v1x; /* x value of side even with pt */
        if ( px == pxe) return(1);      /* pt on side */
        else if ( px < pxe) islefts++;  /* pt to left of side */
      }
    }
  } /* for (ipoly */
  return( islefts % 2); /* return 0 for even number of crossings, 1 for odd */
}
#undef py
#undef px

/**********************************************************************/
/* find x values where a horizontal line crosses a polygon
 * ***N.B. assumes v{1/2}{x/y} macros still defined
 *
 * INPUTS
 * py      is the vertical value of the horizontal line
 * poly[]  contains the vertices of the polygon
 * npoly   is the number of vertices (& sides)
 * ix      is the index into the vector of the "x" values
 * iy      is the index into the vector of the "y" values
 * polyid  is an id to identify this polygon
 *
 * OUTPUTS
 * *crossx is where to put the x values of the crossings
 * *icrossx is where to store polyid for each crossing
 *
 * RETURN
 * number of crossings
 */

static int 
findXCrossings( double py, VEC poly[], int npoly, int ix, int iy, int polyid 
              , double *crossx, int *icrossx) {

double *vtx1, *vtx2;
int ipoly;
double frac, pxe;
int ncrossings;
double maxx, minx, maxy, miny;

  /* step through sides connecting vertices of polygon
   * - don't count a crossing if it is at the top of a vector so only one
   *     side is counted as crossed if pt is even with a vertex
   */

  vtx2 = poly[0];
  ncrossings = 0;

  for ( ipoly = 1; ipoly <= npoly; ipoly++) {
    vtx1 = vtx2;
    vtx2 = poly[ipoly%npoly];

    if ( v1y < v2y) { maxy = v2y; miny = v1y; }   /* find side's min/max y */
    else { maxy = v1y; miny = v2y; }

    if ( py >= maxy ) ;                           /* pt above side */
    else if ( py < miny) ;                        /* below */
    else {
      frac = (py-v1y) / (v2y - v1y);  /* frac of side where py is vertically*/
      *crossx = frac * (v2x - v1x) + v1x; /* x value of side even with py */
      *icrossx = polyid;                  /* save polygon id */
      ncrossings++; crossx++; icrossx++;  /* increment counter & pointers */
    }
  } /* for (ipoly */
  return( ncrossings);
}

/**********************************************************************/
/* calculate area overlap between two frames - per Jim Bell request
 * - choose 100 points in new frame
 * - find out how many of those are inside the old frame
 * - that number is the percentage overlap; that fraction is returned
 * - if no points are inside the old frame, return -1.0
 * - no perspective - i.e. infinitely narrow angle camera
 */

double calcJimboLinesOverlap( SC *inpSc, IMGFRM *oldImgfrm, IMGFRM *newImgfrm) {
int ioverlap;
VEC oldpts[4], newpts[5];
int icampt, inewcampts, ioldcampts;
VEC nulVec = { 0., 0., 0.};
VEC v0, v1, v2, v3, v01, v32, v0132;
VEC dv01, dv32, dv0132;
int i01, i0132;
double x01, x0132;
#define LINES 20
#define LINESM1 (LINES-1)
double sumlens[4];
double crossx[8], crossxptr;
int icrossx[8], icrossxptr;
double loz, delz, zzz, lastx;
int iline, i, istate, icross, ncross, ilast, ilo;
 
  /* convert first 3 or 4 image frame vectors to camera coordinates
   * - find high and low limits of frames
   */ 

  ioldcampts = (oldImgfrm->_ncampts > 3) ? 4 : 3;
  inewcampts = (newImgfrm->_ncampts > 3) ? 4 : 3;

  for ( icampt = 0; icampt < ioldcampts; icampt++) {
    vxm( oldImgfrm->vec3+(3*icampt), inpSc->_mabftocam, oldpts[icampt]);
    zzz = oldpts[icampt][2];
    if ( !icampt) delz = loz = zzz;
    if (zzz < loz) loz = zzz;
    else if ( zzz > delz) delz = zzz;
  }
  for ( icampt = 0; icampt < inewcampts; icampt++) {
    vxm( newImgfrm->vec3+(3*icampt), inpSc->_mabftocam , newpts[icampt]);
    zzz = newpts[icampt][2];
    if (zzz < loz) loz = zzz;
    else if ( zzz > delz) delz = zzz;
  }
  delz -= loz;

  for ( i=0; i<4; i++) sumlens[i] = 0.0;

  /* break horizontal lines where they cross the sides of the frame polygons;
   * add lengths of line segments to sumlens[i] according to which frame(s)
   * the segments are "inside":
   *   sumlens[0] => outside both frames
   *   sumlens[1] => inside new frame only
   *   sumlens[2] => inside old frame only
   *   sumlens[3] => inside both frames
   */

#define NEWPOLYID (1<<0)
#define OLDPOLYID (1<<1)

  for ( iline=0; iline<LINES; iline++) {
    zzz = loz + ((iline + 0.5) * delz / LINES);
    ncross = 0;
    ncross += findXCrossings( zzz, newpts, inewcampts, 0, 2, NEWPOLYID
                            , crossx, icrossx);
    ncross += findXCrossings( zzz, oldpts, ioldcampts, 0, 2, OLDPOLYID
                            , crossx+ncross, icrossx+ncross);
    if ( ncross) {
      /* find least crossing value 
       * - save its index in ilo
       * - subtract value from sumlens[0] to offset first step of algorithm
       */
      ilo = icross = ncross;
      while ( --icross >= 0) if ( crossx[icross] < crossx[ilo]) ilo = icross;
      sumlens[0] -= lastx;

      /* step through crossings, least- to next-to-greatest x
       * - add crossing value to current state's sum (sumlnes[istate])
       * - modify state based on crossing's poly id
       * - subtract crossing value to new state's sum
       * - remove least crossing value for next step
       */
      istate = 0;
      while ( --ncross) {

        /* find least crossing value - save its index in ilo */
        ilo = icross = ncross;
        while ( --icross >= 0) if ( crossx[icross] < crossx[ilo]) ilo = icross;

        /* add least value to state, modify state, subtract from new state */
        sumlens[istate] += crossx[ilo];
        istate ^= icrossx[ilo];
        sumlens[istate] -= crossx[ilo];

        /* if this value is not the last one in crossx i.e. if crossx[ncross]
         * was not the lowest in this step, replace the lowest value with the
         * last value
         */
        if ( ilo != ncross) {
           crossx[ilo] = crossx[ncross];
           icrossx[ilo] = icrossx[ncross];
        }
      }

      /* handle greates crossing */
      sumlens[istate] += crossx[0];

      /* sanity check */
      istate ^= icrossx[0];
      if ( istate)
        fprintf( stderr 
        , "***Programming error; contact programmer, code %s, data %d\n"
        , "calcJimboLinesOverlap()", istate);

    } /* if ( ncross) */
  }

  return( (sumlens[3] > 0.0 && sumlens[1] > 0.0) 
          ? (sumlens[3] / sumlens[1])
          : -1.0 );
}

/**********************************************************************/
/* calculate area overlap between two frames - per Jim Bell request
 * - choose 100 points in new frame
 * - find out how many of those are inside the old frame
 * - that number is the percentage overlap; that fraction is returned
 * - if no points are inside the old frame, return -1.0
 * - no perspective - i.e. infinitely narrow angle camera
 */

double calcJimboOverlap ( SC *inpSc, IMGFRM *oldImgfrm, IMGFRM *newImgfrm) {
int ioverlap;
VEC oldpts[4], newpts[5];
int icampt, inewcampts, ioldcampts;
VEC nulVec = { 0., 0., 0.};
VEC v0, v1, v2, v3, v01, v32, v0132;
VEC dv01, dv32, dv0132;
int i01, i0132;
double x01, x0132;
#define SIDE 10
#define SIDEM1 (SIDE-1)
#define AREA (SIDE*SIDE)
double lens[SIDE], sumlens;
int istart, idel, ileft;
static double lofrac = 0.1/SIDE;
static double hifrac = 1.0 - (0.1/SIDE);
 
  /* convert first 3 or 4 image frame vectors to camera coordinates
   * - zero out the Y value for newImgfrm
   * - if newImgfrm is a triangle, then the first and fourth point are the same
   */ 

  ioldcampts = (oldImgfrm->_ncampts > 3) ? 4 : 3;
  inewcampts = (newImgfrm->_ncampts > 3) ? 4 : 3;

  for ( icampt = 0; icampt < ioldcampts; icampt++) {
    vxm( oldImgfrm->vec3+(3*icampt), inpSc->_mabftocam, oldpts[icampt]);
  }
  for ( icampt = 0; icampt < 4; icampt++) {
    vxm( newImgfrm->vec3+(3*(icampt%inewcampts)), inpSc->_mabftocam
       , newpts[icampt]);
    newpts[icampt][1] = 0.0;
  }

  /* select 100 points somewhat evenly distributed around the new imgfrm */
  /* - first select points in a fraction from each side */

  VMINUS2( newpts[1], newpts[0], dv01);
  VMINUS2( newpts[2], newpts[3], dv32);

  vmxpb( lofrac, dv01, newpts[0], v0);
  vmxpb( lofrac, dv32, newpts[3], v3);
  VMINUS2( v3, v0, dv0132);
  vmxpb( hifrac, dv0132, v0, v3); /* hifrac first because v0 replaced next */
  vmxpb( lofrac, dv0132, v0, v0);

  vmxpb( hifrac, dv01, newpts[0], v1);
  vmxpb( hifrac, dv32, newpts[3], v2);
  VMINUS2( v2, v1, dv0132);
  vmxpb( hifrac, dv0132, v1, v2); /* hifrac first because v1 replaced next */
  vmxpb( lofrac, dv0132, v1, v1);

  VMINUS2( v1, v0, dv01);
  VMINUS2( v2, v3, dv32);

  /* find lengths between each station along top (0-1) and bottom (3-2) sides */
  for ( sumlens=0.0, i01=0; i01<SIDE; i01++) {
    x01 = i01 / (double) SIDEM1;
    vmxpb( x01, dv01, v0, v01);
    vmxpb( x01, dv32, v3, v32);
    VMINUS2( v32, v01, dv0132);
    lens[i01] = VLEN(dv0132);
    sumlens += lens[i01];
  }

  ioverlap = 0;
  istart = (lens[0] < lens[SIDEM1]) ? 0 : SIDEM1;
  idel = istart ? -1 : 1;
  for ( ileft= (AREA-SIDE); istart < SIDE && istart >= 0; istart += idel) {
  int n, idx;
  double xn;

    n = (int) (lens[istart] * ileft / sumlens + 0.5);
    if ( n < 1) n = 1;
    ileft -= n;
    sumlens -= lens[istart];
    xn = (double) n;

    x01 = istart / (double) SIDEM1;
    vmxpb( x01, dv01, v0, v01);
    vmxpb( x01, dv32, v3, v32);
    VMINUS2( v32, v01, dv0132);
    for ( idx=0; idx<=n; idx++) {
      vmxpb( idx/xn, dv0132, v01, v0132);
      ioverlap += inside_lcl( v0132, oldpts, ioldcampts, 0, 2);
    }
  }

  return( ioverlap / (double) AREA);
}

/**********************************************************************/
/* calculate overlap between two frames
 * 1) assume same rotational orientation between frames 
 *    (sometimes a bad assumption, but we'll live with it)
 * 2) use distance between projection of boresight points as 
 *    basis for overlap calculation per A. P. Harch request
 *    - scale distance based on azimuth of old frame wrt new
 *    - scale distance for old frame normal not parallel to new
 * 3) camera coordinates:  +sample is x, boresight is y, up is z
 * 4) no perspective - i.e. infinitely narrow angle camera
 */

double calcAnnboOverlap ( SC *inpSc, IMGFRM *oldImgfrm, IMGFRM *newImgfrm) {
double dist, zerolap, newZerolap, oldZerolap, oldFactor, overlap
     , sinAzimuth, cosAzimuth, tanAzimuth, k2, fovX2, fovY2, range;
VEC oldBore, newBore, oldNorm, azimuthVec, newBoretosc;

#define fovX inpSc->_fovx
#define fovY inpSc->_fovy

  /* convert image frame vectors to camera coordinates */
  vxm( oldImgfrm->_boreVec, inpSc->_mabftocam, oldBore);
  vxm( newImgfrm->_boreVec, inpSc->_mabftocam, newBore);
  vxm( oldImgfrm->_normVec, inpSc->_mabftocam, oldNorm);
  vxm( newImgfrm->_boretoscVec, inpSc->_mabftocam, newBoretosc);

  /* return overlap = 0 if the old normal is facing away from the s/c */
/*  if ( oldNorm[1] >= 0.0) { */
/*  if ( VDOT( oldNorm, newBoretosc) <= 0.0) {
/*    overlap = 0.0;
/*    return( overlap);
/*  }
/**/

  /* get vector between boresights, distance - if distance = 0, overlap = 1 */
  VMINUS2( oldBore, newBore, azimuthVec);
  dist = sqrt( (azimuthVec[0] * azimuthVec[0]) 
             + (azimuthVec[2] * azimuthVec[2]) );
  if ( dist == 0.0) {
    overlap = 1.0;
    return( overlap);
  }

  /* calculate zerolap = distance betw newBore & oldBore at which overlap = 0
   * - newZerolap is contribution by new frame
   * - oldZerolap is contribution by old frame, similar to newZerolap, but
   *     modified by old frame normal not being the same as the new frame
   */

  /* determine azimuth from new to old boresight, sign is unimportant */
  if ( azimuthVec[0] != 0.0) tanAzimuth = azimuthVec[2] / azimuthVec[0];
  else tanAzimuth = 1e10;

  /* newZerolap - intersection of azimuth with ellipse that fits in frame */
  k2 = tanAzimuth * tanAzimuth;
  fovX2 = fovX * fovX / 4.0;
  fovY2 = fovY * fovY / 4.0;
  newZerolap = sqrt( (1.0 + k2) / ((1.0 / fovX2) + (k2 / fovY2)) );
  range = VLEN( newBoretosc);
  newZerolap *= range; /* convert from radians to km */

  /* oldZerolap - scale newZerolap by cosine of angle who's sine is the
   * projection of the old normal onto azimuth
   * - assumes old normal is unit vector
   */
  azimuthVec[0] /= dist;
  azimuthVec[1] = 0.0;
  azimuthVec[2] /= dist;
  oldFactor = VDOT( azimuthVec, oldNorm);
  oldFactor = sqrt( 1.0 - oldFactor * oldFactor);
  oldZerolap = newZerolap * oldFactor;

  zerolap = newZerolap + oldZerolap;

  overlap = (zerolap - dist) / zerolap;

  return( overlap);
}

/**********************************************************************/
/* create list of generated frames */

IMGFRM *
gen_frames( ORBIT *inpOrbit) {
double etLast, etNext, etStart = inpOrbit->_agen._EtStart;
double etEnd;
long etDelta, etDeltaLo, etDeltaHi;
double orbitStart, orbitEnd;
IMGFRM *imgfrm1, *imgfrm2, *oldif;
SC *lclsc = (SC *) inpOrbit->_sc;
POINTING *lclBore, *lclRoll;
double overlap, loTargetOverlap, hiTargetOverlap, loOverlap, hiOverlap;
double tmpOL;
long overlapType, typeBits;
char *instrName;

  /* set initial delta for overlap solution */
  etDelta = 600;

  /* get overlap & overlap type, do sanity check */
  if ( inpOrbit->_agen._FrmOverlap > inpOrbit->_agen._FrmOverlap2) {
    hiTargetOverlap = inpOrbit->_agen._FrmOverlap;
    loTargetOverlap = inpOrbit->_agen._FrmOverlap2;
  } else {
    hiTargetOverlap = inpOrbit->_agen._FrmOverlap2;
    loTargetOverlap = inpOrbit->_agen._FrmOverlap;
  }
  typeBits = inpOrbit->_agen._FrmOverlapTypeBits;
  for ( overlapType=OVERLAP_TYPE_FIRST_BIT;
        overlapType < OVERLAP_TYPE_LAST_BIT && !(typeBits>>overlapType & 1L);
        overlapType++) ;

/*
#define IFLO(A,B) if (loTargetOverlap A (B)) loTargetOverlap = (B)
#define IFHI(A,B) if (hiTargetOverlap A (B)) loTargetOverlap = (B)
*/
#define IF01LOHI(B) IF01(B,loTargetOverlap); IF01(B,hiTargetOverlap)
#define IF01(B,C) IFBASE(<,B,C); else IFBASE(>,1.0-(B),C)
#define IFLOHI(A,B) IFBASE(A,B,loTargetOverlap); IFBASE(A,B,hiTargetOverlap)
#define IFLO(A,B) IFBASE(A,B,loTargetOverlap)
#define IFHI(A,B) IFBASE(A,B,hiTargetOverlap)
#define IFBASE(A,B,C) if (C A (B)) C = (B)

  switch ( overlapType) {
  case( OVERLAP_TYPE_ANN_BIT):     IFLOHI( >, 0.999);               break;
  case( OVERLAP_TYPE_JIM_BIT):     IF01LOHI( 0.5 / AREA);           break;
  case( OVERLAP_TYPE_FOV100_BIT):  IF01LOHI( 0.5 / MAXFOV100PTS); break;
  case( OVERLAP_TYPE_JIM_NEW_BIT): IF01LOHI( 0.001);                break;
  default:
    fprintf( stderr
           , "***Programming error; contact programmer, code %s, data %ld/%ld\n"
           , "WSNBATGH:gen_frames():001\n", overlapType, typeBits);
    exit(1);
  }
/*
  fprintf( stderr, "%15lf %15lf\n", loTargetOverlap, hiTargetOverlap);
  fflush(stderr);
/**/

  /* get the instrument name */
  instrName = orbit_set_instrument( lclsc, lclsc->_instrument);

  /* get bore and roll pointing structures for orbitgui.c */
  orbitgui_return_boreroll( lclsc, &lclBore, &lclRoll);

  /* solve for starting imgfrm i.e. at time = etStart */
  etLast = etNext = etStart;
  if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll)) 
    return( (IMGFRM *) 0);

# define LCLSETLAPBIT \
  if ( overlapType == OVERLAP_TYPE_FOV100_BIT) { DOFOV100_SETOVERLAP( lclsc); }
# define LCLCLRLAPBIT \
  if ( overlapType == OVERLAP_TYPE_FOV100_BIT) { DOFOV100_CLROVERLAP( lclsc); }

  LCLSETLAPBIT
  imgfrm1 = loadImageFrame( lclsc, etNext, (IMGFRM *) 0);
  LCLCLRLAPBIT

  if ( !imgfrm1) return( imgfrm1);

  /* start building links within this list */
  imgfrm1->previf = imgfrm1;  /* *imgfrm1 is start of list, AND for now it is */
                              /*   also the end of the list */
  imgfrm2 = imgfrm1;          /* *imgfrm2 is last complete IMGFRM on list */

  copyInstrument_ScToImgfrm( lclsc, imgfrm2);

  /* set up finishing criteria:  max Time or max Orbits */
  etEnd = etStart + inpOrbit->_agen._MaxTime;

  orbitStart = imgfrm1->_orbitnum;
  orbitEnd = orbitStart + inpOrbit->_agen._MaxOrbits;

  /* - initialize:  hi & lo overlap to average of target overlap limits, 
   * - test:  generate frames until finished criteria (time or orbit) met 
   * - after each pass:  reset to hi & lo target overlaps
   */
  for ( loOverlap = hiOverlap = (loTargetOverlap+hiTargetOverlap) / 2.0;
        ( (inpOrbit->_agen._MaxTime > 0.0 && etNext < etEnd) || 
          (inpOrbit->_agen._MaxTime <= 0.0)
        ) &&
        ( (inpOrbit->_agen._MaxOrbits > 0.0 && imgfrm2->_orbitnum < orbitEnd &&
           imgfrm2->_orbitnum != -999.0) ||
          (imgfrm2->_orbitnum == -999.0) ||
          (inpOrbit->_agen._MaxOrbits <= 0.0)
        );
        (loOverlap = loTargetOverlap), hiOverlap = hiTargetOverlap
      ) {

    /* setup first test to fail & 
     *   force new frame calc using current etDelta 
     */
    etDeltaLo = 0;
    etDeltaHi = etDelta;
    overlap = 1.0;

    /* loop 1:  double etDelta until overlap is below target */

    while ( overlap > hiOverlap) {
      etDelta = etDeltaHi;
      etNext = etLast + etDelta; 
      if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
        return( imgfrm1);

      oldif = imgfrm2->nextif; /* save pointer in case loadImageFrame fails */
      LCLSETLAPBIT
      imgfrm2->nextif = loadImageFrame( lclsc, etNext, oldif);
      LCLCLRLAPBIT

      /* if loadImageFrame failed, free last IMGFRM struct, and return */
      if ( !imgfrm2->nextif) {
        freeImageFrame( oldif);
        return( imgfrm1);
      }
#define calcOverlap(SC,OLD,NEW,TYPE,OL) \
switch( TYPE) { \
case(OVERLAP_TYPE_ANN_BIT): \
  OL=calcAnnboOverlap( SC, OLD, NEW);             break; \
case(OVERLAP_TYPE_JIM_BIT): \
  OL=calcJimboOverlap( SC, OLD, NEW);             break; \
case(OVERLAP_TYPE_FOV100_BIT): \
  OL=orbit_fov_calcScanOverlap( OLD,OLD,NEW,NEW); break; \
case(OVERLAP_TYPE_JIM_NEW_BIT): \
  OL=calcJimboLinesOverlap( SC, OLD, NEW);        break;\
} /* fprintf( stderr, "%15lf\n", OL); fflush(stderr); /**/

      /* calculate overlap */
      calcOverlap( lclsc, imgfrm2, imgfrm2->nextif, overlapType, overlap)

      /* if overlap is still above hi end of target range:
       * - increase time 
       * - reset hi limit = lo limit so we will converge on lo limit 
       */
      if ( overlap > hiOverlap) {
        etDeltaLo = etDeltaHi;
        etDeltaHi *= 2;
        hiOverlap = loOverlap;
      }
    } /* end loop 1 */

    /* loop 2:  binary search of etDelta to the nearest second 
     *          to get overlap close to target overlap
     */

    while ( (overlap < loOverlap || overlap > hiOverlap) && 
            (etDeltaHi-etDeltaLo) > 1 ) {

      /* the first time through, if we get here, then we have exceeded the
       * lower limit (loOverlap) because loop 1 ensures we are not past 
       * the upper limit at that point, so reset the lo limit = the hi limit 
       * i.e. if we exceed the lo limit, zero in on the hi limit
       */
      loOverlap = hiOverlap;

      etDelta = (1+etDeltaHi+etDeltaLo) / 2;
      etNext = etLast + etDelta; 
      if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
        return( imgfrm1);
      oldif = imgfrm2->nextif; /* save pointer in case loadImageFrame fails */
      LCLSETLAPBIT
      imgfrm2->nextif = loadImageFrame( lclsc, etNext, oldif);
      LCLCLRLAPBIT

      /* if loadImageFrame failed, free last IMGFRM struct, and return */
      if ( !imgfrm2->nextif) {
        freeImageFrame( oldif);
        return( imgfrm1);
      }

      /* calculate overlap */
      calcOverlap( lclsc, imgfrm2, imgfrm2->nextif, overlapType, overlap)

      /* do binary search */
      if ( overlap > hiOverlap) {
        etDeltaLo = etDelta;
      } else {
        if ( overlap < loOverlap) {
          etDeltaHi = etDelta;
        } else {
          etDeltaHi = ( etDeltaLo = etDelta);
        }
      }

    } /* end loop 2 */

    etLast = etNext;

    /* maintain links in list */
    imgfrm2->nextif->previf = imgfrm2;
    imgfrm2 = imgfrm2->nextif;
    imgfrm1->previf = imgfrm2;

    imgfrm2->_etDelta = etDelta;
    imgfrm2->_Overlap = overlap;
    copyInstrument_ScToImgfrm( lclsc, imgfrm2);

  } /* time or orbit loop */

  return( imgfrm1);
} /* gen_frames( ORBIT *inpOrbit) */

/*****************************************************/

/* save frame info into imgfrm */
#define IMGFRMSET \
  switch ( lclsc->_instrument) { \
  case SC_NIS: \
  case SC_NIS2: \
    imgfrmN->_nisDuration = seqDef->nisSecPerObs; \
    imgfrmN->_nisSpecnum = numSpectra; \
    imgfrmN->_nisScannum = numScan; \
    imgfrmN->_nisDarkFollows = 0; \
    break; \
  default: \
    break; \
  } \
  copyInstrument_ScToImgfrm( lclsc, imgfrmN)

/**********************************************************************/
/* create list of generated frames for a single sequence definition
 *
 * RETURN VALUE: 
 *
 *   (IMGFRM *) of resultant list (may equal inpImgfrm, see below)
 *
 * INPUTS:  
 *
 *   inpOrbit
 *     ->_agen._EtStart       starting time, s past J2k
 *            ._seqDef        sequence definition to use
 *
 *   - if ._seqDef is NISSEQDEF:
 *            ._StartingStep  first mirror position
 *            ._NISFOV        Aperture (1 for narrow, else wide)
 *
 *   inpImgfrm                if non-null, (IMGFRM *) of start of 
 *                            existing IMGFRM list to fill &/or expand
 *
 * OUTPUTS:
 *
 *    *framesLeft  = number of frames that were not taken because of
 *                   an abnormal exit from this routine;
 *                 = 0 on success;
 *                 = -1 for bad input (unknown inpOrbit->_agen.seqDef->_type).
 */
IMGFRM *
orbit_gen_anyInstrFrames( ORBIT *inpOrbit, IMGFRM *inpImgfrm, long *framesLeft){
double etNext, etEnd;
double orbitStart, orbitEnd;
IMGFRM *imgfrm1, *imgfrmN, *saveImgfrm;
IMGFRM **nextImgfrmPtr;
SC *lclsc = (SC *) inpOrbit->_sc;
POINTING *lclBore, *lclRoll;
long numSpectra;
long numScan = 0;                    /* number of times scan mirror has reset */
long iObs, iScans;
long delTPerObs, delTAtScanEnd;
long instrNumObs, instrNumScans;
char *instrName;
CAS *seqDef = (CAS *) inpOrbit->_agen._seqDef;

  imgfrm1 = (IMGFRM *) 0;
  numSpectra = 0;              /* set number of frames successfully generated */
  *framesLeft = -1;                                         /* assume failure */

  if ( !seqDef || !inpOrbit) return;

  /* copy info from inpOrbit->_agen & seqDef into lclsc, delT*, & framesLeft
   * and get number of observations (instrNumObs) and number of "scans" 
   * comprising instrNumObs observations
   */
  switch (seqDef->_type) {
  case CASTYPE_MSISEQDEF:
    lclsc->_instrument = SC_MSI;
    instrNumObs = seqDef->msiSeqDefNumImages;
    instrNumScans = 1;
    delTPerObs = seqDef->msiSeqDefInterval;
    delTAtScanEnd = 0;   /* not used since instrNumScans = 1 */
    break;
  case CASTYPE_NISSEQDEF:
    lclsc->_nisStepAct = inpOrbit->_agen._StartingStep;
    lclsc->_instrument = (inpOrbit->_agen._NISFOV == 1) ? SC_NIS : SC_NIS2;
    instrNumObs = seqDef->nisNumObs;
    instrNumScans = seqDef->nisNumScans;
    delTPerObs = seqDef->nisSecPerObs + seqDef->nisSecBtwObs;
    delTAtScanEnd = seqDef->nisSecBtwScan - seqDef->nisSecBtwObs;
    break;
  default:
    return (IMGFRM *) 0;
    break;
  }
  *framesLeft = instrNumScans * instrNumObs;      /* how many frames expected */

  instrName = orbit_set_instrument( lclsc, lclsc->_instrument);

  /* get bore and roll pointing structures for orbitgui.c
   * set time = inpOrbit->_agen._EtStart
   * imgfrm1 is start of list, imgfrmN is new IMGRM, 
   * imgfrm1->previf is end of list
   */
  orbitgui_return_boreroll( lclsc, &lclBore, &lclRoll);
  etNext = inpOrbit->_agen._EtStart;
  nextImgfrmPtr = &inpImgfrm;
  imgfrm1 = inpImgfrm;

  /* do scans & observations */

  for ( iScans=0; iScans<instrNumScans; ++iScans) {
    for ( iObs=0; iObs<instrNumObs; ++iObs) {
      orbit_set_instrument( lclsc, lclsc->_instrument);    /* get new vectors */
      if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
        return imgfrm1;

      /* saveImgfrm is pointer to next IMGFRM (null => alloc)  */
      saveImgfrm = *nextImgfrmPtr;
      if ( nextImgfrmPtr != &inpImgfrm)     /* if this is not the first frame */
        if ( saveImgfrm)                            /* & if new IMGFRM exists */
          if (saveImgfrm == imgfrm1)        /* & if it points back to imgfrm1 */
            saveImgfrm = (IMGFRM *) 0;      /* then force alloc of new IMGFRM */

      /* allocate (if saveImgfrm is null) & load IMGFRM */

      if ( !(imgfrmN=loadImageFrame(lclsc,etNext,saveImgfrm)) ) return imgfrm1;

      /* add imgfrmN to list that starts at imgfrm1 */

      if ( !imgfrm1) {                     /* imgfrm1 is null, start new list */
        imgfrm1 = imgfrmN;

      } else {                                         /* add imgfrmN to list */
        if ( !saveImgfrm) {           /* ... if imgfrmN did not already exist */
          imgfrm1->previf->nextif = imgfrmN;
          imgfrmN->previf = imgfrm1->previf;
          imgfrm1->previf = imgfrmN;
          imgfrmN->nextif = imgfrm1;
        }
      }
      if ( !imgfrm1->previf) imgfrm1->previf = imgfrmN;   /* fill null, leave */
      if ( !imgfrmN->nextif) imgfrmN->nextif = imgfrm1;      /* non-null ptrs */

      nextImgfrmPtr = &imgfrmN->nextif;                /* move to next imgfrm */

      IMGFRMSET;

      /* perform instrument-specific actions after each observation */
      switch ( seqDef->_type) {
      case CASTYPE_NISSEQDEF:
        lclsc->_nisStepAct += seqDef->nisStepMirror;
        ++numSpectra;
        break;
      default:
        break;
      }

      --*framesLeft;
      etNext += delTPerObs;
    }

    /* perform instrument-specific actions after each scan */
    switch ( seqDef->_type) {                                                 
    case CASTYPE_NISSEQDEF:                                                   
      lclsc->_nisStepAct = inpOrbit->_agen._StartingStep;
      ++numScan;
      break;                                                                  
    default:                                                                  
      break;                                                                  
    }                                                                         

    etNext += delTAtScanEnd;
  }
  return imgfrm1;
}

/*****************************************************/
/* calculate control variable values that give desired target overlaps
 *
 * INPUTS:
 *
 *   inpOrbit                      (ORBIT *) that contains info:
 *     ->_agen._EtStart            start time
 *            ._MaxTime            duration
 *            ._FrmOverlap         low target overlap
 *            ._FrmOverlap2        high target overlap
 *            ._seqDef             sequence definition
 *
 *   - if _seqDef is NISSEQDEF:
 *
 *            ._StartingStep       mirror position ast start of scan
 *            ._NISFOV             1 for narrow, 2 for wide
 *
 *   delT                       How often to make calculation
 *
 *   ctlVar                     Control Variable
 *
 */
long *
orbit_genOlapLimits( ORBIT *inpOrbit, long delT, long *ctlVar, long *ctlLims) {
SC *lclsc = (SC *) inpOrbit->_sc;
#define loLim ctlLims[0]
#define hiLim ctlLims[1]
double targLap, olap, olapAtLoVal, olapAtHiVal;
long *retVal, loVal, hiVal, curVal;
#define ctlVarVal(I) retVal[1+iHiLo+(2*I)]
#define nTimes *retVal
long iTimes, iHiLo, framesLeft;
IMGFRM *imgfrm, *imgfrm1a, *imgfrm1b, *imgfrm2a, *imgfrm2b;
double saveStart = inpOrbit->_agen._EtStart;
double saveOlap[2];

#define OLAPRESTORE \
  inpOrbit->_agen._EtStart = saveStart; \
  DOFOV100_CLROVERLAP( lclsc); \
  free_imgfrm_list( &imgfrm)

#ifdef ERRRTN
#undef ERRRTN
#endif
#define ERRRTN \
{ if ( !retVal) free( retVal); \
  OLAPRESTORE; \
  return (long *) NULL; \
}

/* macro to generate two scans of frames & calculate overlap between them */
#define DO1LAP( CTLVARVAL) \
  *ctlVar = (CTLVARVAL); \
  imgfrm1a = orbit_gen_anyInstrFrames( inpOrbit, imgfrm, &framesLeft); \
  if ( framesLeft || !imgfrm1a) { \
    fprintf( stderr, "ERROR in orbit_genOlapLimits(), %s; framesLeft=%ld\n" \
                   , "called _gen_anyInstrFrames()", framesLeft); \
    fflush( stderr); \
    ERRRTN; \
  } \
  if ( imgfrm) if ( imgfrm != imgfrm1a) free_imgfrm_list( imgfrm); \
  \
  /* find starts and ends of first & last half of list: */ \
  /* - step end of 2nd half twice for each single step of end of 1st half */ \
  /* - stop when end of 2nd half is at end of list */ \
  imgfrm = imgfrm1b = imgfrm1a; \
  imgfrm2b = imgfrm1a->nextif; \
  while ( imgfrm2b && (imgfrm2b != imgfrm1a)) { \
    /* break out of loop if at end of list */ \
    if (!imgfrm2b->nextif || (imgfrm2b->nextif==imgfrm1a)) break; \
    \
    /* step end of 2nd half twice ... */ \
    imgfrm2b = imgfrm2b->nextif; \
    if ( imgfrm2b && (imgfrm2b != imgfrm1a)) imgfrm2b = imgfrm2b->nextif; \
    \
    /* step end of 1st half once */ \
    imgfrm1b = imgfrm1b->nextif; \
    \
  } \
  if ( !imgfrm2b || (imgfrm2b == imgfrm1a)) { \
    fprintf( stderr, "ERROR in orbit_genOlapLimits(), uneven # frames\n"); \
    fflush( stderr); \
    ERRRTN; \
  } \
  imgfrm2a = imgfrm1b->nextif; \
  olap = orbit_fov_calcScanOverlap( imgfrm1a, imgfrm1b, imgfrm2a, imgfrm2b)

  DOFOV100_SETOVERLAP( lclsc);

  iTimes = (inpOrbit->_agen._MaxTime / delT) + 1;
  retVal = (long *) malloc( sizeof(long) * (1 + (2*iTimes)));
  if ( !retVal) ERRRTN
  *retVal = iTimes;

  fprintf( stdout, "%ld\n", *retVal);

  imgfrm = (IMGFRM *) NULL;

  for ( iTimes=0; iTimes<*retVal; ++iTimes) {

    inpOrbit->_agen._EtStart = saveStart + (iTimes *delT);

    for ( iHiLo=0; iHiLo<2; ++iHiLo) {
      targLap = (iHiLo==0) ? inpOrbit->_agen._FrmOverlap 
                           : inpOrbit->_agen._FrmOverlap2;

      targLap = (targLap < (0.5/MAXFOV100PTS)) ? (0.5/MAXFOV100PTS) :
                (targLap > (1.0-(0.5/MAXFOV100PTS))) ? (1.0-(0.5/MAXFOV100PTS))
                : targLap;

      loVal = ctlLims[0];                 /* calculate overlap at lower limit */
      DO1LAP( loVal); olapAtLoVal = olap;

      if ( olapAtLoVal <= targLap) { /* if overlap at lower limit is too low, */
        hiVal = loVal;                                /* use that lower limit */
        olapAtHiVal = olapAtLoVal;

      } else {
        hiVal = ctlLims[1];                  /* ... and vice versa for hi lim */
        DO1LAP( hiVal); olapAtHiVal = olap;
        if ( olapAtHiVal >= targLap) {
          loVal = hiVal;
          olapAtLoVal = olapAtHiVal;
        }
      }

      while ( (hiVal - loVal) > 1) {                    /* binary search loop */
        curVal = (hiVal + loVal) / 2;
        DO1LAP( curVal);
        if ( olap < targLap)      { hiVal = curVal; olapAtHiVal = olap; } 
        else if ( olap > targLap) { loVal = curVal; olapAtLoVal = olap; } 
             else { hiVal = loVal = curVal; olapAtHiVal=olapAtLoVal=olap; }
      }

      /* pick best limit */
      if ( fabs(olapAtHiVal-targLap) < fabs(olapAtLoVal-targLap)) {
        ctlVarVal(iTimes) = hiVal;
        saveOlap[iHiLo] = olapAtHiVal;
      } else {
        ctlVarVal(iTimes) = loVal;
        saveOlap[iHiLo] = olapAtLoVal;
      }

      fprintf( stdout, "%ld ", ctlVarVal(iTimes));

    } /* for iHiLo */

    fflush( stdout);
    fprintf( stderr, "   #  %lf %lf", saveOlap[0], saveOlap[1]);
    fflush( stderr);

    fprintf( stdout, "\n");

  } /* for iTimes */

  fflush( stdout);

  OLAPRESTORE;
  return retVal;
} /* orbit_genOlapLimits() */
