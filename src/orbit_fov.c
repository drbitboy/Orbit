/* orbitfort.c - C-to-FORTRAN interface routines */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#ifdef vms
char *malloc();
#else
#include <malloc.h>
#endif

#include "orbit3d.h"
#include "pointing.h"

static VEC nullVec = { 0.0, 0.0, 0.0 };

#include "debug.h"
#include "orbit_spice_names.h"

typedef struct FOVABFstr {
  VEC _scVec;
  VEC _tmpVec;
  double _scalefac; /* factor used to scale abf vec = VDOT(ABF,uvBoreAbf) */
  double _yMax; /* if SCALEFAC==0.0, PTFOV will be (0,YMAX+1) (outside FOV) */
  VEC _uvBoreAbf; /* abf boresight unit vector */
  VEC _uvXAbf; /* abf unit vector of X dir in FOV, perpendicular to UVBOREABF */
  VEC _uvYAbf; /* abf unit vector of Y dir in FOV, = UVBOREABF cross UVXABF */
  double *_camVert;  /* fov vertices */
  int _nVert;
  IMGFRM *_imgfrm;
  struct FOVABFstr *next;
} FOVABF;

/****************************************************************************/
/* macro to calculate X&Y perspective projections of an ABF vector to a FOV 
 * VEC PTABF:  abf point to project
 * VEC PTFOV:  projected fov vector (x & y only, suitable for orbit_inside)
 */

#define CHOP2FOVFULL( PTABF, PTFOV, FOVABFPTR) \
  VMINUS2( PTABF, FOVABFPTR->_scVec, FOVABFPTR->_tmpVec); \
  FOVABFPTR->_scalefac = VDOT(FOVABFPTR->_tmpVec, FOVABFPTR->_uvBoreAbf); \
  if ( FOVABFPTR->_scalefac != 0.0) { \
    (PTFOV)[0] = VDOT( FOVABFPTR->_tmpVec,FOVABFPTR->_uvXAbf) \
                 / FOVABFPTR->_scalefac; \
    (PTFOV)[1] = VDOT( FOVABFPTR->_tmpVec,FOVABFPTR->_uvYAbf) \
                 / FOVABFPTR->_scalefac; \
  } else { (PTFOV)[0]=0.0; (PTFOV)[1]=(FOVABFPTR->_yMax)+1.0; } \
  (PTFOV)[2] = 1.0

#define CHOP2FOV CHOP2FOVFULL( ptAbf, ptFov, currFovAbfPtr)

/***************************/
/* fill FOVABF from IMGFRM */

FOVABF *
orbit_fov_imgfrmToFovAbf( IMGFRM *imgfrm, FOVABF *fovAbfPtr) {
FOVABF *currFovAbfPtr = fovAbfPtr;
double *ptAbf, *ptFov;
long iVert;

  if ( !currFovAbfPtr) {                          /* malloc FOVABF + _camVert */
    currFovAbfPtr = (FOVABF *) malloc( sizeof(FOVABF) 
                                     + (sizeof(double)*3*imgfrm->_nClosedPts));
  }
  currFovAbfPtr->next = (FOVABF *) NULL;
  currFovAbfPtr->_imgfrm = imgfrm;
  currFovAbfPtr->_nVert = imgfrm->_nClosedPts;
  currFovAbfPtr->_camVert = (double *) (currFovAbfPtr + 1);
  
  /* calculate imgfrm S/C vector & imgfrm boresight abf unit vector */

  VADD2( imgfrm->_boreVec, imgfrm->_boretoscVec, currFovAbfPtr->_scVec);
  VSCAL2( -1.0, currFovAbfPtr->_scVec, currFovAbfPtr->_uvBoreAbf);
  vhat( currFovAbfPtr->_uvBoreAbf, currFovAbfPtr->_uvBoreAbf);

  /* get arbitrary X & Y FOV vectors in abf for imgfrm */

  VMINUS2( imgfrm->vec3, currFovAbfPtr->_uvBoreAbf, currFovAbfPtr->_tmpVec);
  ucrss( currFovAbfPtr->_uvBoreAbf, currFovAbfPtr->_tmpVec
       , currFovAbfPtr->_uvXAbf);
  ucrss( currFovAbfPtr->_uvBoreAbf, currFovAbfPtr->_uvXAbf
       , currFovAbfPtr->_uvYAbf);

  /* convert imgfrm fov abf vertices to FOV, save max Y */

  currFovAbfPtr->_yMax = -1e6;
  for ( ptAbf=imgfrm->vec3, ptFov=currFovAbfPtr->_camVert, iVert=0
      ; iVert < imgfrm->_nClosedPts
      ; ++iVert, ptAbf+=3, ptFov+=3) {

    CHOP2FOV;
    if ( currFovAbfPtr->_yMax < ptFov[1] && currFovAbfPtr->_scalefac != 0.0) 
      currFovAbfPtr->_yMax = ptFov[1];
  }
  return currFovAbfPtr;
}

/*******************************/
/* free linked list of FOVABFs */

void
orbit_fov_freeFovAbfList( FOVABF *fovAbfBase) {
FOVABF *fovAbfPtr;

  for ( fovAbfPtr=fovAbfBase; fovAbfBase; fovAbfPtr=fovAbfBase) {
    fovAbfBase = fovAbfBase->next;
    free( fovAbfPtr);
  }
  return;
}

/******************************************/
/* calculate overlap onto baseImgfrm by list of IMGFRM's represented by 
 * list of FOVABF structs
 */

double
orbit_fov_calcSingleOverlap( IMGFRM *baseImgfrm, FOVABF *scanFovAbfPtrList) {
VEC ptFov;
double *ptAbf;
long iFov, iVert, nVert;
double overlapCount;
FOVABF *currFovAbfPtr;


  if ( !(nVert=baseImgfrm->_nClosedPts)) return 0.0;

  /* for each point on the body */

  for ( overlapCount=iFov=0; iFov<baseImgfrm->_nFov100Plates; ++iFov) {
    ptAbf = baseImgfrm->_fov100Pts[iFov];

    /* loop through all input IMGFRM/FOVABF's */

    for ( currFovAbfPtr=scanFovAbfPtrList; currFovAbfPtr
        ; currFovAbfPtr=currFovAbfPtr->next) {

      CHOP2FOV;                    /* convert fov100 point from ABF to FOVABF */

      /* test if ptAbf is inside current FOVABF fov ... */

      if ( orbit_inside( ptFov, currFovAbfPtr->_camVert, currFovAbfPtr->_nVert
                       , (int) 1, (int) 0)) {
        overlapCount += 1.0;             /* ... if it is, increment count ... */
        break;  /* and break out of FOVABF loop so ptAbf is only counted once */

      } /* if orbit_inside ...  */
    } /* for currFovAbfPtr ... */
  } /* for iFov ... */

  return overlapCount;
}

/********************************************/
/* calculate overlap between adjacent scans 
 * - base0 & base1 are start & end of linked list of IMGFRMs
 * - scan0 & scan1 are start & end of linked list of IMGFRMs
 * - routine returns approximate overlap (0.0-1.0) of 
 *   scan0 through scan1 over base0 through base1
 * - if linked list assumption is not true, return -1.0
 */
double
orbit_fov_calcScanOverlap( 
    IMGFRM *base0, IMGFRM *base1 , IMGFRM *scan0, IMGFRM *scan1) {
IMGFRM *base, *scanImgfrm;
FOVABF *fovAbfBase, *fovAbfPtr, **fovAbfPtrPtr;
int firstPass; 
double totalOverlap, totalFov100Pts;

  /* malloc & build FOVABF list from scan IMGFRM list */

  fovAbfPtrPtr = &fovAbfBase;
  fovAbfBase = (FOVABF *) NULL;
  firstPass = 1;

  for ( scanImgfrm=scan0; scanImgfrm!=scan1->nextif
      ; scanImgfrm=scanImgfrm->nextif) {

    /* test for error in scan0/scan1 linked list - scan1 not found before ... */
    if ( !scanImgfrm                                  /* ... null pointer ... */
      || (!firstPass && (scanImgfrm==scan0))) {        /* ... or looping list */
      fprintf( stderr
             , "***Program error, contact programmer, code %s%s\n"
             , "WSNBATGH-orbit_fov_calcScanOverlap-SCAN-"
             , (!scanImgfrm) ? "null_pointer_in_list" : "looping_list"
             );
      fflush( stderr);
      orbit_fov_freeFovAbfList( fovAbfBase);    /* cleanup - free FOVABF list */
      return -1.0;
    }
    firstPass = 0;

    *fovAbfPtrPtr = fovAbfPtr = 
     orbit_fov_imgfrmToFovAbf( scanImgfrm, (FOVABF *) NULL);
    if ( fovAbfPtr) fovAbfPtrPtr = &fovAbfPtr->next;
  } /* for scanImgfrm */

  /* loop through each IMGFRM in base list */

  totalOverlap = totalFov100Pts = 0;

  firstPass = 1;
  for ( base=base0; base!=base1->nextif; base=base->nextif) {

    /* test for error in base0/base1 linked list - base1 not found before ... */
    if ( !base                                        /* ... null pointer ... */
      || (!firstPass && (base==base0))) {              /* ... or looping list */
      fprintf( stderr
             , "***Program error, contact programmer, code %s%s\n"
             , "WSNBATGH-orbit_fov_calcScanOverlap-BASE-"
             , (!base) ? "null_pointer_in_list" : "looping_list"
             );
      fflush( stderr);
      orbit_fov_freeFovAbfList( fovAbfBase);    /* cleanup - free FOVABF list */
      return -1.0;
    }
    firstPass = 0;

    /* increment overlapped & total number of fov 100 points on surface */

    totalOverlap += orbit_fov_calcSingleOverlap( base, fovAbfBase);
    totalFov100Pts += base->_nFov100Plates;
  } /* for base ... */


  orbit_fov_freeFovAbfList( fovAbfBase);        /* cleanup - free FOVABF list */

  return (totalFov100Pts==0.0) ? 0.0 : (totalOverlap/totalFov100Pts);
}

double orbit_fov_triangleArea( VEC a, VEC b, VEC c) {
VEC ab, bc, abxbc;
  VMINUS2( a, b, ab);
  VMINUS2( b, c, bc);
  vcross( ab, bc, abxbc);    /* len = [(len ab) * (len bc) * sin(angle(abc))] */
  return VLEN(abxbc) / 2.0;
}

#define nClosedPts sc->_nClosedPts
#define nFov100Pts sc->_nFov100Pts
#define fov100Pts sc->_fov100Pts
#define camvert sc->_camvert
#define camabf sc->_camabf
#define boresight sc->_boresight
#define sampdir sc->_sampdir
#define updir sc->_updir

/* macros to convert to & from coordinate system where X,Y = sampdir,updir */
/* - origin is boresight
 * - assumes boresight & all VIN points are in the same plane of sampdir,updir
 */

#define TOXY( VIN, VOUT) \
  VOUT[0] = VDOT( sampdir, VIN) - VDOT(sampdir, boresight); \
  VOUT[1] = VDOT( updir, VIN) - VDOT( updir, boresight); \
  VOUT[2] = 0.0

#define FROMXY( VIN, VOUT) \
  vmxpb( VIN[0], sampdir, boresight, VOUT); \
  vmxpb( VIN[1], updir, VOUT, VOUT)

/*************************************/
/* distribute ~ NUMFOV100PTS points in an FOV
 * - assume orbit_set_instrument() has already been called
 */ 

void
orbit_fov_fillFov( SC *sc) {
long ifov, ivert, ivert3;
long iSamp, iUp, nSampUp;
double area, xMax, xMin, yMax, yMin;
VEC v0;
double *v1, *v2, *vvert;
double pitch;
VEC zDir;
VEC dV0, v;
double sqrt3 = sqrt(3.0);
double *fov100LclPtr;
long notConverging;

  /* convert 3D vertices to 2D (X & Y) - boresight should be (0,0) */
  v0[0]=v0[1]=v0[2]=0.0;   /* TOXY( boresight, v0); */
  xMax = xMin = v0[0];
  yMax = yMin = v0[1];

  area = 0.0;
  for ( vvert=camvert, v2=camabf, ivert=0; ivert < *nClosedPts
      ; ++ivert, vvert+=3, v2+=3) {

    TOXY( vvert, v2);   /* store 2D vertices in camabf */
    if ( v2[0] > xMax) xMax = v2[0];
    else if ( v2[0] < xMin) xMin = v2[0];
    if ( v2[1] > yMax) yMax = v2[1];
    else if ( v2[1] < yMin) yMin = v2[1];

    /* estimate area of 2D fov by summing triangles in closed polygon 
     * - each triangle is boresight + 2 adjacent fov vertices
     *   - assumes that boresight is inside fov 
     *     & that shape of fov is such that none of the triangles overlap
     */
    if ( ivert) area += orbit_fov_triangleArea( v0, v1, v2);
    v1 = v2;                                    /* for next triangleArea call */
  }

  /* last triangle */
  if ( ivert>1) area += orbit_fov_triangleArea( v0, v1, camabf);

  *nFov100Pts = 0;
  if ( area == 0.0 ) return;

  /* area/point = pitch^2 * sqrt(3) / 2, calculate first guess at pitch */

  pitch = sqrt( (area/NUMFOV100PTS) * 2 / sqrt(3.0));

  notConverging = 0;

  while ( *nFov100Pts > MAXFOV100PTS || *nFov100Pts < MINFOV100PTS) {
  long nThisBoxSize;

    fov100LclPtr = fov100Pts;
    *nFov100Pts = 0;

    /* test points on sides of boxes of increasing size centered on boresight 
     * as long as at least point is added at each size
     * - v0 is bottom left corner of box
     */
    v0[0]=v0[1]=v0[2]=0.0;   /* initial box has side length of 0 at boresight */
    dV0[0] = pitch / 2.0;            /* incremental offset of v0 at each size */
    dV0[1] = dV0[0] * sqrt3;
    dV0[2] = 0.0;

    for ( (nThisBoxSize=1), nSampUp=0; nThisBoxSize; ++nSampUp) {
    VEC vX;

      srand( (unsigned int) 655357);                /* non-random seed for vX */
      vX[2] = 0.0;                            /* vX = v + random displacement */

      nThisBoxSize = 0;      /* initialize count of points added at this size */

      CPYVEC( v0, v); /* v is current pt, start it at bottom left of box (v0) */

      for ( iSamp=0; iSamp<=nSampUp; ++iSamp) {
      int insideWidth = iSamp && (iSamp<nSampUp);

        v[1] = v0[1];                            /* move to bottom row of box */

        for ( iUp=0; iUp<=nSampUp; ++iUp) {

          /* skip to top of box if inside box sides */
          if (insideWidth && iUp && iUp<nSampUp) {
            iUp = nSampUp;
            v[1] = v0[1] + (nSampUp * pitch * sqrt3);
          }

          /* add random offset of +/- (corner offset) / 8 */

          vX[0] = (dV0[0] * rand()) / ((unsigned long)RAND_MAX*4);
          vX[1] = (dV0[1] * rand()) / ((unsigned long)RAND_MAX*4);
          vmxpb( -0.125, dV0, vX, vX);
          VADD2( vX, v, vX);

          if ( xMin<=vX[0] && vX[0]<=xMax && yMin<=vX[1] && vX[1]<=yMax) {
            if ( orbit_inside( vX, camabf, (int) *nClosedPts, (int)0, (int)1)) {
              nThisBoxSize++;
              if ( (*nFov100Pts)++ < MAXFOV100PTS) {
                CPYVEC( vX, fov100LclPtr);
                fov100LclPtr += 3;
              }
            }
          }
          v[1] += pitch*sqrt3;   /* move up by hgt of 2 equilateral triangles */
        } /* for iUp */
        v[0] += pitch;        /* move right by base of 1 equilateral triangle */
      } /* for iSamp */

      VMINUS2( v0, dV0, v0);    /* move bottom left corner of box */

    } /* for ; nThisBoxSize */

    pitch *= sqrt( *nFov100Pts       /* adjust pitch */
                   / (double) ((*nFov100Pts < MINFOV100PTS) 
                               ? MINFOV100PTS+1
                               : MAXFOV100PTS-1));

    /* break out of loop is not converging */

    if ( ++notConverging > 90)
    {
      fprintf( stderr, "step, *nFov100Pts, pitch = %ld, %ld, %lg\n"
                     , notConverging, *nFov100Pts, pitch);
      if ( notConverging > 99)
      {
        fprintf( stderr, "FOV Point placement not converging; ");
        fprintf( stderr, "Switching to spray method ...\n");
        break;
      }
    }

  } /* while ( *nFov100Pts > MAXFOV100PTS || *nFov100Pts < MINFOV100PTS) */

  /* if not converged, spray pseudo-random points into FOV */

  if ( *nFov100Pts > MAXFOV100PTS || *nFov100Pts < MINFOV100PTS)
  {
  double frac0, frac1;
  VEC vX;
  
    srand( (unsigned int) 655357);                /* non-random seed for vX */
    *nFov100Pts = 0;
    fov100LclPtr = fov100Pts;
    while ( *nFov100Pts < MAXFOV100PTS)
    {
       frac0 = rand() / (double) RAND_MAX;
       frac1 = rand() / (double) RAND_MAX;
       vX[0] = frac0 * xMin + (1.0 - frac0) * xMax;
       vX[1] = frac1 * yMin + (1.0 - frac1) * yMax;
       if ( orbit_inside( vX, camabf, (int) *nClosedPts, (int)0, (int)1))
       {
         (*nFov100Pts)++;
         CPYVEC( vX, fov100LclPtr);
         fov100LclPtr += 3;
       }
    }  /* while ( *nFov100Pts < NUMFOV100PTS) */

    fprintf( stderr, " ... sprayed %ld FOV points\n", *nFov100Pts);

  } /* if ( *nFov100Pts > MAXFOV100PTS || *nFov100Pts < MINFOV100PTS) */

  /* convert from 2D to 3D s/c coordinates */

  for ( fov100LclPtr = fov100Pts, ifov=0; ifov<*nFov100Pts
      ; ++ifov, fov100LclPtr += 3) {
    CPYVEC( fov100LclPtr, v);
    v[2] = 0.0;   /* just in case */
    FROMXY( v, fov100LclPtr);
  }
  return;
}

#ifdef _ORBIT_FOV_TEST_
int
main( int argc, char **argv) {
VEC a, b, c;
  while (1) {
    if ( 3 != scanf( "%lf%*[, ]%lf%*[, ]%lf", a, a+1, a+2)) break;
    if ( 3 != scanf( "%lf%*[, ]%lf%*[, ]%lf", b, b+1, b+2)) break;
    if ( 3 != scanf( "%lf%*[, ]%lf%*[, ]%lf", c, c+1, c+2)) break;
    printf( " %20.7lg", orbit_fov_triangleArea( a, b, c));
    printf( " %20.7lg", orbit_fov_triangleArea( c, a, b));
    printf( " %20.7lg", orbit_fov_triangleArea( b, c, a));
    printf( "\n = area of \n");
    printf( " %20.7lg %20lg %20lg\n", a[0], a[1], a[2]);
    printf( " %20.7lg %20lg %20lg\n", b[0], b[1], b[2]);
    printf( " %20.7lg %20lg %20lg\n", c[0], c[1], c[2]);
  }
  return 0;
}
#endif /* _ORBIT_FOV_TEST_ */
