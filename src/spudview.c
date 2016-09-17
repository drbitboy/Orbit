#include <stdio.h>
#include <string.h>
#include <math.h>
#include <malloc.h>
#include "orbit3d.h"
#include "spudshap.h"

#ifdef vms
char *malloc();
int indexx();
#endif

#ifdef DEBUG
#define DPR(A) fprintf A , fflush(stderr)
#else
#define DPR(A)
#endif

#define spudf spudv->_spudf
#define loe spudf->oe
#define loeidx spudf->oeidx
#define lfaceapices spudf->faceapices
#define lfaceoeidx spudf->faceoeidx

#define VR spudv->_VR
#define SR spudv->_SR
#define hidden spudv->_hidden
#define persp spudv->_persp
#define range spudv->_range
#define xyz spudv->_xyz
#define valt spudv->_valt
#define segstat spudv->_segstat
#define vstat spudv->_vstat
#define pstat spudv->_pstat
#define v0 spudv->_v0
#define facemin0 spudv->_facemin0
#define facemin1 spudv->_facemin1
#define sortv0 spudv->_sortv0
#define sortfmin0 spudv->_sortfmin0
#define viewabf spudv->_viewabf
#define viewUpAbf spudv->_viewUpAbf
#define scCam spudv->_scCam
#define scAbf spudv->_scAbf

double sv0StatPlates, sv0StatPtsArr[10];
double sv1StatPlates, sv1StatPtsArr[10];

/* spudview0 - rotate shape model by matrix
 *           - sort for hidden gridpt check if required
 */

void 
spudview0( SPUDV *spudv, double MTXR[4][4], int sort4hgpc) {
/* sort4hgpc==0: just rotate; 
 * sort4hgpc!=0: rotate and sort for hidden gridpoint check
 */
unsigned long i, j, k;
long S;
double bigval;
double xtmp, ytmp, dx, dy;
double xtmp1, ytmp1;
unsigned long numvert = spudf->nv;

  if ( sort4hgpc) {
    sort4hgpc = 1; /* 0 becomes 0; !0 becomes 1 */
  }

  /* save view & viewing object position for later calcs
   *                                                            noPersp Persp
   * viewabf   - reverse boresight in ABF
   *             - for emission check                              X
   * viewUpAbf - camera UP in ABF                                         X
   *             - to determine if plate is above 
   *               or below viewer
   * scCam     - viewer wrt body in camera coordinates                    X
   * scAbf     - viewer in ABF, INPUT REQUIRED FOR PERSPECTIVE            X
   */

# ifndef _SPUDVIEW_PERSP_ /* perspective disallowed unless this macro defined */
  range = 0.0;   /* mimic infinity, i.e. eliminate any attempt at perspective */
# endif

  if ( range == 0.0) { 
    VNEG2( MTXR[1], viewabf);             /* view from infinity, use unit vec */

  } else {                   /* **************** PERSPECTIVE***************** */
    for ( i=0;i<3;++i) {
      scCam[i] = VDOT(MTXR[i],scAbf);
    }
    /* VSCAL2( -range, MTXR[1], viewabf);             /* camera position, ABF */
    VSCAL2( 1.0, MTXR[2], viewUpAbf);          /* camera up, unit vector, ABF */
  }

/* macros to rotate body: */

#define ICOPTR(S) (xyz+((S)*3))

#define ROT \
  double *basexyz, *spudvxyz; \
  double scaleFactor; \
  int istep = sort4hgpc ? 1 : 2;           /* see for i= loop below */ \
  VEC scToXyz; \
    basexyz = spudf->Rxyz; \
    spudvxyz = xyz; \
    for (S=0; S < numvert; S++, basexyz += 3, spudvxyz += 3) { \
      for (i=0; i<3; i+=istep) {     /* only compute x and z (istep=2) */ \
      double *mtxr;            /* unless doing hidden gridpt (istep=1) */ \
        mtxr = MTXR[i]; \
        spudvxyz[i] = VDOT(mtxr,basexyz); \
      } \
      \
      /* ***N.B. matching "}" to go with "for S=0 ... {" above */
      /*         left off macro #definition */

  if ( sort4hgpc) { 
  double *lv0 = v0;
  double *lsinalt0 = valt;
/**/
    ROT
      /* put X or azimuth into v0 for sort below */
      if ( range == 0.0) *(lv0++) = *spudvxyz;       /* no perspective: use X */
      else {
        VMINUS2( spudvxyz, scCam, scToXyz);      /* perspective:  use azimuth */
        /* CPYVEC( spudvxyz, scToXyz);        
        /* scToXyz[1] += range; /* subtract sc (at [0,-range,0]) from grid pt */
        /* save azimuth from s/c */
        *(lv0++) = ( *scToXyz != 0.0 || scToXyz[1] != 0.0)
                   ? atan2( *scToXyz, scToXyz[1]) 
#                  define PI_SPEC (M_PI + 1.0)
                   : PI_SPEC;     /* special value for pts above or below s/c */
        /* save sin(altitude) from s/c */
        *(lsinalt0++) = scToXyz[2] / VLEN(scToXyz); 
/**/
      }
    } /* ROT */
    indexx( &numvert, v0, sortv0);  /* v0[sortv0] are vertices sorted by X */

    /* put dummy azimuth/X value in v0[nv] that is > real largest az/X */
    if ( range > 0.0) *lv0 = 999.0;
    else {
      bigval = v0[sortv0[numvert-1]];
      *lv0 = bigval + ((bigval == 0.0) ? 1.0 : fabs(bigval));
    }

  } else {
    ROT
    } /* ROT */
  }
  return;
} /* spudview0 */

/* spudviewhgpcPersp:  hidden grid point check using perspective */

void 
spudviewhgpcPersp( SPUDV *spudv, short ibit) {
unsigned long ilo, ihi, imid, iv, siv, ip, iv3[3];
unsigned long i, j, k, F, FF, F0, F1, Fnext, P, PP, npanel, panel;
double *p, *v[3], lox, hix, loz, hiz, x;
double *lsinalt0, *lsinalt[3];
VEC pltNorm, v01, v02;  /* plate normal derived from plate, camera coodinates */
VEC pltToP;
VEC xyzToSc;
double dotProds[3];
VEC pltCrossProds[3], scToXyz[4];
/* #define CALCXYZVEC(V,I) CPYVEC(V,scToXyz[I]); scToXyz[I][1] += range */
#define CALCXYZVEC(V,I) CPYVEC(V,scToXyz[I]); VMINUS2( V, scCam, scToXyz[I]);
#define CALCXYZVECV(I) CALCXYZVEC(v[I],I)
#define CALCXYZVECP CALCXYZVEC(p,3)
short notibit = ~ibit;
short zeroTo2Pi, ipass;
double loaz, hiaz;

int iLoIdx, iHiIdx;
double iLoHiArr[4];

unsigned long numvert = spudf->nv;
unsigned long numface = spudf->nface;

double *sv1StatPtsPtr;

#ifndef MAX
#define MAX(A,B) ((A)>(B)?(A):(B))
#endif

  /* set all vertices visible for now */
  for ( iv=0; iv<numvert; ++iv) vstat[iv] |= ibit;

  lsinalt0 = valt;
/**/
  /* loop through plates, find which vertices are hidden by each plate */
  for ( ip=0; ip<numface; ++ip) {

    iv3[0] = lfaceapices[ip];

    /* get vector from first vertex to viewer, dot product of that 
     * with normal should be positive to continue i.e. emission < 90
     */
    VMINUS2( scAbf, spudf->Rxyz+(iv3[0]*3), xyzToSc);
    if ( VDOT( xyzToSc, spudf->uvnorms+(3*ip)) <= 0.0) continue;

    iv3[1] = loe[lfaceoeidx[ip]];
    iv3[2] = loe[lfaceoeidx[ip]+1];

    for ( i=0; i<3; ++i) {
      v[i] = ICOPTR(iv3[i]);
      lsinalt[i] = lsinalt0 + iv3[i];
/**/
      CALCXYZVECV(i);
    }

    /* get plate normal in camera coordinates */
    VMINUS2( v[1], v[0], v01);
    VMINUS2( v[2], v[0], v02);
    vcross( v01, v02, pltNorm);
    /* ensure plate normal points toward s/c => away from sc-to-plate vec */
    if ( VDOT( scToXyz[0], pltNorm) > 0.0) { VNEG( pltNorm); } 

    /* get dot products to determine on which side of plane of 
     * camera/v[N]/v[N+1] the gridpt v[N+2] is
     */
    for ( i=0; i<3; ++i) {
      vcross( scToXyz[i], scToXyz[(i+1)%3], pltCrossProds[i]);
      dotProds[i] = VDOT( scToXyz[(i+2)%3], pltCrossProds[i]);
    }

    /* find rectangular range (azimuth & pseudo-altitude) of plate extents */
    /* - azimuth
     */
    lox = M_PI;
    hix = -M_PI;
    for ( i=0; i<3; ++i) {
      x = v0[iv3[i]];
      if ( x <= M_PI) {                     /* for those points which are not */
        if ( x < lox) lox = x;              /* directly above or below camera */
        if ( x > hix) hix = x;
      }
    }

    /* - is max azimuth delta <= 180 (w/az's in range -180 to +180) */

    hiz = -1.0;    /* set max & min pseudo-altitude also */
    loz = 1.0;
/**/
    if ( (hix-lox) <= M_PI) zeroTo2Pi = 0;                             /* yes */
    else {                                                              /* no */
    double hix02pi = -1.0;
    double lox02pi = 3*M_PI;
      for ( i=0; i<3; ++i) {
        x = v0[iv3[i]];
        if ( x <= M_PI) {
          if ( x<0.0) x += (2*M_PI);
          if ( x<lox02pi) lox02pi = x;
          if ( x>hix02pi) hix02pi = x;
        }
      }
      if ( (hix02pi-lox02pi) <= M_PI) { /* az's max del<=180 w/range 0 to 360 */
        zeroTo2Pi = 1;
        DPR((stderr, "Z"));
      } else {                            /* no, plate covers entire az range */
        DPR((stderr, "z"));
        zeroTo2Pi = 0;
        lox = -M_PI;
        hix = PI_SPEC;      /* to catch points directly above or below camera */

        /* if plate norm is facing down, set hiz beyond max possible sin(alt) */

        if ( VDOT( viewUpAbf, spudf->uvnorms+(3*ip)) <= 0.0) hiz = 1.1;
        else loz = -1.1;              /* else set loz below min poss sin(alt) */
/**/
      }
    }

    /* - pseudo-altitude i.e. max & min lat extents of great circle paths
     *   between vertices 
     */
    for ( i=0; i<3 && (loz > -1.0 || hiz < 1.0); ++i) {
    double *vlo, *vhi;
    VEC axis, tangent;
    long iPlus1;
      iPlus1 = (i+1) % 3;
      if ( *(lsinalt[i]) < *(lsinalt[iPlus1]) ) {
        iLoIdx = i; iHiIdx = iPlus1;
        vlo = scToXyz[i]; vhi = scToXyz[iPlus1];
        CPYVEC( scToXyz[i], axis);
      } else {
        iHiIdx = i; iLoIdx = iPlus1;
        vhi = scToXyz[i]; vlo = scToXyz[iPlus1];
      }
      vcross( vlo, vhi, axis);

                                           /* if vlo-to-vhi great circle path */
      x = VLEN(axis);                          /* (perpendicular to axis) ... */
      if ( x != 0.0 && x != 1.0) {
        x = 1.0 / x;              /* (make axis unit vector so its horizontal */
        VSCAL( x, axis); /* length is sin(alt) of max extent of great circle) */
      }
      if ( loz > -1.0) {        
        vcross( axis, vlo, tangent);      /* ... goes below vlo, tangent will */
        if ( tangent[2] < 0.0) {                  /* aim down i.e. have Z < 0 */
          x = - sqrt( 1.0 - (axis[2]*axis[2]));
        } else {
          x = *(lsinalt[iLoIdx]);
        }
        if ( x < loz) loz = x;
      }
      /* repeat similar action for hiz */
      if ( hiz < 1.0) {
        VNEG( axis);                            /* (vhi X vlo) = -(vlo X vhi) */
        vcross( axis, vhi, tangent);
        if ( tangent[2] > 0.0) {
          x = sqrt( 1.0 - (axis[2]*axis[2]));
        } else {
          x = *(lsinalt[iHiIdx]);
        }
        if ( x > hiz) hiz = x;
      }
    } /* for i=0; i<3; ++i ...*/
/**/
      
    if ( zeroTo2Pi) {
      iLoHiArr[0] = -M_PI;
      iLoHiArr[1] = lox;
      iLoHiArr[2] = hix;
      iLoHiArr[3] = M_PI;
    } else {
      iLoHiArr[0] = lox;
      iLoHiArr[1] = hix;
    }

#   ifdef INCSVSTAT
#   undef INCSVSTAT
#   endif

#   ifdef SVSTATS
    sv1StatPlates++;
#   define INCSVSTAT (*(sv1StatPtsPtr++))++;
    sv1StatPtsArr[0] += numvert; /* 0 */
#   else
#   define INCSVSTAT
#   endif


    for ( i=0; i<= zeroTo2Pi; ++i) {
      lox = iLoHiArr[i*2];
      hix = iLoHiArr[(i*2)+1];

      /* binary search to find vertex with lowest azimuth to test 
       * - keep ilo to left of lowest azimuth
       */
      ilo = 0; ihi = numvert - 1; 
      for ( imid = (ilo + ihi) / 2; imid > ilo; ) {
        if ( v0[sortv0[imid]] < lox) ilo = imid;
        else ihi = imid;
        imid = (ilo + ihi) / 2;
      }

      /* step through vertices that may be covered by this plate */
      /* ***N.B. extra v0 & sortv0 needed when iv=nv */
      for ( siv=sortv0[iv=ilo] ; v0[siv] <= hix ; siv=sortv0[++iv]) {

#       ifdef SVSTATS
        sv1StatPtsPtr = sv1StatPtsArr + 1;
#       endif
        INCSVSTAT /* 1 */

        /* inside this loop, "continue" => pt cannot be hidden by plate */

        if ( lsinalt0[siv]>hiz) continue;        /* above max sin(alt) extent */
        INCSVSTAT /* 2 */
        if ( lsinalt0[siv]<loz) continue;        /* below min sin(alt) extent */
        INCSVSTAT /* 3 */
        if ( !(vstat[siv]&ibit)) continue;             /* not already hidden? */
        INCSVSTAT /* 4 */
        p = ICOPTR(siv);                                         /* get point */

        CALCXYZVECP;                     /* calc sc-to-pt vector (scToXyz[3]) */
        /* dot sc-to-pt vec with each of frustum's plane's normal, check if
         * point is on same side of planes as plate vertex that is opposite edge
         */
        for ( j=0; j<3; ++j) {
          if ( (dotProds[j] * VDOT(scToXyz[3],pltCrossProds[j])) < 0.0) {
            break;    /* point is not on same side of frustum plane as 3rd pt */
          }
        }
        if ( j < 3) continue;     /* broke out => point is not inside frustum */
        INCSVSTAT /* 5 */

        VMINUS2( p, v[0], pltToP);/* plate-to-point vector for dot w/plt norm */
        if ( VDOT( pltNorm, pltToP) > 0.0)       /* pt & s/c on same sides of */
          continue;                               /* plt => plt can't hide pt */
        INCSVSTAT /* 6 */

        vstat[siv] &= notibit;

      } /* for siv=sortv0[iv=ilo] ... */

    } /* for i=0; i<=zeroTo2Pi ... */
  } /* for ip ... */

  return;
} /* spudviewhgpcPersp */

/* check if point p is hidden by triangle formed by vertices v */
/* - return 0 if not hidden */
/* - return 1 if hidden (p[1] greater than corresponding point in v */

short 
hidgridpt( double *p, double *v[3]) {
double vc01, vc12, vc20;

#define V2MINUS( V2, V3, V2OUT) V2OUT[0]=V2[0]-V3[0]; V2OUT[1]=V2[2]-V3[2]
#define V2CROSS( V, VV, VC) VC = ((V[0]*VV[2]) - (V[0]*VV[2]))

double ax, bx, ay, by, det, kx, ky, f1, f2, f21, try;

  /* a triangle can't block its own vertices */

  if ( p == v[0] || p == v[1] || p == v[2]) return(0); /**/

  /* calculate deltas for solving simultaneous equations */

  ax = (double) v[2][0] - v[0][0];
  bx = (double) v[1][0] - v[2][0];
  ay = (double) v[2][2] - v[0][2];
  by = (double) v[1][2] - v[2][2];

  /* determinant */

  det = (by*ax) - (bx*ay);

  if ( det == 0.) return(0); /* singular, colinear vertices */

  kx = (double) p[0] - v[0][0];
  ky = (double) p[2] - v[0][2];

  /* DPR((stderr," - past det==0 test\n")); /**/

  f1 = ((by*kx) - (bx*ky)) / det;
  if ( (f1 < 0.) || (f1 > 1.) ) return(0);

  /* DPR((stderr," - past 0<=f1<=1 test\n")); /**/

  f21 = ((ky*ax) - (kx*ay)) / det;

  /* solve for f2 fraction */

  if ( f1 != 0.) {
    f2 = f21 / f1;
    if ( (f2 < 0.) || (f2 > 1.) ) return(0);

  } else {
    if ( f21 == 0.) f2 = 0.;
    else return (0);

  }

  /* calculate range value for f1,f2 position in triangle */

  try = v[0][1] + ( (v[2][1]-v[0][1])*f1 ) + ( (v[1][1]-v[2][1])*f21 );

  if ( ((double)p[1]) <= try) return(0);
  return(1);
} /* hidgridpt */

/* spudviewhgpc:  hidden grid point check */

void 
spudviewhgpc( SPUDV *spudv, short ibit) {
unsigned long ilo, ihi, imid, iv, siv, ip;
unsigned long i, j, k, F, FF, F0, F1, Fnext, P, PP, npanel, panel;
double *p, *v[3], lox, hix, loz, hiz;
short notibit = ~ibit;
unsigned long numvert = spudf->nv;
unsigned long numface = spudf->nface;

double *sv0StatPtsPtr;

#ifndef MAX
#define MAX(A,B) ((A)>(B)?(A):(B))
#endif

  /* GO ELSEWHERE FOR PERSPECTIVE */

  if ( range > 0.0) { spudviewhgpcPersp( spudv, ibit); return; }

  /* set all vertices visible for now */
  for ( iv=0; iv<numvert; ++iv) vstat[iv] |= ibit;

  /* loop through plates, find which vertices are hidden by each plate */
  for ( ip=0; ip<numface; ++ip) {

    /* dot product of direction to viewer with plate normal should be 
     * positive i.e. emission < 90 deg if this plate is hiding any grid points,
     * so go to next plate if emission >= 90 deg
     */
    if ( VDOT( viewabf, spudf->uvnorms+(3*ip)) <= 0.0) continue;

#   ifdef SVSTATS
    sv0StatPlates += 1.0;
#endif

    v[0] = ICOPTR(lfaceapices[ip]);
    v[1] = ICOPTR(loe[lfaceoeidx[ip]]);
    v[2] = ICOPTR(loe[lfaceoeidx[ip]+1]);

    /* find rectangular range [0] & [2] (x & z) of plate */
    if ( v[0][0] < v[1][0]) {
      if ( v[0][0] < v[2][0]) { lox = v[0][0]; hix = MAX(v[1][0],v[2][0]); }
      else { lox = v[2][0]; hix = v[1][0]; }
    } else {
      if ( v[1][0] < v[2][0]) { lox = v[1][0]; hix = MAX( v[0][0], v[2][0]); }
      else { lox = v[2][0]; hix = v[0][0]; }
    }
    if ( v[0][2] < v[1][2]) {
      if ( v[0][2] < v[2][2]) { loz = v[0][2]; hiz = MAX(v[1][2],v[2][2]); }
      else { loz = v[2][2]; hiz = v[1][2]; }
    } else {
      if ( v[1][2] < v[2][2]) { loz = v[1][2]; hiz = MAX( v[0][2], v[2][2]); }
      else { loz = v[2][2]; hiz = v[0][2]; }
    }

    /* binary search to find leftmost vertex to test 
     * - keep ilo to left of lowest X
     */
    ilo = 0; ihi = numvert - 1; 
    for ( imid = (ilo + ihi) / 2; imid > ilo; ) {
      if ( v0[sortv0[imid]] < lox) ilo = imid;
      else ihi = imid;
      imid = (ilo + ihi) / 2;
    }

#   ifdef INCSVSTAT
#   undef INCSVSTAT
#   endif
#   ifdef SVSTATS
#   define INCSVSTAT (*(sv0StatPtsPtr++))++;
    sv0StatPtsArr[0] += numvert; /* 0 */
#   else
#   define INCSVSTAT
#   endif

    /* step through vertices that may be covered by this plate */
    for ( siv=sortv0[iv=ilo]
        ; v0[siv] <= hix
        ; siv=sortv0[++iv]) { /* ***N.B. extra v0 & sortv0 needed when iv=nv */

#     ifdef SVSTATS
      sv0StatPtsPtr = sv0StatPtsArr + 1;
#     endif

      INCSVSTAT /* 1 */
      p = ICOPTR(siv);
      if ( p[2] <= hiz && p[2] >= loz) {
        INCSVSTAT /* 2 */
        if ( vstat[siv] & ibit) { /* not already hidden? */

          INCSVSTAT /* 3 */
          if ( hidgridpt( p, v)) {
            INCSVSTAT /* 4 */
            vstat[siv] &= notibit;
          }
        } /* if vstat[siv] & ibit */
      } /* if p[2] <= hiz && p[2] >= loz ... */

    } /* for siv=sortv0[iv=ilo] ... */

  } /* for ip ... */

  return;
} /* spudviewhgpc */

/**********************************************************************/
/* spudview1:  - rotate shape using matrix in argument
 *             - check for hidden grid points after rotation
 */

void 
spudview1( SPUDV *spudv, double MTXR[4][4]) {
unsigned long i, S;
unsigned long *joe, *koe;
unsigned long numvert = spudf->nv;
short *pst, *vst, *sst;

  spudview0( spudv, MTXR, (int) 1);
  bzero( (void *)vstat, (int)(numvert * sizeof(short)));
  spudviewhgpc( spudv, (short) 1);

  /* translate vstat into segstat */

  for (vst=vstat, S= 0; S < (numvert - 1); ++S, ++vst) {
    i = loeidx[S];
    joe = loe + loeidx[S+1];
    sst = segstat + i;
    if ( *vst) {
      for (koe=loe+i; koe<joe; ++koe, ++sst) 
        *sst = (*vst & vstat[*koe]);
    } else {
      for (koe=loe+i; koe<joe; ++koe, ++sst) 
        *sst = 0;
    }
  }

  /* translate vstat into pstat */

  for ( pst=pstat, S=0; S < spudf->nface; ++S, ++pst) {
  short *v1, *v2, *v3;
    v1 = vstat + lfaceapices[S];
    v2 = vstat + loe[lfaceoeidx[S]];
    v3 = vstat + loe[lfaceoeidx[S]+1];
    *pst = (*v1 & (*v2 & *v3));
  }

/* DPR((stderr,"leaving spudview1 // ")); /**/
  return;
} /* spudview1 */

/**********************************************************************/
/* spudview2:  - rotate shape to Sun view, then camera view
 *             - check for hidden grid points after each rotation
 */

void 
spudview2( SPUDV *spudv) {
unsigned long i, S;
unsigned long *joe, *koe;
unsigned long numvert = spudf->nv;
short *vst, *sst, *pst;
double savRange;

  savRange = range;
  range = 0.0;
  spudview0( spudv, SR, (int) 1);
  bzero( (void *)vstat, (int)(numvert * sizeof(short)));
  spudviewhgpc( spudv, (short) SPUDV_SHAD);

  range = savRange;
  spudview0( spudv, VR, (int) 1);
  spudviewhgpc( spudv, (short) SPUDV_HID);

  /* translate vstat into segstat */

  for (vst=vstat, S= 0; S < (numvert - 1); ++S, ++vst) {
    i = loeidx[S];
    joe = loe + loeidx[S+1];
    sst = segstat + i;
    if ( *vst == SPUDV_SHADHID) {
      for (koe=loe+i; koe<joe; ++koe, ++sst) 
        *sst = (SPUDV_SHADHID == (SPUDV_SHADHID & vstat[*koe]));
    } else {
      for (koe=loe+i; koe<joe; ++koe, ++sst) 
        *sst = 0;
    }
  }

  /* translate vstat into pstat */

  for ( pst=pstat, S=0; S < spudf->nface; ++S, ++pst) {
  short *v1, *v2, *v3;
    v1 = vstat + lfaceapices[S];
    v2 = vstat + loe[lfaceoeidx[S]];
    v3 = vstat + loe[lfaceoeidx[S]+1];
    *pst = (SPUDV_SHADHID == (SPUDV_SHADHID & (*v1 & (*v2 & *v3))));
  }

  /* DPR((stderr,"leaving spudview2 // ")); /**/
  return;
} /* spudview2 */

/**********************************************************************/
/* spudviewPersp:  general spud viewing routine, uses perspective 
 *                 for view from viewer is spudv->_range != 0.0
 */

void 
spudviewPersp( SPUDV *spudv) {

# if 0
  if ( spudf->_spudf2) {
    spudview_spudf2_( spudv);
    return;
  }
# endif
 
  if ( hidden==SPUDV_SHADHID) {    /* hidden gridpoints + shadowed gridpoints */
    spudview2( spudv);

  } else if ( hidden==SPUDV_SHAD) {                    /* shadowed gridpoints */
  double savRange = range;
    range = 0.0;      spudview1( spudv, SR);        /* assume sun at infinity */
    range = savRange; spudview0( spudv, VR, (int) 0); /*rotate to view coord's*/

  } else if ( hidden==SPUDV_HID) {                       /* hidden gridpoints */
    spudview1( spudv, VR);

  } else {                 /* rotate only, no hidden or shadowed gridpt check */
    spudview0( spudv, VR, (int) 0);
  }

/* DPR((stderr,"leaving spudview // ")); /**/
  return;
} /* spudviewPersp */

/* spudview - disable perspective */

void 
spudview( SPUDV *spudv) {
double savRange = spudv->_range;
  spudv->_range = 0.0;
  spudviewPersp( spudv);
  spudv->_range = savRange;
  return;
} /* spudview */

#define DOALLSPUDV \
 FREEORMALLOC( xyz, double, 3 * spudf->nv) \
 FREEORMALLOC( vstat, short, spudf->nv) \
 FREEORMALLOC( pstat, short, spudf->nface) \
 FREEORMALLOC( segstat, short, spudf->nseg) \
 \
 /* extra index in v0 & sortv0 so no test required  \
  * at end of siv=sortv0[iv] loop in spudview_hgpc above \
  */ \
 FREEORMALLOC( v0, double, (spudf->nv + 1)) \
 FREEORMALLOC( sortv0, long int, (spudf->nv + 1)) \
 FREEORMALLOC( valt, double, spudf->nv)
#if 0
 FREEORMALLOC( sortfmin0, long int, spudf->nface) \
 FREEORMALLOC( facemin0, double, spudf->nface) \
 FREEORMALLOC( facemin1, double, spudf->nface)
#endif

/****************************************************/
/* spud_freeSpudv( SPUDV *):   free spudv structure */

#define FREEORMALLOC( PTR, CAST, COUNT) \
  if ( PTR) { free( PTR); PTR = (CAST *) 0; }

void
spud_freeSpudv( SPUDV *spudv) {

  if ( !spudv) return;

  DOALLSPUDV
  FREEORMALLOC( spudv, SPUDV, 1)               /* free spudv last - see below */

  return;
} /* spud_freeSpudv */

/******************************************************************/
/* newspudv( SPUDF *):  make spudv structure from spudf structure */

#undef FREEORMALLOC
#define FREEORMALLOC( PTR, CAST, COUNT) \
  PTR = (CAST *) malloc( COUNT * sizeof(CAST));

SPUDV *
newspudv( SPUDF *spudfptr) {
SPUDV *spudv;

 FREEORMALLOC( spudv, SPUDV, 1)  /* need to malloc spudv separately so it can */
 spudf = spudfptr;               /* go before this line and last in freeSpudv */
 DOALLSPUDV

 sortv0[spudf->nv] = spudf->nv; /* point to v0[nv] which will */
                                /* have greatest value in v0  */

 if ( !spudf->uvnorms) spudf_calcs( spudf);

 return( spudv);
} /* newspudv */

