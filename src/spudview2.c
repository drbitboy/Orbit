#include <stdio.h>
#include <string.h>
#include <math.h>
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

#define spudf2 spudf->_spudf2
#define plateVerts spudf2->_plateVerts
#define ivV0 spudf2->_ivV0
#define coneOVis spudf2->_coneOVis
#define coneOSilence spudf2->_coneOSilence

#define VR spudv->_VR
#define SR spudv->_SR
#define hidden spudv->_hidden
#define persp spudv->_persp
#define xyz spudv->_xyz
#define segstat spudv->_segstat
#define vstat spudv->_vstat
#define pstat spudv->_pstat
#define v0 spudv->_v0
#define sortv0 spudv->_sortv0
#define range spudv->_range
#define viewabf spudv->_viewabf

/* spudview_spudf2_0 - rotate shape model by matrix
 *           - sort for hidden gridpt check if required
 *             - assume all vstat ibit bits set (to 1 i.e. visible) before call
 */

void 
spudview_spudf2_0( SPUDV *spudv, double MTXR[4][4], short ibit) {
/* ibit==0:  rotate only; 
 * ibit!=0:  rotate and sort for hidden gridpoint check
 */
unsigned long i, j, k;
long S;
double bigval;
double xtmp, ytmp, dx, dy;
double xtmp1, ytmp1;
unsigned long numvert;
short notIbit = ~ibit;

  if ( ibit) {

    /* rotate viewing object (0,-range,0) to ABF for emission chk later */

    /* HACK1:  perspective disallowed for now */
    range = 0.0; /* mimic infinity, i.e. eliminate any attempt at perspective */
    if ( range == 0.0) { VNEG2( MTXR[1], viewabf); }/* infinity, use unit vec */
    else { VSCAL2( -range, MTXR[1], viewabf); }
  }

  if ( ibit) { 
  double *lv0 = v0;
  unsigned long *livV0 = ivV0;
  double *coneOVisPtr = coneOVis;
  double *coneOSilencePtr = coneOSilence;
  double *basexyz, *spudvxyz;
    numvert = 0;
    basexyz = spudf->Rxyz;
    spudvxyz = xyz;
    for (S=0; S < spudf->nv
        ; S++, basexyz += 3, spudvxyz += 3, coneOVisPtr+=4, coneOSilence+=4) { 

      /* if vertex is inside cone of darkness, it must be occluded */

      if ( VDOT( viewabf, coneOSilencePtr) >= coneOSilencePtr[3]) {
        vstat[S] &= notIbit;
      } else {
        for (i= 0; i < 3; ++i) {
        double *mtxr;
          mtxr = MTXR[i]; 
          spudvxyz[i] = VDOT(mtxr,basexyz); 
        }

        /* if vertex is inside cone of visibility, it may be visible, so 
         * put it into sortv0 list
         */

        if ( VDOT( viewabf, coneOVisPtr) <= coneOSilencePtr[3]) {
          numvert++;
          *(lv0++) = *spudvxyz;     /* put projected X into v0 for sort below */
          *(livV0++) = S;
        }
      }
    }
    indexx( &numvert, v0, sortv0);     /* v0[sortv0] are vertices sorted by X */

    /* put dummy X value in v0[nv] that is larger than real largest X
     * & put numvert into extra element of ivV0
     */
    bigval = v0[sortv0[numvert-1]];
    *lv0 = bigval + ((bigval == 0.0) ? 1.0 : fabs(bigval));
    ivV0[spudf->nv] = numvert;

  } else {
  double *basexyz, *spudvxyz;
    basexyz = spudf->Rxyz;
    spudvxyz = xyz;
    for (S=0; S < spudf->nv; S++, basexyz += 3, spudvxyz += 3) {
      for (i= 0; i < 3; i+= 2) {                      /* only compute x and z */
      double *mtxr;
        mtxr = MTXR[i];
        spudvxyz[i] = VDOT(mtxr,basexyz);
      }
    }
  }
  return;
}

#ifndef MAX
#define MAX(A,B) ((A)>(B)?(A):(B))
#endif

#define ICO(S,I) xyz[(S)*3+I]        
#define ICOPTR(S) (xyz+((S)*3))    

/* spudview_spudf2_hgpc:  hidden grid point check */

void 
spudview_spudf2_hgpc( SPUDV *spudv, short ibit) {
unsigned long ilo, ihi, imid, iv, siv, ip;
unsigned long i, j, k, F, FF, F0, F1, Fnext, P, PP, npanel, panel;
double *p, *v[3], lox, hix, loz, hiz;
VEC vabf;
short notIbit = ~ibit;
unsigned long numvert = ivV0[spudf->nv];
unsigned long numface = spudf->nface;
unsigned long *pV;

  /* if ( range == 0.0) /**/ { CPYVEC( viewabf, vabf); }

  /* loop through plates, find which vertices are hidden by each plate */
  pV = plateVerts;
  for ( ip=0; ip<numface; ++ip) {
  unsigned long i0, i1, i2;

    i0 = *(pV++);
    i1 = *(pV++);
    i2 = *(pV++);

    if ( (((ibit & vstat[i0]) & vstat[i1]) & vstat[i2]) ) {

    v[0] = ICOPTR(i2);

    /* get vector from first vertex to viewer, dot product of that 
     * with normal should be positive to continue i.e. emission < 90
     */
    /* HACK1:  perspective disallowed for now */
    /* if ( range > 0.0) { VMINUS2( viewabf, v[0], vabf); } /**/

    if ( VDOT( vabf, spudf->uvnorms+(3*ip)) > 0.0) {

    v[1] = ICOPTR(i1);
    v[2] = ICOPTR(i2);

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

    /* step through vertices that may be covered by this plate */
    for ( siv=sortv0[iv=ilo]
        ; v0[siv] <= hix
        ; siv=sortv0[++iv]) { /* ***N.B. extra v0 & sortv0 needed when iv=nv */
      i0 = ivV0[siv];
      if ( vstat[i0] & ibit) { /* not already hidden? */
        p = ICOPTR(i0);
        if ( p[2] <= hiz && p[2] >= loz) {

          if ( hidgridpt( p, v)) vstat[i0] &= notIbit;

        } /* if p[2] <= hiz && p[2] >= loz ... */
      } /* if vstat[i0] & ibit */

    } /* for siv=sortv0[iv=ilo] ... */

    } /* if VDOT(vabf,->uvnorms) > 0.0 ... */
    } /* if ( (((ibit & vstat[i0]) & vstat[i1]) & vstat[i2]) ) ... */
  } /* for ip ... */

  return;
}

/**********************************************************************/
/* spudview_spudf2_1:  - rotate shape using matrix in argument
 *                     - check for hidden grid points after rotation
 */

void
spudview_spudf2_1( SPUDV *spudv, double MTXR[4][4], short ibit) {
unsigned long i, S;
unsigned long *joe, *koe;
unsigned long numvert = spudf->nv;
unsigned long *pV;
short *pst, *vst, *sst;

  for ( vst=vstat, S=0; S<numvert; ++S) *vst = ibit;

  spudview_spudf2_0( spudv, MTXR, ibit);
  spudview_spudf2_hgpc( spudv, ibit);

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

  for ( pV=plateVerts, pst=pstat, S=0; S < spudf->nface; ++S, ++pst) {
  short *v1, *v2, *v3;
    v1 = vstat + *(pV++);
    v2 = vstat + *(pV++);
    v3 = vstat + *(pV++);
    *pst = (*v1 & (*v2 & *v3));
  }

/* DPR((stderr,"leaving spudview_spudf2_1 // ")); /**/
  return;
}

/**********************************************************************/
/* spudview_spudf2_2:  - rotate shape to Sun view, then camera view
 *             - check for hidden grid points after each rotation
 */

void
spudview_spudf2_2( SPUDV *spudv) {
unsigned long i, S;
unsigned long *joe, *koe;
unsigned long numvert = spudf->nv;
unsigned long *pV;
short *vst, *sst, *pst;

  for ( vst=vstat, S=0; S<numvert; ++S) *vst = SPUDV_SHADHID;

  spudview_spudf2_0( spudv, SR, SPUDV_SHAD);
  spudview_spudf2_hgpc( spudv, SPUDV_SHAD);
  spudview_spudf2_0( spudv, VR, SPUDV_HID);
  spudview_spudf2_hgpc( spudv, SPUDV_HID);

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

  for ( pV=plateVerts, pst=pstat, S=0; S < spudf->nface; ++S, ++pst) {
  short *v1, *v2, *v3;
    v1 = vstat + *(pV++);
    v2 = vstat + *(pV++);
    v3 = vstat + *(pV++);
    *pst = (SPUDV_SHADHID == (SPUDV_SHADHID & (*v1 & (*v2 & *v2))));
  }

  /* DPR((stderr,"leaving spudview_spudf2_2 // ")); /**/
  return;
}

/**********************************************************************/
/* spudview_spudf2_:  general spud viewing routine
 */

void 
spudview_spudf2_( spudv)
SPUDV *spudv; 
{
/* DPR(( stderr, " ... in spudview_spudf2_, spudv = %16x, hidden = %d / %d "
    , spudv, hidden, spudv->_hidden)); /**/
 
  if ( hidden==SPUDV_SHADHID) {    /* hidden gridpoints + shadowed gridpoints */
    spudview_spudf2_2( spudv);

  } else if ( hidden==SPUDV_SHAD) {                    /* shadowed gridpoints */
    spudview_spudf2_1( spudv, SR, SPUDV_SHAD);
    spudview_spudf2_0( spudv, VR, 0);          /* - rotate to viewing coord's */

  } else if ( hidden==SPUDV_HID) {                       /* hidden gridpoints */
    spudview_spudf2_1( spudv, VR, SPUDV_HID);

  } else {                 /* rotate only, no hidden or shadowed gridpt check */
    spudview_spudf2_0( spudv, VR, 0);
  }

/* DPR((stderr,"leaving spudview_spudf2_ // ")); /**/
  return;
}
