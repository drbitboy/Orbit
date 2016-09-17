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

enum { TESTED_NOTYET, TESTED_OUTSIDE, TESTED_MAXOUTSIDE };

#define nvert spudf->nv
#define np spudf->nface
#define spudf ((SPUDF *)(spudf2->_spudf))
#define plateVerts spudf2->_plateVerts
#define adjPlates spudf2->_adjPlates
#define coneOSilence spudf2->_coneOSilence
#define coneOVis spudf2->_coneOVis
#define ivV0 spudf2->_ivV0

#define UNORM( RECIP, V) RECIP = 1.0 / VLEN(V); VSCAL( RECIP, V)

/* adjust cone for new vector IF vector is inside cone */

void
calcVisConeUpdate( double *uv, double *outCone) {
double cosineNew, sineNew, sineOpp, new2Opp;
#define cosineOpp outCone[3]
VEC oldAxisNew;             /* vector on old center axis, length is cosineNew */
VEC oldAxisOpp;             /* vector on old center axis, length is cosineOpp */
VEC oldAxis2New;                                     /* from oldAxisNew to uv */
VEC oldAxis2Opp;                                  /* from oldAxisOpp to uvOpp */
VEC uvOpp;                          /* vector on old cone opposite new vector */
VEC newAxis;                              /* new cone axis, splits uv & uvOpp */
VEC uvCross;
double newAxisLen, newAxisLenRecip;

  if ( (cosineNew=VDOT( uv, outCone)) <= outCone[3]) return;

  VSCAL2( cosineNew, outCone, oldAxisNew);
  VSCAL2( cosineOpp, outCone, oldAxisOpp);
  sineNew = sqrt( 1.0 - (cosineNew*cosineNew));
  VMINUS2( uv, oldAxisNew, oldAxis2New);
  sineOpp = sqrt( 1.0 - (cosineOpp*cosineOpp));
  new2Opp = - sineOpp / sineNew;
  VSCAL2( new2Opp, oldAxis2New, oldAxis2Opp);
  VADD2( oldAxisOpp, oldAxis2Opp, uvOpp);
  VADD2( uvOpp, uv, newAxis);
  VSCAL( 0.5, newAxis);
  newAxisLen = VLEN( newAxis);

  if ( newAxisLen != 0.0) {
    newAxisLenRecip = ( (VDOT(newAxis,outCone)>0.0) ? 1.0 : -1.0 ) / newAxisLen;
    VSCAL2( newAxisLenRecip, newAxis, outCone);
    outCone[3] = newAxisLen;

  /* special case:  uv and uvOpp are opposing vectors ... */
  } else {        
    ucrss( uv, outCone, uvCross);                 /* cross uv and old ctr ... */
    ucrss( uvOpp, uvCross, outCone);/* cross result with uvOpp to get new ctr */
    outCone[3] = 0.0;
  }
  return;
}

/* find largest cone that does not enclose any of a set of points
 * Apex of cone = origin
 * UVi = unit vectors toward points, all will be on or outside cone
 * Axis = axis of cone
 * OutVec = "outward" facing vector, Axis dot OutVec > 0
 * Ang = cosine of angle of cone (from Axis to surface)
 *
 * - three unit vectors UVa, UVb, UVc, plus the apex will determine a cone
 * - plane through unit vector endpoints will be perpendicular to Axis
 * - since UVa/b/c are all distance 1 away from apex, plane's intersection 
 *   with cone will be circle with Axis pointing at (or away from) its center
 */
void 
calcVisConeMulti( double *uv, unsigned long nVec, VEC outUVec, double *outCone){
VEC v01, v02;
double *v0, *v1, *v2;
VEC uCross;
double dotP;
double dotPOut;
unsigned long i0, i1, i2;
  
  CPYVEC( outUVec, outCone);
  outCone[3] = -1.0;

  for ( i0=0; i0<(nVec-2); ++i0) {
    v0 = uv + (3*i0);
    for ( i1=i0+1; i1<(nVec-1); ++i1) {
      v1 = uv + (3*i1);
      VMINUS2( v1, v0, v01);
      for ( i2=i1+1; i2<nVec; ++i2) {
        v2 = uv + (3*i2);
        VMINUS2( v2, v0, v02);
        ucrss( v01, v02, uCross);
        dotPOut = VDOT( uCross, outUVec);
        if ( dotPOut < 0.0) { dotPOut = - dotPOut; VSCAL( -1.0, uCross); }
        dotP = VDOT( uCross, v2);
        if ( dotP > outCone[3] && dotPOut > dotP) { 
          CPYVEC( uCross, outCone); 
          outCone[3] = dotP;
  } } } }
  return;
}

#define TESTMALLOCINIT( VAR, TYPE, N, IVAL) \
  TESTMALLOC( VAR, TYPE, N); \
  for ( i=0; i<(N); ++i) VAR[i] = (TYPE) IVAL

#define TESTMALLOC( VAR, TYPE, N) \
  if ( nmalloc == MAXMALLOC) { \
    FREEALL((stderr,"newSpudf2:  out of mallocs space; contact Programmer\n"));\
    return BADRTNVAL; \
  } \
  if ( !(mallocs[nmalloc] = (void *) (MALLOC( VAR, TYPE, N)))) { \
    FREEALL( (stderr, "Problem allocating space\n")); \
    return BADRTNVAL; \
  } \
  nmalloc++

#define MALLOC( VAR, TYPE, N) VAR = (TYPE *) malloc( (N) * sizeof( TYPE))

#define MAXMALLOC 40
#define FREEALL( A) \
  FREEOTHER; \
  fprintf A; while( nmalloc) free( mallocs[--nmalloc])

#define BADRTNVAL (SPUDF2 *) 0

#define ADDNHOE( V0, V1) nHoe[((V0)<(V1))?(V0):(V1)]++

#define MATCHHOE( V0, V1) \
  loVtx = ((V0)<(V1)) ? (V0) : (V1); \
  hiVtx = (loVtx != (V0)) ? (V0) : (V1); \
  for ( hoePtr=hoeIdxPtr[loVtx]; *hoePtr!=hiVtx && *hoePtr!=nvert; hoePtr++) { \
    if ( hoePtr == hoeIdxPtr[loVtx+1]) { \
      fprintf( stderr, "newSpudf2:  Program error; contact programmer, %s\n" \
                     , "code WSNBATGH=0; exiting..."); \
      fflush( stderr); \
      exit(-1); \
    } \
  }

/* put current plate in current hoePlate list */

#define SETADJ( V0, V1) \
  MATCHHOE( V0, V1) \
  if ( *hoePtr == nvert) {                               /* no matching hoe */ \
    *hoePtr = hiVtx;                            /* make this a matching hoe */ \
    hoePlate[hoePtr-hoe] = ip;/* and save this plate as adjacent to this seg */\
  \
  } else {   /* found common seg, save plates in each other's adjacency list */\
  unsigned long thatPlate; \
  unsigned long *thatUlPtr, *thatAdjPtr; \
    thatPlate = hoePlate[hoePtr-hoe];   /* get plate adjacent to common seg */ \
    \
    if ( thatPlate == np) {           /* we've been here before (see below) */ \
      fprintf( stderr \
             , "newSpudf2:  Program error; contact programmer, %s\n" \
             , "code WSNBATGH=1; exiting..."); \
      fflush( stderr); \
      exit( -2); \
    } \
    thatUlPtr = plateVerts + (3*thatPlate); \
    thatAdjPtr = adjPlates + (3*thatPlate); \
    \
    *adjPtr = thatPlate;  /* save other plate int this plates adj. list */ \
    \
    /* ... then save this plate in that plate's adjacency list - one of */ \
    /*     three positions:  0 => common segment is 0->1 or 1->0 */ \
    /*                       1 => common segment is 0->2 or 2->0 */ \
    /*                       2 => common segment is 1->2 or 2->1 */ \
    \
    if ( loVtx == thatUlPtr[0]                              /* 0->1 or 0->2 */ \
      || hiVtx == thatUlPtr[0]) {                           /* 1->0 or 2->0 */ \
      if ( hiVtx == thatUlPtr[1] || loVtx == thatUlPtr[1]) thatAdjPtr[0] = ip; \
      else thatAdjPtr[1] = ip; \
    } else thatAdjPtr[2] = ip;    /* 1->2 or 2->1 */ \
    \
    /* set adjacent plate out of range to force error if we come here again */ \
    hoePlate[hoePtr-hoe] = np; \
  } \
  adjPtr++;


/* add point to vList at vNext */

#define ADDPT(IV) \
  if ( !vLevel[IV]) { \
    *(vNext++) = IV; \
    iNext++; \
    vLevel[IV] = currLevel; \
    xyzPtr = spudf->Rxyz + (3*(IV)); \
    VMINUS2( xyzPtr, xyzBasePtr, uVNextPtr); \
    UNORM( r, uVNextPtr); \
    uVNextPtr += 3; \
    vMaxPtr = vMax + (3*(IV)); \
    VMINUS2( vMaxPtr, xyzBasePtr, uVMaxNextPtr); \
    UNORM( r, uVMaxNextPtr); \
    uVMaxNextPtr += 3; \
  }

#ifdef FREEOTHER
#undef FREEOTHER
#endif
#define FREEOTHER

SPUDF2 *
mallocSpudf2( SPUDF *spudfPtr) {
void *mallocs[MAXMALLOC];
int nmalloc = 0;
SPUDF2 *spudf2 = (SPUDF2 *) 0;

  TESTMALLOC( spudf2, SPUDF2, 1);
  spudf2->_spudf = (void *) spudfPtr;
  TESTMALLOC( plateVerts, unsigned long, np*3);
  TESTMALLOC( adjPlates, unsigned long, np*3);
  TESTMALLOC( coneOSilence, double, nvert*4);
  TESTMALLOC( coneOVis, double, nvert*4);
  TESTMALLOC( ivV0, unsigned long, nvert+1);
  spudfPtr->_spudf2 = spudf2;
  return spudf2;
}

void
freeSpudf2( SPUDF2 *spudf2) {

  free( plateVerts);
  free( adjPlates);
  free( coneOSilence);
  free( coneOVis);
  free( ivV0);
  if ( spudf2->_spudf)
    if ( spudf->_spudf2 == spudf2)
      spudf->_spudf2 = (SPUDF2 *) 0;
  free( spudf2);
  return;
}

SPUDF2 *newSpudf2( SPUDF *spudfPtr) {
SPUDF2 *spudf2;
void *mallocs[MAXMALLOC];
int nmalloc = 0;
unsigned long i, ii;
unsigned long *ulPtr;
double r, maxR;
double *dPtr;
unsigned long *hoe;           /* [np*3/2] other ends, grouped by lower vertex */
unsigned long *hoePtr;
unsigned long *hoePlate;
unsigned long *nHoe;                         /* [nv] number of hoe per vertex */
unsigned long **hoeIdxPtr;          /* [nv+1] pointer to first hoe for vertex */
unsigned long loVtx, hiVtx, iv, iv2, ip;
unsigned long *adjPtr;
unsigned long *ppv, *ppvPtr, *nPpv, **ppvIdxPtr;

double *xyzBasePtr, *xyzPtr, *uVBase, *uVBasePtr, *coneBasePtr;
double *uV, *uVCurrPtr, *uVNextPtr;
double *uVMax, *uVMaxCurrPtr, *uVMaxNextPtr;
double *vMax, *vMaxPtr;
unsigned long *vLevel, *vLevelPtr, *vList, *vListPtr, *vNext, *vCurr;
VEC uVBaseNeg;
double cone[4];
unsigned long nVec, iCurr, iNext, currLevel;

  spudf2 = mallocSpudf2( spudfPtr);
  if ( !spudf2) return spudf2;

# ifdef FREEOTHER
# undef FREEOTHER
# endif
# define FREEOTHER freeSpudf2( spudf2)
 
  TESTMALLOCINIT( hoe, unsigned long, np*3/2, nvert);       /* High Other End */
  TESTMALLOCINIT( nHoe, unsigned long, nvert, 0);
  TESTMALLOC( hoeIdxPtr, unsigned long *, nvert+1);         /* pointer to hoe */
  TESTMALLOCINIT( hoePlate, unsigned long, np*3/2, np);

  TESTMALLOC( ppv, unsigned long, np*3);            /* Plates for each vertex */
  TESTMALLOCINIT( nPpv, unsigned long, nvert, 0);
  TESTMALLOC( ppvIdxPtr, unsigned long *, nvert+1);         /* pointer to ppv */

  TESTMALLOC( uVBase, double, nvert*3);/* unit vector from center toward vert */
  TESTMALLOC( uV, double, nvert*3); /* unit vector from base vert toward vert */
  TESTMALLOC( vMax, double, nvert*3);      /* vert extended out to max radius */
  TESTMALLOC( uVMax, double, nvert*3);   /* uv from base vert toward max vert */
  TESTMALLOC( vLevel, unsigned long, nvert);   /* where vert is wrt base vert */
  TESTMALLOC( vList, unsigned long, nvert);      /* list of verts around base */

  /* determine max radius, scale all vertices to unit vectors */

  xyzBasePtr = spudf->Rxyz;
  uVBasePtr = uVBase;
  vMaxPtr = vMax;
  maxR = 0.0;
  for ( iv=0; iv<nvert; ++iv) {
    r = VLEN( xyzBasePtr);
    if ( maxR < r) maxR = r;
    r = 1.0 / r;
    VSCAL2( r, xyzBasePtr, uVBasePtr);
    xyzBasePtr += 3;
    uVBasePtr += 3;
    vMaxPtr += 3;
  }

  /* scale all vertices (saved in vMax as unit vectors) out to max radius */

  vMaxPtr = vMax;
  uVBasePtr = uVBase;
  for ( iv=0; iv<nvert; ++iv) {
    VSCAL2( maxR, uVBasePtr, vMaxPtr);
    uVBasePtr += 3;
    vMaxPtr += 3;
  }

  /* for each plate,
   * (1) put verts of plate into plateVerts
   * (2) track number of hoe for each vert
   * (3) increment number of plates of which each vert is a part
   */
  for ( ulPtr=plateVerts, ip=0; ip<np; ++ip) {
    *ulPtr = spudf->faceapices[ip];                                    /* (1) */
    ulPtr[1] = spudf->oe[spudf->faceoeidx[ip]];
    ulPtr[2] = spudf->oe[spudf->faceoeidx[ip]+1];
    ADDNHOE( ulPtr[0], ulPtr[1]);   /* (2) ***N.B. each segment is duplicated */
    ADDNHOE( ulPtr[0], ulPtr[2]);
    ADDNHOE( ulPtr[1], ulPtr[2]);
    nPpv[*(ulPtr++)]++;                                                /* (3) */
    nPpv[*(ulPtr++)]++;
    nPpv[*(ulPtr++)]++;
  }

  /* set pointers in hoeIdxPtr & ppvIdxPtr into hoe & ppv, respectively
   * - halve nHoe to remove duplicates
   */
  *hoeIdxPtr = hoe;
  *ppvIdxPtr = ppv;
  for ( iv=0; iv<nvert; ++iv) {
    hoeIdxPtr[iv+1] = hoeIdxPtr[iv] + (nHoe[iv] / 2);
    ppvIdxPtr[iv+1] = ppvIdxPtr[iv] + nPpv[iv];
  }

  /* fill out adjacencies & plates per vertex */

  ulPtr = plateVerts;
  adjPtr = adjPlates;
  for ( ip=0; ip<np; ++ip, (ulPtr+=3)) {               /* step through plates */

    SETADJ( ulPtr[0], ulPtr[1])                            /* set adjacencies */
    SETADJ( ulPtr[0], ulPtr[2])
    SETADJ( ulPtr[1], ulPtr[2])

    /* fill plates per vertex array */

    for ( i=0; i<3; ++i) *(ppvIdxPtr[ulPtr[i]+1] - (nPpv[ulPtr[i]]--)) = ip;
  }

  for ( iv=0; iv<nvert; ++iv) { /* step through all vertices; iv is base vert */
    
    vLevelPtr = vLevel;
    for ( iv2=0; iv2<nvert; ++iv2) *(vLevelPtr++) = 0;   /* zero vLevel array */

    *vList = iv;                                   /* add base vertex to list */
    vLevel[iv] = 1;                                          /* add its level */
    xyzBasePtr = spudf->Rxyz + (3*iv);          /* pointer to base vertex xyz */
    uVBasePtr = uVBase + (3*iv);       /* pointer to base vertex unit vec xyz */
    coneBasePtr = spudf2->_coneOVis + (4*iv);

    vCurr = vNext = vList + 1;        /* starting point for add'l vertices */
    iCurr = iNext = 1;
    uVCurrPtr = uVNextPtr = uV + 3;             /* get ready for add'l points */
    uVMaxCurrPtr = uVMaxNextPtr = uVMax + 3;
    currLevel = 2;

    /* add level 2 vertices i.e. those surrounding base vertex */

    for ( ppvPtr = ppvIdxPtr[iv]; ppvPtr < ppvIdxPtr[iv+1]; ++ppvPtr) {
      ip = *ppvPtr;
      ulPtr = plateVerts + (3*ip);
      ADDPT( *ulPtr) ulPtr++;
      ADDPT( *ulPtr) ulPtr++;
      ADDPT( *ulPtr)
    }

    /* get the outward cone that excludes all the level 2 vertices */

    nVec = iNext - iCurr;
    calcVisConeMulti( uVCurrPtr, nVec, uVBasePtr, coneBasePtr);

    /* do the same for the inward cone */

    VSCAL2( -1.0, uVBasePtr, uVBaseNeg);
    calcVisConeMulti( uVCurrPtr, nVec, uVBaseNeg, coneOSilence+(4*iv));

    /* loop through current level, for current level's maxVerts that are 
     * inside the current cone, add verts that are part of plates 
     * that have current level's verts in them, adjust coneOVis, 
     * quit when no more verts have been added
     */
    while ( nVec) {
      currLevel++;
      for ( ; nVec; --nVec) {

        /* if vCurr is inside cone when projected to max radius, 
         * add verts in plates containing vCurr
         */
        if ( VDOT(uVMaxCurrPtr,coneBasePtr) > coneBasePtr[3]) for 
          ( ppvPtr=ppvIdxPtr[*vCurr]; ppvPtr<ppvIdxPtr[(*vCurr)+1]; ++ppvPtr) {
          ulPtr = plateVerts + (3 * (*ppvPtr));
          ADDPT( *ulPtr) ulPtr++;
          ADDPT( *ulPtr) ulPtr++;
          ADDPT( *ulPtr)
        }

        /* use current pointer to update cone */

        calcVisConeUpdate( uVCurrPtr, coneBasePtr);
        ++vCurr;
        ++iCurr;
        uVCurrPtr += 3;
        uVMaxCurrPtr += 3;
      }
      nVec = iNext - iCurr;
    }

    if ( (iv % 500) == 499) { fprintf( stderr, "."); fflush( stderr); }

  } /* for iv=0; iv<nvert ... */

  fprintf( stderr, "\n"); fflush( stderr);

  while (nmalloc) free( mallocs[--nmalloc]);
  return spudf2;
}

#if 0   /* getSpudvByname moved to spudface.c or spudview.c */

#ifdef BADRTNVAL
#undef BADRTNVAL
#endif
#define BADRTNVAL -1

SPUDV *getSpudvByname( char *fnShape) {
SPUDR *spudr;
SPUDF *spudfPtr;
SPUDV *spudv;

static SPUDF staticSpudf;
static SPUDR staticSpudr;

char *fnActual;

  /* start by trying to read plate model from fnShape, file getenv("PLATES") 
   * or plates
   */
  if ( !(spudfPtr = getplatByname( (SPUDR *) 0, fnShape, &fnActual))) {

    /* failed to read plate model, try spud model from fnShape, 
     * getenv("VIEW") or view
     */

    spudr = (SPUDR *) malloc( sizeof( SPUDR));
    *spudr = staticSpudr;
    spudr->nlatR = SPUDRNLAT;
    spudr->nlonR = SPUDRNLON;
    spudr->eastlon = 
      getviewByname( spudr->Rmodel[0], &spudr->nlatR, &spudr->nlonR, fnShape
                   , &fnActual);
    if ( spudr->eastlon == -1) {
      fprintf( stderr, "Failed to load shape model\n");
      return (SPUDV *) 0;
    }
    if ( !spudr->eastlon) spudr->eastlon = -1;

    spudfPtr = (SPUDF *) malloc(sizeof( SPUDF));/* convert spud to facet model*/
    *spudfPtr = staticSpudf;

    rmod2face( spudr, spudfPtr);

  } /* if !(spudfPtr = getplatByname) */

  spudv = newspudv( spudfPtr);           /* convert facet model to view model */

  return spudv;
}

#endif /* #if 0 */
