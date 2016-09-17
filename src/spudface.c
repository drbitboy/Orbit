/* spudface.c */

#include "debug.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#ifndef vms
#include <malloc.h>
#else
char *malloc();
int exit();
#endif

#include "spudshap.h"
#include "SpiceUsr.h"

#define lnv spudf->nv
#define lnface spudf->nface
#define lnseg spudf->nseg
#define lRxyz spudf->Rxyz
#define lnlat spudf->nlat
#define llats spudf->lats
#define llatidx spudf->latidx
#define lnlon spudf->nlon
#define loe spudf->oe
#define loeidx spudf->oeidx
#define lfaceapices spudf->faceapices
#define lfaceoeidx spudf->faceoeidx
#define lplatenum spudf->platenum
#define lplatecolor spudf->platecolor
#define luvnorms spudf->uvnorms
#define lmidpts spudf->midpts
#define lr2 spudf->r2
#define leastlon spudf->eastlon
#define lspudf2 spudf->_spudf2

static double rpd;
#define deg *rpd;

#define MALLOC( PTR, CAST, COUNT) \
  if ( !(PTR = (CAST *) malloc( (COUNT) * sizeof( CAST) ))) exit(-1)

typedef struct SEGPLTstr {  /* structure to hold which plates share a segment */
  unsigned long _iv0, _iv1;                                    /* _iv0 < _iv1 */
  unsigned long _ip[2];                      /* two plates share each segment */
# define _ip0 _ip[0]
# define _ip1 _ip[1]
  struct SEGPLTstr *_next; /* pointer to next struct with same _iv0 */
} SEGPLT;

typedef struct {
  SEGPLT *_store;
  SEGPLT *_nextStore;
  SEGPLT **_firstSegPltForIv0s;
  unsigned long _nseg, _nv;
} SEGPLTTOP;

/*******************************************************************************
 * find first SEGPLT in sorted linked list for _iv0 where _iv1 is >= to iv1
 * - returns (SEGPLT **) i.e. initial pointer into _iv0 list OR 
 *                            ->_next of prev SEGPLT
 * - returns (SEGPLT **) 0 if no match found
 */
SEGPLT **spudf_findNextSegPlt( SEGPLTTOP *segPltTop
                             , unsigned long iv0, unsigned long iv1) {
SEGPLT **foundPtr;
unsigned long iTmp;
  if ( iv0 > iv1) { iTmp = iv0; iv0 = iv1; iv1 = iTmp; }
  foundPtr = segPltTop->_firstSegPltForIv0s + iv0;
  while ( *foundPtr) {
    if ( (*foundPtr)->_iv1 >= iv1) break;
    foundPtr = &(*foundPtr)->_next;
  }
  return foundPtr;
}

/*******************************************************************************
 * add ip to SEGPLT for vertices iv0 & iv1
 * - initialises next SEGPLT on _store if none allocated to iv0/iv1 yet
 * - returns pointer to SEGPLT used or added
 */
SEGPLT **spudf_addSegPlt( SEGPLTTOP *segPltTop
                        , unsigned long iv0, unsigned long iv1
                        , unsigned long ip) {
SEGPLT **foundPtr;
SEGPLT *found;
SEGPLT spTmp = { iv0, iv1, ip, ip, (SEGPLT *) 0 };
unsigned long iTmp;

  if ( iv0 > iv1) { 
    iTmp = iv0; iv0 = iv1; iv1 = iTmp;
    spTmp._iv0 = iv0; spTmp._iv1 = iv1;
  }

  foundPtr = spudf_findNextSegPlt( segPltTop, iv0, iv1);

  found = *foundPtr;

  if ( found) if ( found->_iv1 == iv1) {    /* if !NULL & if iv1 matches ...  */
    found->_ip1 = ip;                       /*   add ip ...                   */
    return foundPtr;                        /*   & return                     */
  }

  /* to this point, found is either 
   *   - NULL or 
   *   - points to a SEGPLT whose ->_iv1 does not match the iv1 argument
   * in either case, use ->_nextStore (i.e. next SEGPLT on ->_store list)
   */
  spTmp._next = found;                      /* save next pointer, NULL or not */
  found = *foundPtr = segPltTop->_nextStore++;        /* get next one on list */
  *found = spTmp;

  return foundPtr;
} /* SEGPLT **spudf_addSegPlt( SEGPLTTOP *segPltTop ... */

/* segments of a plate in pairs */

#define IPSEG01(A,P) (A)->faceapices[P],(A)->oe[(A)->faceoeidx[P]]
#define IPSEG20(A,P) (A)->oe[(A)->faceoeidx[P]+1],(A)->faceapices[P]
#define IPSEG12(A,P) (A)->oe[(A)->faceoeidx[P]],(A)->oe[(A)->faceoeidx[P]+1]

/* vertices of a plate as a triplet */

#define IPVRT(S,P) (S)->faceapices[P],(S)->oe[(S)->faceoeidx[P]]\
                   ,(S)->oe[(S)->faceoeidx[P]+1]

#define IPSUMVRT(S,P) ((S)->faceapices[P] + (S)->oe[(S)->faceoeidx[P]]\
                      + (S)->oe[(S)->faceoeidx[P]+1])

/*******************************************************************************
 * create & initialize SEGPLTTOP structure, fill all SEGPLT structures
 */
SEGPLTTOP *spudf_initSegPlt( SPUDF *spudf) {
SEGPLTTOP *segPltTop;
SEGPLT **p, **e;
unsigned long ip;
  MALLOC( segPltTop, SEGPLTTOP, 1);
  segPltTop->_nseg = lnseg;
  segPltTop->_nv = lnv;
  MALLOC( segPltTop->_store, SEGPLT, lnseg);
  MALLOC( segPltTop->_firstSegPltForIv0s, SEGPLT *, lnv);
  segPltTop->_nextStore = segPltTop->_store;
  for ( e=(p=segPltTop->_firstSegPltForIv0s)+lnv; p<e; ++p) *p = (SEGPLT *) 0;

  for ( ip=0; ip<spudf->nface; ++ip) {   /* for each plate, add to SEGPLT ... */
    spudf_addSegPlt( segPltTop, IPSEG01(spudf,ip), ip);
    spudf_addSegPlt( segPltTop, IPSEG20(spudf,ip), ip);
    spudf_addSegPlt( segPltTop, IPSEG12(spudf,ip), ip);
  }

  return segPltTop;
} /* SEGPLTTOP *spudf_initSegPlt( SPUDF *spudf) { */

/*******************************************************************************
 * free SEGPLTTOP contents & struct
 */
void spudf_destroySegPlt( SEGPLTTOP *segPltTop) {
  free( segPltTop->_store);
  free( segPltTop->_firstSegPltForIv0s);
  free( segPltTop);
  return;
} /* void spudf_destroySegPlt( SEGPLTTOP *segPltTop) { */

/**************************************************************
 * spudf_fixNorms - fix inward-facing normals
 * - start by assuming plate that is farthest out has positive dot product
 *   between its normal and its radius vector
 * - ensure adjacent plates are consistent with that plate
 * - continue working via plate adjacencies through complete model
 */

long spudf_fixNorms( SPUDF *spudf, int testEnv) {
unsigned long ip, iv, maxIv;
unsigned long ipBase, ipAdj;
double *vtx0, *vtx1, *vtx2, *vtmp;
VEC seg10, seg20, seg21, seg01;
double *uvn, *mp, *r2;
double vl, maxVl, minCP;
double l10, l20, l21, ltmp;
double a2, b2, c2, c, cb, d2, eod, g;
VEC vg, v2g;
SEGPLTTOP *segPltTop;
SEGPLT *segPlt, **segPltPtr;
unsigned long *lclVrts, *vrtPtr, *vrtPtr2, *endVrtPtr;
unsigned long *lclPlts, *pltPtr, *pltPtr2, *endPltPtr;
unsigned long *lclFixedPlts, *fixedPltPtr, *fixedPltPtr2, *endFixedPltPtr;
long retVal = 0;
char *debug;

  if ( testEnv && getenv( "SPUDF_NOFIXNORMS")) return 0;

  debug = getenv( "SPUDF_FIXNORMSDEBUG");

  /* I) find plate that is furthest from origin
   * I-01) start with vertex farthest from origin, maxIv
   */
  maxVl = 0.0;
  for ( vtx0=lRxyz+(iv=0); iv<lnv; ++iv, vtx0+=3) {
    if ( (vl=VLEN(vtx0)) > maxVl) {
      maxIv = iv;
      maxVl = vl;
    }
  } /* I-01) complete */

  /* I-02) for each segment with maxIv as one endpoint, find the other vertex
   * I-02-A) allocate & init SEGPLTTOP struct; allocate vertex & plate arrays
   */
  segPltTop = spudf_initSegPlt( spudf);
  MALLOC( lclVrts, unsigned long, lnv);
  MALLOC( lclPlts, unsigned long, lnface);
  endVrtPtr = lclVrts;
  endPltPtr = lclPlts;

# define IVMATCH(V,S,P) MATCH3( V \
                              , (S)->faceapices[P] \
                              , (S)->oe[(S)->faceoeidx[P]] \
                              , (S)->oe[(S)->faceoeidx[P]+1] )
# define MATCH3(V,V0,V1,V2) ((V)==(V0) ? 0 : \
                            ((V)==(V1) ? 1 : \
                            ((V)==(V2) ? 2 : 3 )))

# define IPGETVRT(V,S,P) GETVRT( V \
                               , (S)->faceapices[P] \
                               , (S)->oe[(S)->faceoeidx[P]] \
                               , (S)->oe[(S)->faceoeidx[P]+1] )
# define GETVRT(V,V0,V1,V2) ((V)==(0) ? V0 : ((V)==(1) ? V1 : ( V2 )))

  /* I-02-B) find any segment with maxIv as one vertex, save relevant plates
   */

  if ( (segPlt=segPltTop->_firstSegPltForIv0s[maxIv])) {
    *(endPltPtr++) = segPlt->_ip0;
    *(endPltPtr++) = segPlt->_ip1;
  } else {
    for ( ip=0; ip<lnface; ++ip) {
      if ( IVMATCH(maxIv,spudf,ip) < 3) {
        *(endPltPtr++) = ip;
        break;
      }
    }
  }

  /* I-02-C) starting from lclPlts[0], find all other plates that use maxIv
  /* I-02-C-i) for each plate in lclPlts up to endVrtPtr ...
   */
  for ( pltPtr=lclPlts; pltPtr<endPltPtr; ++pltPtr) {
  int i;
    /* I-02-C-ii) ... look at all vertices other than maxIv, 
     *            they must be an endpt of a segment that has maxIv at 
     *            the other end.
     */
    for ( i=0; i<3; ++i) {
      if ( maxIv != (iv=IPGETVRT( i, spudf, *pltPtr)) ) {

        /* I-02-C-iii) for this vertex (iv), test if it has been addedlclVrts
         */
        for ( vrtPtr=lclVrts; vrtPtr<endVrtPtr; ++vrtPtr) {
          if ( iv == *vrtPtr) break;
        }

        /* I-02-C-iv) add iv if it has not been added before
         */
        if ( vrtPtr == endVrtPtr) {                /* iv not found in lclVrts */
        int j;

          /* add this vertex to the lclVrts array
           * - the guts of step I-02
           */
          *(endVrtPtr++) = iv;                   

          /* I-02-C-v) for this newly added vertex (iv), look at both
           *            plates that use the segment between it & maxIv
           */
          if ( !(segPlt = *(spudf_findNextSegPlt(segPltTop,maxIv,iv))) ) {
            continue;                   /* jumps to "for (i=0;i<3;++i)" above */
          }
          j = 0;
          for ( pltPtr2=lclPlts; pltPtr2<endPltPtr; ++pltPtr2) {
            if ( *pltPtr2 == segPlt->_ip0) j |= 1;
            if ( *pltPtr2 == segPlt->_ip1) j |= 2;
          }
          switch (j) {
          case 0:          /* neither plate found in lclPlts, add both plates */
            *(endPltPtr++) = segPlt->_ip0;
            *(endPltPtr++) = segPlt->_ip1;
            break;
          case 1:                          /* _ip0 found in lclPlts, add _ip1 */
            *(endPltPtr++) = segPlt->_ip1;
            break;
          case 2:                          /* _ip1 found in lclPlts, add _ip0 */
            *(endPltPtr++) = segPlt->_ip0;
            break;
          case 3:          /* both plates found in lclPlts, add neither plate */
          default:
            break;
          } /* switch j */
        } /* if ( vrtPtr == endVrtPtr ) i.e. added iv vertex */
      } /* if ( maxIv != (iv=IPGETVRT( i, spudf, ip)) ) { */
    } /* for ( i=0; i<3; ++i) { */

  } /* for ( pltPtr=lclPlts; pltPtr<endPltPtr; ++pltPtr) { */

  /* I-02) complete - vertices adjacent to maxIv in lclVrts */

  /* I-03) find segment that is closest to perpendicular to radius to maxIv
   */
  vtx0 = spudf->Rxyz + (3 * maxIv);                        /* radius to maxIv */

  for ( vrtPtr=lclVrts; vrtPtr<endVrtPtr; ++vrtPtr) {   /* loop thru segments */
  VEC v, vcp;
  double lVcp;
    VMINUS2( spudf->Rxyz + (3 * (*vrtPtr)), vtx0, v);     /* maxIv to *vrtPtr */
    vcrss_c( vtx0, v, vcp);              /* cross product of radius & segment */
    lVcp = VLEN( vcp);                             /* length of cross product */
    if ( lVcp < minCP || vrtPtr == lclVrts) { /* smaller => more perpendicular*/
      iv = *vrtPtr;
      minCP = lVcp;
    }
  }
  /* I-03) complete */

  /* I-04) find plate on segment that is most nearly perpendicular 
   *       to maxIv radius i.e. has greatest VDOT between normal and vtx0
   * - save that plate at start of lclPlts
   */
  if ( !(segPlt = *(spudf_findNextSegPlt(segPltTop,maxIv,iv))) ) {
    exit(-1);
  }
  lclPlts[0] = 
    (VDOT(vtx0,luvnorms+(3*segPlt->_ip0))>VDOT(vtx0,luvnorms+(3*segPlt->_ip1)))
    ? segPlt->_ip0 : segPlt->_ip1;

  /* I-04) complete */
  /* I) complete - plate farthest from origin found */

  /* II) walk around plate model by way af adjacent plates starting from 
   *       plate lclPlts[0] found in I-04
   * - lclPlts will hold adjacent plate numbers
   * - lclFixedPlts[ip] will be 0 for plate ip before is has had its normal
   *   checked & then been added to lclPlts, then be set to 1
   */
  endPltPtr = lclPlts+1;              /* where to add the next adjacent plate */

  /* II-01) allocate and init map to fixed status of each plate 
   */
  MALLOC( lclFixedPlts, unsigned long, lnface);
  endFixedPltPtr=lclFixedPlts+lnface;
  for ( fixedPltPtr=lclFixedPlts; fixedPltPtr<endFixedPltPtr; ++fixedPltPtr) {
    *fixedPltPtr = 0;
  }
  lclFixedPlts[ lclPlts[0] ] = 1;                          /* add first plate */

  /* II-01) complete */

  /* II-02) loop through fixed plates
   * - for each adjacent plates
   *   - check adjacent plates' normals
   *   - add adjacent plate to lclPlts
   */
  for ( pltPtr=lclPlts; pltPtr<endPltPtr; ++pltPtr) {
  unsigned long baseVrts[4], idxBaseVrt;
  unsigned long ipAdj, ivOtherBase, ivOtherAdj, sumSegVrt, sumPltPtrVrt;
  double *baseNorm, *adjNorm, dotxdot, ovlen;
  VEC otherVec;

    /* II-02-A) for each fixed plate *pltPtr, loop through its three segments
     */
    baseVrts[0] = spudf->faceapices[*pltPtr];
    baseVrts[1] = spudf->oe[spudf->faceoeidx[*pltPtr]];
    baseVrts[2] = spudf->oe[spudf->faceoeidx[*pltPtr]+1];
    baseVrts[3] = baseVrts[0];

    sumPltPtrVrt = IPSUMVRT( spudf, *pltPtr);
    baseNorm = luvnorms + (*pltPtr * 3);

    for ( idxBaseVrt=0; idxBaseVrt<3; ++idxBaseVrt) {

      /* II-02-A-i) get segPlt structure for this segment
       */
      segPlt = *(spudf_findNextSegPlt(segPltTop
                                     ,baseVrts[idxBaseVrt]
                                     ,baseVrts[idxBaseVrt+1]));

      /* II-02-A-ii) get index of adjacent plate
       * - ipAdj:  plate adjacent to *pltPtr in segPlt 
       *           - i.e. which of segPlt->_ip0/_ip1 is not *pltPtr
       *   - if ipAdj has already been fixed, skip (continue) to end of loop
       *   - else set ipAdj as fixed
       */

      ipAdj =  (segPlt->_ip0 + segPlt->_ip1) - *pltPtr;

      if ( lclFixedPlts[ipAdj] ) continue;

      lclFixedPlts[ipAdj] = 1;
      *(endPltPtr++) = ipAdj;

      /* II-02-A-iii) get these sums
       * - sumPltPrtVrt - sum of *pltPtr vertices (done above)
       * - sumSegVrt - sum of segment vertices segPlt->_iv0/_iv1
       */
      sumSegVrt = segPlt->_iv0 + segPlt->_iv1;

      /* II-02-A-iv) calculate these indices
       * - ivOtherBase - other vertex of *pltPtr is not in segPlt->_iv0/_iv1
       * - ivOtherAdj - other vertex of ipAdj is not in segPlt->_iv0/_iv1
       */

      ivOtherBase = sumPltPtrVrt - sumSegVrt;
      ivOtherAdj = IPSUMVRT( spudf, ipAdj) - sumSegVrt;

      /* II-02-A-v) get pointers to these vectors' locations
       * - vtx0:  origin to ivOtherBase
       * - vtx1:  origin to ivOtherAdj
       * - baseNorm:  normal of base plate (done above)
       * - adjNorm:  normal of adjacent plate
       */
      vtx0 = lRxyz + (3 * ivOtherBase);
      vtx1 = lRxyz + (3 * ivOtherAdj);
      adjNorm = luvnorms + (ipAdj * 3);

      /* II-02-A-vi) calculate
       * - otherVec:  vector between "other" vertices ivOtherBase & ivOtherAdj
       * - dotxdot:  product of dot products of normals with otherVec
       */
      VMINUS2(vtx1,vtx0,otherVec);
      dotxdot = VDOT( adjNorm, otherVec) * VDOT( baseNorm, otherVec);

      ovlen = VLEN(otherVec);
      if ( ovlen > 0.0 ) {
        dotxdot /= (ovlen*ovlen);                         /* unitize otherVec */
      } else {   /* ovlen is 0; otherVec is null; vtx0 & vtx1 are the same pt */
        VNEG2( baseNorm, adjNorm);          /* adj norm is opposite base norm */
        continue;                                 /* done with this adj plate */
      }

     /* I-02-A-vii) finally, check sign of dotxdot
      * - if negative, adjacent plate adjNorm is ok, continue to end of loop
      * - if 0, base & adjacent plate are parallel
      *   - calculate dot product of norms
      *     - if positive, adjNorm is ok, continue to end of loop
      *     - if negative, adjNorm is NOT ok
      *   - 0 is defined as < 1e-12 for roundoff of parallel plates
      * - if positive, adjacent plate adjNorm is NOT ok
      *
      * - if adjNorm is NOT ok, VNEG it
      */
     if ( dotxdot < 0.0) continue;
     if ( dotxdot < 1e-12) if ( VDOT(adjNorm,baseNorm) > 0.0) continue;

     VNEG( adjNorm);                                        /* fix the normal */

     if ( debug) {
#      define PVEC( T, P) fprintf( stderr, "%s ( %lg %lg %lg )\n", T \
                          , (double) (P)[0], (double) (P)[1], (double) (P)[2])

       fprintf( stderr, "Fixing norm @ plt %lu(%lu) adj to base plt %lu(%lu)\n"
              , ipAdj, spudf->platenum[ipAdj]
              , *pltPtr, spudf->platenum[*pltPtr]);

       fprintf( stderr, "%lg = dotxdot = %lg(adj) * %lg(base)\n"
              , dotxdot, VDOT( adjNorm, otherVec), VDOT( baseNorm, otherVec));

       fprintf( stderr, "Base VDOT(R0,uvn) = %lg\n"
              , VDOT( spudf->Rxyz + (3 * spudf->faceapices[*pltPtr])
                    , luvnorms + (3 * *pltPtr))  );
       fprintf( stderr, "Adj  VDOT(R0,uvn) = %lg\n"
              , VDOT( spudf->Rxyz + (3 * spudf->faceapices[ipAdj])
                    , luvnorms + (3 * ipAdj)) );

       fprintf( stderr, "Base Vert id's = ( %lu %lu %lu ) sumVerts=%lu\n"
                      , IPVRT(spudf,*pltPtr), sumPltPtrVrt);
       fprintf( stderr, "Adj  Vert id's = ( %lu %lu %lu ) sumVerts=%lu\n"
                      , IPVRT(spudf,ipAdj), IPSUMVRT(spudf,ipAdj));

       fprintf( stderr, "segPlt:  _ip0/_ip1 = ( %lu %lu )\n"
              , segPlt->_ip0, segPlt->_ip1);
       fprintf( stderr, "segPlt:  _iv0/_iv1 = ( %lu %lu )\n"
              , segPlt->_iv0, segPlt->_iv1);

       fprintf( stderr, "Base Other vertnum = %lu\n", ivOtherBase);
       fprintf( stderr, "Adj  Other vertnum = %lu\n", ivOtherAdj );

       PVEC( "Base other Vert = ", vtx0);
       PVEC( "Adj  other Vert = ", vtx1);

       PVEC( "Base norm = ", baseNorm);
       PVEC( "Adj  norm = ", adjNorm);
       fprintf( stderr, "\n");
     } /* if ( debug) { */

     retVal++;

    } /* for ( idxBaseVrt=0; idxBaseVrt<3; ++idxBaseVrt) { */
  } /* for ( pltPtr=lclPlts; pltPtr<endPltPtr; ++pltPtr) { */

  free( lclVrts);
  free( lclPlts);
  free( lclFixedPlts);
  spudf_destroySegPlt( segPltTop);
  return retVal;
} /* void spudf_fixNorms( SPUDF *spudf, int testEnv) { */

/******************************************************/
/* conversion from Radius model to Facet model attempt to fix area/facet */

void rmod2face1( SPUDR *spudr, SPUDF *spudf) {

double latdel, londel;
unsigned long i, ilat;
double coslat, sinlat, coslon, sinlon;
double xlon, xlat;
unsigned long ilon, iv;
double *xxx, rrr, maxr2c, baser2c, r2c;
double *tmplons;
unsigned long ntmp;
double **llons;

unsigned long curlon0, nextlon0, nlon0, lastlon0;
unsigned long curlon1, nextlon1, nlon1, lastlon1, ntail1;
unsigned long lat0, lat1, curoe;
unsigned long curface;
unsigned long *loef, *loef0, *loef1, *loefc;
double dlon, dlon0, dlon1;

  if ( rpd == 0.) rpd = atan(1.0) / 45.;

  leastlon = spudr->eastlon;
  lnlat = spudr->nlatR;
  spudf->_didNotMallocLatInfo = 0;
  MALLOC( llats, double, lnlat);
  MALLOC( llatidx, unsigned long, lnlat+1);
  MALLOC( lnlon, unsigned long, lnlat);

  MALLOC( llons, double *, lnlat);

/* find largest R*R*cos(lat) in model */

# define R2C( LAT, LON, COSLAT) \
        (COSLAT * pow( (double)lltor1(spudr,LAT,LON),2.0))

  latdel = 180. / (lnlat-1);
  londel = 360. / (spudr->nlonR-1);
  maxr2c = 0.0;
  llats[0] = -90 deg;
  for ( i=1; i<lnlat; ++i) {
    xlat = ((i*latdel) - 90.);
    llats[i] = xlat deg;
    coslat = cos((double)llats[i]);
    for (ilon=0; ilon<spudr->nlonR; ++ilon) {
      r2c = R2C( xlat, ilon*londel, coslat);
      if ( r2c > maxr2c) maxr2c = r2c;
    }
  }
/* calculate R*R*cos(lat) for 1st lat above south pole w/south pole radius */
  baser2c = R2C( -90.0, 0.0, cos((double)llats[1]));

/* use ratio to calculate maximum number of longitudes at a latitude */
/* 6 faces at pole (60 degrees each) * area at max R*cos(lat) / area at pole */
/* - use 9 instead of 6 => 50% contingency */

  ntmp = (int) ( 0.5 + (9 * maxr2c / baser2c));

  MALLOC( tmplons, double, ntmp);

/* load up latitude arrays */
  lnv = 0;
  ilat = 0;

/* macro to create array of longitudes for this lat
 * - include extra element for 360
 */

# define ADDNLON(NLON) \
  llatidx[ilat] = lnv; \
  lnlon[ilat] = NLON; \
  MALLOC( llons[ilat], double, NLON+1); \
  llons[ilat][NLON] = 360.0; \
  lnv += NLON

/* - south pole */
  ADDNLON(1);
  llons[ilat][0] = 0.0;
  ilat++;

/* - south pole + 1 */
  ADDNLON(6);
  for ( ilon=0; ilon<lnlon[ilat]; ++ilon) llons[ilat][ilon] = ilon * 60.0;
  ilat++;

/* - between poles & +/- (90-latdel) */
  for ( ; ilat<(lnlat-2); ++ilat) {
  int nlon;
  double xlon, dlon;

    xlat = llats[ilat] / rpd;
    coslat = cos( (double)llats[ilat]);

/* here's the beef:  keep the area of each face approximately constant, 
 * for the base area, approximate as one of 6 faces around south pole
 * i.e. AREA ~ 0.5 * (dlat*R) * (dlon*R*coslat)
 *      AREA(lat,lon) ~ AREA(lat=-90+dlat,lon=*) =>
 *         0.5 * (dlat*R)   * (dlon(lon) * R(lat,lon) * coslat       ) 
 *       = 0.5 * (dlat*Rsp) * (60        * Rsp        * cos(-90+dlat))
 *      - Rsp = radius at south pole
 *      - 60 = dlon around south pole
 *      - 0.5 & dlat are constant and drop out, result is dlon * R * R * coslat
 *      - right side is 60 * baser2c
 *      - solve for dlon = 60 * baser2c / r2c(lat,lon)
 */
    for ( nlon = xlon = dlon = 0; (xlon + dlon) < 360.0; ) {
      tmplons[nlon] = xlon;
      dlon = 60.0 * baser2c / R2C( xlat, xlon, coslat);
      if ( dlon > 60.0) dlon = 60.0;
      xlon += dlon;
      nlon++;
      /* boost temporary longitudes array */
      if ( nlon >= ntmp) {
        ntmp *= 2;
        tmplons = realloc( tmplons, ntmp * sizeof(double));
        fprintf( stderr
               , "RMOD2FACE1:  Pathology required, contact programmer\n");
      }
    }
    /* make last longitude halfway between previous long & 360 */
    tmplons[nlon-1] = (360.0 + tmplons[nlon-2]) / 2;

    ADDNLON( nlon);
    memcpy( (void *)llons[ilat], (void *)tmplons, nlon * sizeof(double));
  }

/* - north pole - 1 */

  if ( ilat == (lnlat-2)) {
    ADDNLON(6);
    for ( ilon=0; ilon<lnlon[ilat]; ++ilon) llons[ilat][ilon] = ilon * 60.0;
    ilat++;
  }

/* - north pole */
  ADDNLON(1);
  llons[ilat][0] = 0.0;

/* - end pointer for north pole, if necessary */
  llatidx[lnlat] = lnv;

/* calculate # faces and segments */
  lnface = (lnv-2) * 2;
  lnseg = (lnface * 3) / 2;

/* allocate remaining arrays */
/* - special case for south pole faces:  allocate extra oe for wraparound */
  MALLOC( lRxyz, double, lnv*3);
  MALLOC( loeidx, unsigned long, lnv);
  MALLOC( lfaceapices, unsigned long, lnface);
  MALLOC( lfaceoeidx, long, lnface);
  MALLOC( lplatenum, unsigned long, lnface);
  MALLOC( lplatecolor, double, lnface);
  MALLOC( loe, unsigned long, lnseg+1);
  loe++;

/* fill xyz array */
  xxx = lRxyz;
  for ( ilat=0; ilat<lnlat; ilat++) {
    londel = 360. / lnlon[ilat];
    coslat = cos((double)llats[ilat]);
    sinlat = sin((double)llats[ilat]);
    xlat = llats[ilat]/rpd;
    for (ilon = 0, iv = llatidx[ilat]; iv<llatidx[ilat+1]; iv++, ilon++) {
      xlon = llons[ilat][ilon];
      rrr = lltor1( spudr, xlat, xlon);
      xlon *= rpd;
      coslon = cos(xlon); sinlon = sin( leastlon * xlon);
      *xxx++ = rrr * coslon * coslat;
      *xxx++ = rrr * sinlon * coslat;
      *xxx++ = rrr * sinlat;
    }
  }

/* load segments array */
/* - south pole */
  for ( loeidx[0] = curoe = 0; curoe<lnlon[1]; curoe++) loe[curoe] = curoe+1;

/* - between poles */
  for ( lat0=1; lat0<(lnlat-2); lat0++) {
  double *llons0, *llons1;

/*   - setup for lat */
    lat1 = lat0 + 1; 
    curlon0 = llatidx[lat0]; nlon0 = lnlon[lat0]; nextlon0 = curlon0 + 1;
    curlon1 = llatidx[lat1]; nlon1 = lnlon[lat1]; nextlon1 = curlon1 + 1;
    llons0 = llons[lat0];
    llons1 = llons[lat1];
    dlon0 = llons0[1] - llons0[0];  /* 360. / nlon0; */
    dlon1 = llons1[1] - llons1[0];  /* 360. / nlon1; */
    loeidx[curlon0] = curoe;

/*   - start of each lat */
/*     - add segment from first to last long along lat0 */
    lastlon0 = curlon0 + nlon0 - 1;
    if ( nlon0>1) loe[curoe++] = lastlon0;

/*     - optionally add segs from lat0 to high-long lat1 */
/*       - i present this algorithm for ntail1 & lastlon1 w/o comment */
    ntail1 = (nlon1 + nlon0) / (2 * nlon0);
    if ( (2*ntail1*nlon0) == (nlon1+nlon0)) ntail1--;
    lastlon1 = curlon1 + nlon1 - ntail1;
/**/
    if ( nlon1>1) for ( ilon=lastlon1; ilon<(curlon1+nlon1); ilon++) {
      loe[curoe++] = ilon;
    }

/*     - add curlon0 to curlon1 seg */
/*     - set dlon, which is curlon0-curlon1 delta longitude */
    loe[curoe++] = curlon1;
    dlon = 0.;

/*   - loop through longitudes in this lat */
    while ( curlon0 < lastlon0 || curlon1 < lastlon1 ) {

/*     - if next cross-lat seg is from next lat0 to current lat1 ... */
/*       - add seg along lat0 if this isn't the last dlon0 seg */
/*       - increment curlon0 & nextlon0 along lat0 */
/*       - start new index into oe */
/*       - add next seg */
/*       - modify dlon */
      if ( curlon0<lastlon0 && ((dlon0-dlon) < (dlon+dlon1)) ) {
        loe[curoe++] = nextlon0;
        loeidx[nextlon0] = curoe;
        loe[curoe++] = curlon1;
        curlon0 = nextlon0++;
        dlon -= dlon0;
        llons0++;
        dlon0 = llons0[1] - llons0[0];

/*     - else next cross-lat seg is from current lat0 to next lat 1 ... */
/*       - add next seg */
/*       - increment curlon1 & nextlon1 along lat1 */
/*       - modify dlon */
      } else {
        loe[curoe++] = ( nextlon1<llatidx[lat1+1]) ? nextlon1 : llatidx[lat1];
        curlon1 = nextlon1++;
        dlon += dlon1;
        llons1++;
        dlon1 = llons1[1] - llons1[0];
      }
    }
  }

/* - north pole */
/*   - setup */
/*     - add seg from first to last long on lat0 */
/*     - add seg from first long on lat0 to first long on lat1 */
  lat1 = lat0 + 1;
  curlon0 = llatidx[lat0];
  curlon1 = llatidx[lat1];
  loeidx[curlon0] = curoe;
  if ( lnlon[lat0]>1) loe[curoe++] = curlon1-1;
  loe[curoe++] = curlon1;

/*   - loop through long's on lat0 */
/*     - add seg to next long on lat0, increment long along lat0 */
/*     - start new index into oe */
/*     - add seg from lat0 to lat1 */
  while ( curlon0<(curlon1-1)) {
    loe[curoe++] = ++curlon0;
    loeidx[curlon0] = curoe;
    loe[curoe++] = curlon1;
  }

/* - finish off other end index */
  loeidx[curlon1] = curoe;

/* face index - use each oe which has a following oe from the same pt */
/* - special case: wraparound at S pole */

  loef = loe - 1;
  *loef = loe[loeidx[1]-1];

  for ( loef1=loef, curlon0=curface=0; curlon0 < (lnv - 1); curlon0++) {
    loef0 = loef1;
    loef1 = loe + loeidx[curlon0+1];
    for (loefc=loef0; loefc<(unsigned long *)(loef1-1); loefc++) {
      lplatenum[curface] = curface + 1;
      lfaceapices[curface] = curlon0;
      lplatecolor[curface] = -1.0; /* VLEN( lRxyz+(curlon0*3)); /**/
      lfaceoeidx[curface++] = ((long)(loefc - loef)) - 1;
    }
  }

  luvnorms = lmidpts = lr2 = (double *) 0;
  lspudf2 = (void *) 0;

  /* cleanup */
  for ( ilat=0; ilat<lnlat; ++ilat) free( llons[ilat]);
  free( llons);
  free( tmplons);

  return;
}

/******************************************************/
/* simple conversion from Radius model to Facet model */

void rmod2face0( SPUDR *spudr, SPUDF *spudf) {

double latdel, londel;
unsigned long i, ilat;
double coslat, sinlat, coslon, sinlon;
double xlon, xlat;
unsigned long ilon, iv;
double *xxx, rrr;

unsigned long curlon0, nextlon0, nlon0, lastlon0;
unsigned long curlon1, nextlon1, nlon1, lastlon1, ntail1;
unsigned long lat0, lat1, curoe;
unsigned long curface;
unsigned long *loef, *loef0, *loef1, *loefc;
double dlon, dlon0, dlon1;

  if ( rpd == 0.) rpd = atan(1.0) / 45.;

  leastlon = spudr->eastlon;
  lnlat = spudr->nlatR;
  spudf->_didNotMallocLatInfo = 0;
  MALLOC( llats, double, lnlat);
  MALLOC( llatidx, unsigned long, lnlat+1);
  MALLOC( lnlon, unsigned long, lnlat);

/* load up latitude arrays */

/* - south pole */
  llatidx[0] = 0;
  llats[0] = -90. deg;
  lnlon[0] = 1;
  latdel = 180. / (lnlat-1);

  lnv = 1;

/* - between poles */
  for ( i=1; i<(lnlat-1); i++) {
  int nlon;
  double xnlon;

    llatidx[i] = lnv;
    llats[i] = ((i*latdel) - 90.) deg;
    xnlon = ( (spudr->nlonR - 1) * cos((double)llats[i])) +2; /* round to n*4 */
    nlon = ((int) (xnlon/4)) * 4;		/* force lons at 0,90,180,270 */
    if ( !nlon) nlon = 4;
    lnlon[i] = nlon;
    lnv += nlon;
  }

/* - north pole */
  llatidx[lnlat-1] = lnv++;
  llats[lnlat-1] = - llats[0];
  lnlon[lnlat-1] = 1;

/* - end pointer for north pole, if necessary */
  llatidx[lnlat] = lnv;

/* calculate # faces and segments */
  lnface = (lnv-2) * 2;
  lnseg = (lnface * 3) / 2;

/* allocate remaining arrays */
/* - special case for south pole faces:  allocate extra oe for wraparound */
  MALLOC( lRxyz, double, lnv*3);
  MALLOC( loeidx, unsigned long, lnv);
  MALLOC( lfaceapices, unsigned long, lnface);
  MALLOC( lfaceoeidx, long, lnface);
  MALLOC( lplatenum, unsigned long, lnface);
  MALLOC( lplatecolor, double, lnface);
  MALLOC( loe, unsigned long, lnseg+1);
  loe++;

/* fill xyz array */
  xxx = lRxyz;
  for ( ilat=0; ilat<lnlat; ilat++) {
    londel = 360. / lnlon[ilat];
    coslat = cos((double)llats[ilat]);
    sinlat = sin((double)llats[ilat]);
    xlat = llats[ilat]/rpd;
    for (ilon = 0, iv = llatidx[ilat]; iv<llatidx[ilat+1]; iv++, ilon++) {
      xlon = londel * ilon;
      rrr = lltor0( spudr, xlat, xlon);
      xlon *= rpd;
      coslon = cos(xlon); sinlon = sin( leastlon * xlon);
      *xxx++ = rrr * coslon * coslat;
      *xxx++ = rrr * sinlon * coslat;
      *xxx++ = rrr * sinlat;
    }
  }

/* load segments array */
/* - south pole */
  for ( loeidx[0] = curoe = 0; curoe<lnlon[1]; curoe++) loe[curoe] = curoe+1;

/* - between poles */
  for ( lat0=1; lat0<(lnlat-2); lat0++) {
/*   - setup for lat */
    lat1 = lat0 + 1;
    curlon0 = llatidx[lat0]; nlon0 = lnlon[lat0]; nextlon0 = curlon0 + 1;
    curlon1 = llatidx[lat1]; nlon1 = lnlon[lat1]; nextlon1 = curlon1 + 1;
    dlon0 = 360. / nlon0;
    dlon1 = 360. / nlon1;
    loeidx[curlon0] = curoe;

/*   - start of each lat */
/*     - add segment from first to last long along lat0 */
    lastlon0 = curlon0 + nlon0 - 1;
    if ( nlon0>1) loe[curoe++] = lastlon0;

/*     - optionally add segs from lat0 to high-long lat1 */
/*       - i present this algorithm for ntail1 & lastlon1 w/o comment */
    ntail1 = (nlon1 + nlon0) / (2 * nlon0);
    if ( (2*ntail1*nlon0) == (nlon1+nlon0)) ntail1--;
    lastlon1 = curlon1 + nlon1 - ntail1;
/**/
    if ( nlon1>1) for ( ilon=lastlon1; ilon<(curlon1+nlon1); ilon++) {
      loe[curoe++] = ilon;
    }

/*     - add curlon0 to curlon1 seg */
/*     - set dlon, which is curlon0-curlon1 delta longitude */
    loe[curoe++] = curlon1;
    dlon = 0.;

/*   - loop through longitudes in this lat */
    while ( curlon0 < lastlon0 || curlon1 < lastlon1 ) {

/*     - if next cross-lat seg is from next lat0 to current lat1 ... */
/*       - add seg along lat0 if this isn't the last dlon0 seg */
/*       - increment curlon0 & nextlon0 along lat0 */
/*       - start new index into oe */
/*       - add next seg */
/*       - modify dlon */
      if ( curlon0<lastlon0 && ((dlon0-dlon) < (dlon+dlon1)) ) {
        loe[curoe++] = nextlon0;
        loeidx[nextlon0] = curoe;
        loe[curoe++] = curlon1;
        curlon0 = nextlon0++;
        dlon -= dlon0;

/*     - else next cross-lat seg is from current lat0 to next lat 1 ... */
/*       - add next seg */
/*       - increment curlon1 & nextlon1 along lat1 */
/*       - modify dlon */
      } else {
        loe[curoe++] = ( nextlon1<llatidx[lat1+1]) ? nextlon1 : llatidx[lat1];
        curlon1 = nextlon1++;
        dlon += dlon1;
      }
    }
  }

/* - north pole */
/*   - setup */
/*     - add seg from first to last long on lat0 */
/*     - add seg from first long on lat0 to first long on lat1 */
  lat1 = lat0 + 1;
  curlon0 = llatidx[lat0];
  curlon1 = llatidx[lat1];
  loeidx[curlon0] = curoe;
  if ( lnlon[lat0]>1) loe[curoe++] = curlon1-1;
  loe[curoe++] = curlon1;

/*   - loop through long's on lat0 */
/*     - add seg to next long on lat0, increment long along lat0 */
/*     - start new index into oe */
/*     - add seg from lat0 to lat1 */
  while ( curlon0<(curlon1-1)) {
    loe[curoe++] = ++curlon0;
    loeidx[curlon0] = curoe;
    loe[curoe++] = curlon1;
  }

/* - finish off other end index */
  loeidx[curlon1] = curoe;

/* face index - use each oe which has a following oe from the same pt */
/* - special case: wraparound at S pole */

  loef = loe - 1;
  *loef = loe[loeidx[1]-1];

  for ( loef1=loef, curlon0=curface=0; curlon0 < (lnv - 1); curlon0++) {
    loef0 = loef1;
    loef1 = loe + loeidx[curlon0+1];
    for (loefc=loef0; loefc<(unsigned long *)(loef1-1); loefc++) {
      lplatenum[curface] = curface + 1;
      lfaceapices[curface] = curlon0;
      lplatecolor[curface] = -1.0; /* VLEN( lRxyz+(curlon0*3)); /**/
      lfaceoeidx[curface++] = ((long)(loefc - loef)) - 1;
    }
  }

  luvnorms = lmidpts = lr2 = (double *) 0;
  lspudf2 = (void *) 0;

  return;
}

/***************************************************
 * spudf_colorStats
 * - init color statistics to zeroes, calculate if non-null colors exist
 *   - colors are values associated with each plate, -1.0 => no data/null value
 */
void
spudf_colorStats( SPUDF *spudf) {

  spudf->avgcolor = spudf->sigcolor = spudf->hicolor = spudf->locolor = 0.0;

  if ( spudf->platecolor) {  /* if plate color array exists ... */
  double sumcolor = 0.0;
  double sumcolor2 = 0.0;
  double *colorptr = spudf->platecolor;
  long nonulls, ip;

    for ( nonulls=ip=0; ip<spudf->nface; ++ip, ++colorptr) {

      if ( *colorptr != -1.0) {             /* ... which have non-null values */

        sumcolor += *colorptr;                  /* ... sum colors for average */
        sumcolor2 += (*colorptr) * (*colorptr); /* ... and for average square */

        /* also save high & low values */
        if ( !nonulls) spudf->locolor = spudf->hicolor = *colorptr;   /* init */
        if ( spudf->locolor > *colorptr) spudf->locolor = *colorptr;
        else if ( spudf->hicolor < *colorptr) spudf->hicolor = *colorptr;

        ++nonulls;
      }
    }
    if ( nonulls) {
      sumcolor /= nonulls;                                         /* average */
      sumcolor2 /= nonulls;                                 /* average square */
      spudf->avgcolor = sumcolor;                             /* save average */
      spudf->sigcolor = sqrt( sumcolor2 - sumcolor*sumcolor);   /* save sigma */
    }
  } /* if spudf->platecolor */
  return;
}

/**************************************************************
 * spudf_calcs
 * - calculate items relates to facets of a shape model:
 *   - color statistics using spudf_colorStats above
 *   - unit vector normals 
 *     - optionally allocate ->uvnorms if it is a null pointer
 *   - midpoints of each facet
 *   - radius squared of min sphere around midpoint that contains facet vertices
 */

void spudf_calcs( SPUDF *spudf) {
long ip;
double *vtx0, *vtx1, *vtx2, *vtmp;
VEC seg10, seg20, seg21, seg01;
double *uvn, *mp, *r2;
double vl;
double l10, l20, l21, ltmp;
double a2, b2, c2, c, cb, d2, eod, g;
VEC vg, v2g;

  spudf_colorStats( spudf);   /* color statistics */

  /* allocate space for normals, midpoints, r-squared's */

  if ( !spudf->uvnorms) 
    spudf->uvnorms = (double *) malloc( spudf->nface * 3 * sizeof(double));
  if ( !spudf->midpts) 
    spudf->midpts = (double *) malloc( spudf->nface * 3 * sizeof(double));
  if ( !spudf->r2) 
    spudf->r2 = (double *) malloc( spudf->nface * sizeof(double));

  uvn = spudf->uvnorms;
  mp = spudf->midpts;
  r2 = spudf->r2;

  for ( ip=0; ip<spudf->nface; ++ip, (uvn+=3), (mp+=3), ++r2) {
    /* unit vector normal */
    vtx0 = spudf->Rxyz + (3 * spudf->faceapices[ip]);
    vtx1 = spudf->Rxyz + (3 * spudf->oe[spudf->faceoeidx[ip]]);
    vtx2 = spudf->Rxyz + (3 * spudf->oe[spudf->faceoeidx[ip]+1]);
    VMINUS2( vtx1, vtx0, seg10);          /* vector from vertex 0 to vertex 1 */
    VMINUS2( vtx2, vtx0, seg20);          /* vector from vertex 0 to vertex 2 */
    vcrss_c( seg10,seg20,uvn); /* cross product of segments is normal to face */
    vl = 1.0 / VLEN(uvn);                             /* normalization factor */
    /* ensure normal faces outward
     * - dot prod of normal w/radial vector to any vertex should be > 0
     */
    if ( VDOT( uvn,vtx0) < 0.0) { VSCAL( -vl, uvn); }
    else { VSCAL( vl, uvn); }

    /* midpoint, radius squared */
    /* - find segment of maximum length, reset vtx pointers accordingly
     *   - c (& c2 = c*c) is longest side (0 to 1); b/a => 1->2, 2->0
     *   - seg01 is vector from vertex 1 to vertex 0
     */
    VMINUS2( vtx2, vtx1, seg21);
    l10 = VDOT(seg10,seg10);
    l20 = VDOT(seg20,seg20);
    l21 = VDOT(seg21,seg21);

#   define CBA2(C,B,A) c2=C; b2=B; a2=A
#   define VPSW( V0, V1) vtmp=V0; V0=V1; V1=vtmp

    if ( l10>=l20) {
      /* l10 is largest, invert seg10 to be seg01 */
      if ( l10>=l21) { CBA2(l10,l21,l20); VNEG2(seg10,seg01); }
      /* l21 is largest, swap vertices 0 & 2, copy seg21 */
      else { CBA2(l21,l10,l20); VPSW(vtx2,vtx0); CPYVEC(seg21,seg01); }
    } else {
      /* l20 is largest, swap vertices 1 & 2, invert seg20 */
      if ( l20>=l21) { CBA2(l20,l21,l10); VPSW(vtx2,vtx1); VNEG2(seg20,seg01); }
      /* l21, same as above */
      else { CBA2(l21,l10,l20); VPSW(vtx2,vtx0); CPYVEC(seg21,seg01); }
    }

    VMINUS2( vtx0, vtx1, seg01);

    /* initially put midpoint at middle of longest side */

    vlcom_c( 0.5, seg01, 1.0, vtx1, mp);
    *r2 = c2 / 4;

    /* adjust midpt if angle opp longest side is acute (not right or obtuse) */

    if ( (a2+b2) > c2) {
      c = sqrt( c2);
      cb = (b2 + c2 - a2) / (2 * c);      /* dist from V2-proj-onto-V01 to V1 */
      d2 = b2 - (cb*cb);                           /* distance from V01 to V2 */
      g = (c / 2) - cb;            /* dist from V01 midpt to V2-proj-onto-V01 */
      eod = (d2+(g*g)-(c2/4)) / (2*d2); /* dist from V01 midpt to facet midpt */
                                                        /* as a fraction of d */
      vlcom_c( cb/c, seg01, 1.0, vtx1, vg); /* vg = projection of V2 onto V01 */
      VMINUS2( vtx2, vg, v2g);           /* v2g = vector from vg on V01 to V2 */
      vlcom_c( eod, v2g, 1.0, mp, mp);                        /* adjust midpt */
      *r2 += (eod*eod*d2);                                   /* adjust radius */
    }
  } /* for ip ... */

  if ( (ip=spudf_fixNorms(spudf,1)) ) fprintf(stderr, "%ld normals fixed\n",ip);

  return;
}
 
void rmod2face( spudr, spudf) SPUDR *spudr; SPUDF *spudf; {
  if ( getenv( "RMOD2FACE1")) {
    rmod2face1( spudr, spudf); /**/
    fprintf( stderr, "Using rmod2face1()\n");
    fflush( stderr);
  } else {
    rmod2face0( spudr, spudf); /**/
  }
  spudf_calcs( spudf);
  return;
}

/* spudf_intersect - find intersection of boresight with plate model */
void spudf_intersect( 
SPUDF *spudf   /* INPUT:   spud facets */
, VEC sc       /* INPUT:   S/C position, ABF */
, VEC ubs      /* INPUT:   Boresight, UNIT VECTOR, ABF */
, double *dist /* OUTPUT:  dist from S/C to intersect; return 0 if ~intersect */
, unsigned 
  long *intplate /* OUTPUT:  plate index (NOT PLATENUM); =->nface if " */
) {
unsigned long ip;
double *mp, *uvn, *r2, *vtx0, *vtx1, *vtx2;
VEC mpmi, mpmsc, scpkbs, v0, v1, v2, v0xv1, v1xv2, v2xv0;
/* 
double bsdn,                dot012, dot120, dot201, ksofar, k; /**/

double bsdn, c2, b, a2, d2, dot012, dot120, dot201, ksofar, k; /**/
unsigned long ipsofar;
double bsdotuvn, mpmscduvn;
unsigned long *fa;
long *foidx;

# define ICOPTR(S) (spudf->Rxyz + (3*(S)))

  mp = spudf->midpts;                                  /* initialize pointers */
  fa=spudf->faceapices;
  foidx = spudf->faceoeidx;
  uvn = spudf->uvnorms;
  r2 = spudf->r2;

  ipsofar = spudf->nface;                              /* no intersection yet */
  ksofar = VLEN(sc) * 1.0e10;   /* no intersect'n yet, set > any intersection */
                               /* - assumes: (1) sc is outside of shape model */
                               /* -          (2) max/min R in shape < 1e10    */

  for ( ip=0; ip<spudf->nface; ++ip, ++fa, ++foidx, ++r2, (mp+=3), (uvn+=3)) {
    while (1) {         /* not really a loop, use here to force break to exit */
      bsdn = VDOT( ubs, uvn);                   /* boresight dot plate normal */
      if ( bsdn >= 0.0) break;              /* keep plate emission angle < 90 */
      VMINUS2( mp, sc, mpmsc);                              /* sc to midpoint */

#     ifndef _ALTERNATE_SPUDF_INT_
      b = VDOT( ubs, mpmsc);     /* SC to closest-point-on-boresight-to-midpt */
      c2 = VDOT( mpmsc, mpmsc);                        /* SC to midpt squared */
      a2 = c2 - (b*b);                   /* min boresight distance from midpt */
      if ( a2 > *r2) break;     /* boresight must pass within radius of midpt */
      if ( (k=VDOT(mpmsc,uvn)/bsdn) < 0.0) break;
      if ( k >= ksofar) break;  /* must intersect before current intersection */
      vlcom_c( k, ubs, 1.0, sc, scpkbs);  /* intersection, k * bore + sc, ABF */
#     else
      k=VDOT(mpmsc,uvn)/bsdn;                    /* k = range to intersection */
      vlcom_c( k, ubs, 1.0, sc, scpkbs);  /* intersection, k * bore + sc, ABF */
      VMINUS2( mp, scpkbs, mpmi);                 /* intersection to midpoint */
      if ( VDOT(mpmi,mpmi) > *r2) break;   /* ... range must pass close to mp */
      if ( k < 0.0) break;       /* ... must intersect in front of instrument */
      if ( k >= ksofar) break;  /* must intersect before current intersection */
#     endif

      vtx0 = ICOPTR( *fa);                                        /* vertices */
      vtx1 = ICOPTR( spudf->oe[ *foidx]);
      vtx2 = ICOPTR( spudf->oe[ (*foidx) + 1]);
      VMINUS2( vtx0, scpkbs, v0);    /* vectors from intersection to vertices */
      VMINUS2( vtx1, scpkbs, v1);
      VMINUS2( vtx2, scpkbs, v2);
      vcrss_c( v0, v1, v0xv1);      /* cross products of sequence of vertices */
      vcrss_c( v1, v2, v1xv2);
      if ( (dot012=VDOT(v0xv1,v1xv2)) < 0.0) break; /* cp dot cp should be >0 */
      vcrss_c( v2, v0, v2xv0);                      /* if intersection is     */
      if ( (dot120=VDOT(v1xv2,v2xv0)) < 0.0) break; /* inside triangle        */
      if ( (dot201=VDOT(v2xv0,v0xv1)) < 0.0) break;
      if ( dot012==0.0 || dot120==0.0) {  /* final checks for 0-length cross- */
      VEC v01, v12, v20;                  /* product => intersection is on or */
      double lv01, lv12, lv20;            /* in line & beyond a segment       */
        if ( dot012 == 0.0 && dot201 == 0.0) {                /* - vtx0->vtx1 */
          VMINUS2( v0, v1, v01); lv01 = VDOT(v01,v01);    /* - segment length */
          if ( lv01 < VDOT(v0,v0) || lv01 < VDOT(v1,v1)) break;   /* - beyond */
        } else if ( dot120 == 0.0 && dot012 == 0.0) {         /* - vtx1->vtx2 */
          VMINUS2( v1, v2, v12); lv12 = VDOT(v12,v12);    /* - segment length */
          if ( lv12 < VDOT(v1,v1) || lv12 < VDOT(v2,v2)) break;   /* - beyond */
        } else if ( dot201 == 0.0 && dot120 == 0.0) {         /* - vtx2->vtx0 */
          VMINUS2( v2, v0, v20); lv20 = VDOT(v20,v20);    /* - segment length */
          if ( lv20 < VDOT(v2,v2) || lv20 < VDOT(v0,v0)) break;   /* - beyond */
        }
      }
      ipsofar = ip;      /* passed all checks, save this intersection & break */
      ksofar = k;
      break;
    } /* while(1) */
  } /* for ip ... */

  if ( (*intplate = ipsofar) < spudf->nface) *dist = ksofar;
  else *dist = 0.0;

  return;
}

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
