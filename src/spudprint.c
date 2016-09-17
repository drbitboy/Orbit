#include <stdio.h>
#include <stdlib.h> /* for getenv */
#include <string.h>
#include <math.h>
#ifdef vms
char *malloc();
#else
#include <malloc.h>
#endif

#include "orbit_spice_names.h"
#include "orbit3d.h"
#include "spudshap.h"
#include "debug.h"

void spudprint_xplate( SPUDF *spudf) {
unsigned long	iv, ip, ipn, i0, i1, i2;
unsigned long	*adj, *order, *adjip, *platenum2face;
double *v0, *v1, *v2;
VEC v01, v02, crossp;
#define ICOPTR(S) (spudf->Rxyz+(3*(S)))

  /* allocate adjacent plates & order arrays and zero them out */

  adj = (unsigned long *) malloc( spudf->nface * 5 * sizeof( unsigned long));
  for ( ip=0; ip<(spudf->nface*5); ++ip) adj[ip] = 0L;
  order = adj + (3*spudf->nface);
  platenum2face = order + spudf->nface;  /* to sort faces by platenum */

  /* determine order of vertices, fill platenum sort indices, 
   * & find adjacent plates
   */
  for ( ip=0; ip<spudf->nface; ++ip) {
  unsigned long ipo;

    /* indices to faces sorted by plate number */
    platenum2face[spudf->platenum[ip]-1] = ip;

    /* determine order of vertices 
     * - calculate normal as cross product of (v1-v0) & (v2-v0)
     * - normal must point out
     * - set order[ip] = 1 if vertex order needs to be flipped; leave 0 if not
     */
    v0 = ICOPTR(spudf->faceapices[ip]);
    v1 = ICOPTR(spudf->oe[spudf->faceoeidx[ip]]);
    v2 = ICOPTR(spudf->oe[spudf->faceoeidx[ip] + 1]);
    VMINUS2( v1, v0, v01);
    VMINUS2( v2, v0, v02);
    vcross( v01, v02, crossp);
    if ( VDOT(v0,crossp) < 0.0) order[ip] = 1;

    /* find adjacent plates */

    /* - macro to find plate that has a segment that matches a given segment
     *   - IV1, IV2:  vertex numbers of segment endpoints
     *   - IW:  which adjacency this is - 0 => v0/v1; 1=> v1/v2; 2 => v1/v2
     */
#define FAP(IV1,IV2,IW) \
  if ( !adjip[IW]) { \
    /* for segment IV1/IV2 in this plate, step through all later plates */ \
    /* to find the matching segment */ \
    for ( ipo=ip+1; ipo<spudf->nface; ++ipo) { \
    unsigned long ivo0, ivo1, ivo2, iwo; \
      ivo0 = spudf->faceapices[ipo]; \
      ivo1 = spudf->oe[spudf->faceoeidx[ipo]]; \
      ivo2 = spudf->oe[spudf->faceoeidx[ipo] + 1]; \
      /* find matching plate, set iwo to (IW+1) for matching plate */ \
      iwo = 0; \
      if ( IV1 == ivo0) { \
        if ( IV2 == ivo1) iwo = 1; else if ( IV2 == ivo2) iwo = 2; \
      } else if ( IV1 == ivo1) { \
        if ( IV2 == ivo0) iwo = 1; else if ( IV2 == ivo2) iwo = 3; \
      } else if ( IV1 == ivo2) { \
        if ( IV2 == ivo0) iwo = 2; else if ( IV2 == ivo1) iwo = 3; \
      } \
      if ( iwo) { \
        /* matched a segment - save platenums: */ \
        /* - matching plate in adjip[IW] */ \
        /* - save matched plate in matching plate's adj[] */ \
        adjip[IW] = spudf->platenum[ipo]; \
        adj[(ipo*3) + iwo - 1] = spudf->platenum[ip]; \
        ipo = spudf->nface; /* break out of loop */ \
      } \
    } /* for ipo ... */ \
  } /* if !adjip[IW] ... */
 
    adjip = adj + (3*ip);
    FAP( spudf->faceapices[ip], spudf->oe[spudf->faceoeidx[ip]], 0)
    FAP( spudf->faceapices[ip], spudf->oe[spudf->faceoeidx[ip]+1], 1)
    FAP( spudf->oe[spudf->faceoeidx[ip]], spudf->oe[spudf->faceoeidx[ip]+1], 2)
  }

  /* print out modified plate model (modifications per xgrs o3d.pro */

  /* print out header - nv, np nconn psize */
  printf( "    %ld %ld %ld 3\n", spudf->nv, spudf->nface, 3*spudf->nface);

  /* print out vertices */
  for ( iv=0; iv<spudf->nv; iv++) 
    printf( "%13.6lg%13.6lg%13.6lg\n"
         , spudf->Rxyz[3*iv], spudf->Rxyz[3*iv+1], spudf->Rxyz[3*iv+2]);

  /* print out faces */
  for ( ipn=0; ipn<spudf->nface; ipn++) {
  unsigned long ip3;
    ip = platenum2face[ipn];
    ip3 = ip * 3;
    i0 = spudf->faceapices[ip];
    i1 = spudf->oe[spudf->faceoeidx[ip]];
    i2 = spudf->oe[spudf->faceoeidx[ip] + 1];
    if ( !order[ip])                         /* verts, corrected for order    */
      printf("    3%6d%6d%6d",i0,i1,i2);
    else
      printf("    3%6d%6d%6d",i0,i2,i1);
    adjip = adj + ip3;
    printf( "%6d%6d%6d", adjip[0], adjip[1], adjip[2]);    /* adjacent plates */
    printf( "%13.7lf%13.7lf%13.7lf"          /* plate normal (ignored by o3d) */
          , spudf->uvnorms[ip3], spudf->uvnorms[ip3+1], spudf->uvnorms[ip3+2]);
    printf( "%13.7lf", 1.0);                   /* plate area (ignored by o3d) */
#define XYZPTR(IN,I) (spudf->Rxyz+((IN)*3))[I]
#define AVGXYZ(I) ((XYZPTR(i0,I) + XYZPTR(i1,I) + XYZPTR(i2,I))/3.0)
    printf( "%13.7lf%13.7lf%13.7lf\n"                   /* plate midpoint */
          , AVGXYZ(0), AVGXYZ(1), AVGXYZ(2));
  }
  return;
}

void spudprint_stl( SPUDF *spudf, double shellthick) {
unsigned long ip, i, iv, ipn;
double *v[3];
unsigned long nv3 = spudf->nv * 3;
double *shellNorms = (shellthick == 0.0) ? (double *) 0 
                       : (double *) malloc( sizeof(double) * nv3);
#define innerShell shellNorms
double *pn, *uvn, scalePn;
double flipNorm = (shellthick < 0.0) ? -1.0 : 1.0;

  if ( shellNorms) {
    /* for each vertex, sum normals of all plates that include that vertex */
    for ( iv=0, pn=shellNorms; iv<nv3; ++iv) *(pn++) = 0.0;
    for ( ip=0; ip<spudf->nface; ++ip) {
      uvn = spudf->uvnorms+(3*ip);
      pn = shellNorms + (3*spudf->faceapices[ip]);
      VADD2( pn, uvn, pn);
      pn = shellNorms + (3*spudf->oe[spudf->faceoeidx[ip]]);
      VADD2( pn, uvn, pn);
      pn = shellNorms + (3*spudf->oe[spudf->faceoeidx[ip] + 1]);
      VADD2( pn, uvn, pn);
    }
    /* scale summed normals to shell thickness, add to surface vertex 
     * ***N.B. surface vector & plate normals occupy same variable space
     */
    for ( iv=0, pn=shellNorms; iv<spudf->nv; ++iv, pn += 3) {
      scalePn = - shellthick / VLEN(pn);
      vmxpb( scalePn, pn, spudf->Rxyz+(3*iv), pn);
    }
  }

/*
solid
  facet normal 0.00 0.00 1.00
    outer loop
      vertex  2.00  2.00  0.00
      vertex -1.00  1.00  0.00
      vertex  0.00 -1.00  0.00
    endloop
  endfacet
  facet ...
endsolid
 */

  printf( "solid\n");                                               /* header */

  for ( ip=0; ip<spudf->nface; ip++) {                              /* plates */
  unsigned long ip3;
  VEC vn;
    ip3 = ip * 3;
    VSCAL2( flipNorm, spudf->uvnorms+ip3, vn);
    v[0] = ICOPTR(spudf->faceapices[ip]);
    v[1] = ICOPTR(spudf->oe[spudf->faceoeidx[ip]]);
    v[2] = ICOPTR(spudf->oe[spudf->faceoeidx[ip] + 1]);
    printf( "facet normal %13.7lf %13.7lf %13.7lf\n"          /* plate normal */
          , vn[0], vn[1], vn[2]);
    printf( "  outer loop\n");
    for ( i=0; i<3; ++i) {                                  /* plate vertices */
      printf( "    vertex %13.7lf %13.7lf %13.7lf\n"
            , v[i][0], v[i][1], v[i][2]);
    }
    printf( "  endloop\nendfacet\n");

    
    if ( innerShell) {
    VEC v01, v02, vcross, vn;
      /* get inner shell vertices */
      v[0] = innerShell + (3*spudf->faceapices[ip]);
      v[1] = innerShell + (3*spudf->oe[spudf->faceoeidx[ip]]);
      v[2] = innerShell + (3*spudf->oe[spudf->faceoeidx[ip] + 1]);
      /* get 2 plate sides, cross them to get unit normal */
      VMINUS2( v[1], v[0], v01);
      VMINUS2( v[2], v[0], v02);
      ucrss( v01, v02, vn);
      /* flip normal to ensure normal points in */
      if ( VDOT(vn,v[1]) > 0.0) { VNEG( vn); }
      VSCAL( flipNorm, vn);
      printf( "facet normal %13.7lf %13.7lf %13.7lf\n"        /* plate normal */
            , vn[0], vn[1], vn[2]);
      printf( "  outer loop\n");
      for ( i=0; i<3; ++i) {                                /* plate vertices */
        printf( "    vertex %13.7lf %13.7lf %13.7lf\n"
              , v[i][0], v[i][1], v[i][2]);
      }
      printf( "  endloop\nendfacet\n");
    }
  }
  printf( "endsolid\n");                                         /* trailer */
  if ( shellNorms) free( shellNorms);
  return;
}

void spudprint_oogl( SPUDF *spudf) {
unsigned long i;
double *Rxyz = spudf->Rxyz;

  printf( "\nOFF\n%d %d %d\n", spudf->nv, spudf->nface, spudf->nseg);

  for ( i=0; i<(spudf->nv*3); i+=3) {
    printf( "    %f", *(Rxyz++));
    printf( " %f", *(Rxyz++));
    printf( " %f\n", *(Rxyz++));
  }
  
  for ( i=0; i<spudf->nface; i++) printf( "    3 %d %d %d\n",
      spudf->faceapices[i], spudf->oe[spudf->faceoeidx[i]], 
      spudf->oe[1+spudf->faceoeidx[i]]);
  return;
}

/******************************************************/
void spudprint_plateBareToFile( SPUDF *spudf, FILE *f) {
int iv, ip, pnum, i0, i1, i2;

long *plateNum2Ip = (long *) malloc( sizeof(long) * spudf->nface);

int oldStyle = getenv( "OLD_STYLE_SPUDPRINT") ? 1 : 0;

  if ( oldStyle) fprintf( f, "%7d\n", spudf->nv);
  else fprintf( f, "%7d %7d\n", spudf->nv, spudf->nface);

  /* print out vertices */

  if ( oldStyle) {
    for ( iv=0; iv<spudf->nv; iv++) 
      fprintf( f, "%7d%20.12e%20.12e%20.12e\n", iv+1, spudf->Rxyz[3*iv]
           , spudf->Rxyz[3*iv+1], spudf->Rxyz[3*iv+2]);
  } else {
    for ( iv=0; iv<spudf->nv; iv++) 
      fprintf( f, "%20.12e%20.12e%20.12e\n", spudf->Rxyz[3*iv]
           , spudf->Rxyz[3*iv+1], spudf->Rxyz[3*iv+2]);
  }


  /* build platenum-to-ip index */
  plateNum2Ip--;  /* ->platenum is one-based */
  for ( ip=0; ip<spudf->nface; ++ip) plateNum2Ip[spudf->platenum[ip]] = ip;

  if ( oldStyle) fprintf( f, "%7d\n", spudf->nface);

  /* print out faces IN PLATENUM ORDER */

  for ( pnum=1; pnum<=spudf->nface; pnum++) {
    ip = plateNum2Ip[pnum];
    i0 = spudf->faceapices[ip] + 1;
    i1 = spudf->oe[spudf->faceoeidx[ip]] + 1;
    i2 = spudf->oe[spudf->faceoeidx[ip] + 1] + 1;
    if ( oldStyle) 
      fprintf( f, "%7d%7d%7d%7d%20.12e\n", pnum, i0, i1, i2
            , spudf->platecolor ? spudf->platecolor[ip] : 1.0);
    else
      fprintf( f, "%7d%7d%7d%20.12e\n", i0-1, i1-1, i2-1
            , spudf->platecolor ? spudf->platecolor[ip] : 1.0);
  }
  free( ++plateNum2Ip);
}

/***************************************/
void spudprint_plateBare( SPUDF *spudf) { 
  spudprint_plateBareToFile( spudf, stdout);
}

void spudprint_plate( SPUDF *spudf) {
int iv, ip, i0, i1, i2;
int oldStyle = getenv( "OLD_STYLE_SPUDPRINT") ? 1 : 0;

  /* print out vertices */
  if ( oldStyle) printf( "%7d   ! Number of vertices\n", spudf->nv);
  else printf( "%7d %7d\n", spudf->nv, spudf->nface);

  if ( oldStyle) {
    for ( iv=0; iv<spudf->nv; iv++) 
      printf( "%7d%20.12e%20.12e%20.12e\n", iv+1, spudf->Rxyz[3*iv]
         , spudf->Rxyz[3*iv+1], spudf->Rxyz[3*iv+2]);
  } else {
    for ( iv=0; iv<spudf->nv; iv++) 
      printf( "%20.12e%20.12e%20.12e\n", spudf->Rxyz[3*iv]
           , spudf->Rxyz[3*iv+1], spudf->Rxyz[3*iv+2]);
  }

  /* print out faces */

  if ( oldStyle) printf( "%7d   ! Number of faces\n", spudf->nface);

  for ( ip=0; ip<spudf->nface; ip++) {
    i0 = spudf->faceapices[ip] + 1;
    i1 = spudf->oe[spudf->faceoeidx[ip]] + 1;
    i2 = spudf->oe[spudf->faceoeidx[ip] + 1] + 1;
    if ( oldStyle) 
      printf( "%7d%7d%7d%7d%20.12e\n", spudf->platenum[ip], i0, i1, i2
            , spudf->platecolor ? spudf->platecolor[ip] : 1.0);
    else
      printf( "%7d%7d%7d%20.12e\n", i0-1, i1-1, i2-1
            , spudf->platecolor ? spudf->platecolor[ip] : 1.0);
  }

}

/**********************************************************************/
/* the following routines are for spudprint_raysh()                   */
/**********************************************************************/
static double rpd;

/**********************************************************************/

#define VECOUT( A) (A)[0], (A)[1], (A)[2]

#define LATLONRANGE( LAT, LON) \
  if ( LAT < -90. || LAT > 90. || LON < 0. || LON > 360.) { \
    if ( LAT < -90.01) LAT += 360. * ( 1 + (int)((90.-LAT)/360.) ); \
    if ( LAT > 90.01) LAT -= 360. * ( 1 + (int)((LAT-90.)/360.) ); \
    if ( LAT < -90.01) { LAT += (LAT+90.)*2; LON += 180.; } \
    if ( LAT > 90.01) { LAT -= (LAT-90.)*2; LON += 180.; } \
    if ( LON < 0.) LON += 360. * ( 1 + (int)( (0.-LON)/360.)); \
    if ( LON >= 360.) LON -= 360. * ( 1 + (int)(LON-360.)/360.); \
  }

/**********************************************************************/
/* cross product */

#define VXV(V,VV,VVV) \
  (VVV)[0] = (V)[1] * (VV)[2] - (VV)[1] * (V)[2]; \
  (VVV)[1] = (V)[2] * (VV)[0] - (VV)[2] * (V)[0]; \
  (VVV)[2] = (V)[0] * (VV)[1] - (VV)[0] * (V)[1]

/* vector subtraction */

#define VMV(V,VV,VVV) \
  (VVV)[0] = (V)[0] - (VV)[0]; \
  (VVV)[1] = (V)[1] - (VV)[1]; \
  (VVV)[2] = (V)[2] - (VV)[2]

/* trig functions in degrees */
/* - uses rpd */

#define COS(A) cos((A)*rpd)
#define SIN(A) sin((A)*rpd)

/* lat, lon to XYZ unit vector */
/* - uses spudr->eastlon */

#define RVEC( XLAT, XLON, VEC) \
  (VEC)[0] = COS(XLON) * COS(XLAT); \
  (VEC)[1] = spudr->eastlon * SIN(XLON) * COS(XLAT); \
  (VEC)[2] = SIN(XLAT)

/* scale vector */

#define SCALEVEC( V, R) (V)[0] *= (R); (V)[1] *= (R); (V)[2] *= (R)

/* ilat, ilon to vector */
/*  - uses xlat, xlon, latdel, londel, spudr->Rmodel */

#define RVECI( ILAT, ILON, VEC) \
  xlat = ((ILAT) * latdel) - 90.0; \
  xlon = (ILON) * londel; \
  RVEC(xlat,xlon,VEC); \
  SCALEVEC( VEC, spudr->Rmodel[ILAT][ILON])

/* convert vector to unit vector */
/* - uses vlen */

#define VNORM(V) \
  vlen = (V)[0] * (V)[0] + (V)[1] * (V)[1] + (V)[2] * (V)[2]; \
  if (vlen > 0.0 && vlen != 1.0) { \
    vlen = 1.0 / sqrt( vlen); \
    SCALEVEC( V, vlen); \
  }

/**********************************************************************/
/* order 0 xyz, normal & uv mapping calculation */

static double *lltornu0( spudr, ilat, ilon) SPUDR *spudr; int ilat, ilon; {
int ilats, ilons, ilatw, ilonw, ilatn, ilonn, ilate, ilone;
int nlat=spudr->nlatR;
int nlon=spudr->nlonR;
double latdel=spudr->latdel;
double londel=spudr->londel;
double vece[3], vecw[3], vecn[3], vecs[3], vnms[3], vemw[3];
double xlat, xlon, dlat, dlon;
double vlen; /* for VNORM */
static double rnu[6];

/* DPR1( "in lltornu0 ... //"); /**/

  ilat = (ilat <= 0) ? 0 : ( (ilat >= (nlat-1)) ? nlat : ilat );
  ilon = (ilat <= 0) ? 0 : ( (ilon >= (nlon-1)) ? nlon-1 : ilon );

  ilats = ilat - 1;
  ilatn = ilat + 1;
  ilons = ilonn = ilon;

  ilonw = ilon - spudr->eastlon;
  ilone = ilon + spudr->eastlon;
  ilatw = ilate = ilat;

  /* poles */
  if ( ilat == (nlat-1)) {
    ilatn = ilats = ilate = ilatw = nlat - 2;
    ilonn = nlon / 2;
    ilons = 0;
    ilonw = ilonn + spudr->eastlon * (ilonn / 2);
    ilone = ilone - spudr->eastlon * ilonn;
  } else {
    ilatn = ilats = ilate = ilatw = 1;
    ilons = nlon / 2;
    ilonn = 0;
    ilonw = ilons + spudr->eastlon * (ilons / 2);
    ilone = ilone - spudr->eastlon * ilons;
  }

  if ( rpd == 0.0) rpd = atan(1.0) / 45.0;

  rnu[0] = spudr->Rmodel[ilat][ilon];
  RVECI(ilatn,ilonn,vecn);
  RVECI(ilats,ilons,vecs);
  RVECI(ilate,ilone,vece);
  RVECI(ilatw,ilonw,vecw);
  VMV(vecn,vecs,vnms);
  VMV(vece,vecw,vemw);
  VXV(vemw,vnms,rnu+1);
DPR((stderr," %15le %15le %15le %s", VECOUT(vecn), "=vecn\n"));
DPR((stderr," %15le %15le %15le %s", VECOUT(vecs), "=vecs\n"));
DPR((stderr," %15le %15le %15le %s", VECOUT(vece), "=vece\n"));
DPR((stderr," %15le %15le %15le %s", VECOUT(vecw), "=vecw\n"));
DPR((stderr," %15le %15le %15le %s", VECOUT(vnms), "=vnms\n"));
DPR((stderr," %15le %15le %15le %s", VECOUT(vemw), "=vemw\n"));
DPR((stderr," %15le %15le %15le %s", VECOUT(rnu+1), "=raw normal\n"));
  VNORM(rnu+1)
DPR((stderr," %15le %15le %15le %s", VECOUT(rnu+1), "=VNORM(normal)\n"));
  rnu[4] = ilon / (nlon-1.0);
  rnu[5] = ilat / (nlat-1.0);

  return( rnu);
}

/**********************************************************************/
/* order 1 lat/lon to xyz, normal and u-v mapping calculation */

static double *lltornu1( spudr, xlat, xlon) SPUDR *spudr; double xlat, xlon; {
int ilat, ilon;
int nlat=spudr->nlatR;
int nlon=spudr->nlonR;
double latdel=spudr->latdel;
double londel=spudr->londel;
int i;
double latfrac, lonfrac, llfrac;
double xxlat, xxlon;
double vlen; /* for VNORM */
double *rnu0;
static double rnu[6];

/* DPR1( "in lltornu1 ... // "); /**/
/* DPR(( stderr, "lat/londel = %lf %lf //", latdel, londel)); /**/

  xxlat = (xlat+90.) / latdel;
  xxlon = xlon / londel;

/* DPR(( stderr, "xxlat,xxlon = %lf %lf //", xxlat, xxlon)); /**/

  ilat = (xxlat <= 0.0) ? 0 : ( (xxlat > (nlat-2)) ? (nlat-2) : (int) xxlat );
  ilon = (xxlon <= 0.0) ? 0 : ( (xxlon > (nlon-2)) ? (nlon-2) : (int) xxlon );

  latfrac = xxlat - ilat;
  lonfrac = xxlon - ilon;

/* DPR(( stderr, "lat/lonfrac = %lf %lf //", latfrac, lonfrac)); /**/

  for ( i=0; i<6; i++) rnu[i] = 0.0;

  /* linear interpolation of R, normal, & u-v mapping */

#define RNU(ILAT,ILON,LLFRAC) \
  rnu0 = lltornu0( spudr, (ILAT), (ILON)); \
  llfrac = (LLFRAC); \
  for ( i=0; i<6; i++) rnu[i] += (rnu0[i] * llfrac)

  RNU( ilat, ilon, (1.0-latfrac)*(1.0-lonfrac));

/* DPR1( "in lltornu1 ... // "); /**/
  RNU( ilat+1, ilon, latfrac*(1.0-lonfrac));
  RNU( ilat, ilon+1, (1.0-latfrac)*lonfrac);
  RNU( ilat+1, ilon+1, latfrac*lonfrac);
  VNORM(rnu+1)

  return( rnu);
}

/**********************************************************************/
/* order 1 xyz to xyz, normal and u-v mapping calculation */

static double *xyztoxyznu1( spudr, xyz0) SPUDR *spudr; double *xyz0; {
int i;
double xlat, xlon;
double vlen; /* for VNORM */
double *rnu1;
static double xyzrnu[8];

  if ( rpd == 0.0) rpd = atan(1.0) / 45.0;
  for ( i=0; i<3; i++) xyzrnu[i] = xyz0[i];
/* DPR((stderr, "xyzrnu[2] = %15le // ", xyzrnu[2])); /**/
  VNORM( xyzrnu)
/* DPR((stderr, "norm xyzrnu[2] = %15le\n", xyzrnu[2])); /**/

  xlat = asin( xyzrnu[2]) / rpd;
  if ( xyzrnu[0] != 0.0 || xyzrnu[1] != 0.0) {
    xlon = atan2( spudr->eastlon * xyzrnu[1], xyzrnu[0]) / rpd;
    if ( xlon < 0.0) xlon += 360.0;
  } else {
    xlon = 0.0;
  }

/* DPR(( stderr, "%lf %lf=xlat, xlon\n", xlat, xlon)); /**/

  rnu1 = lltornu1( spudr, xlat, xlon);
  for ( i=1; i<6; i++) xyzrnu[i+2] = rnu1[i];
  SCALEVEC( xyzrnu, rnu1[0]);
DPR((stderr, "xyz0=%15le %15le %15le\n", VECOUT( xyz0))); /**/
/* DPR((stderr, "rnu1[1:2:3]=%15le %15le %15le\n", VECOUT( rnu1+1))); /**/
DPR((stderr, "xyzrnu=%15le %15le %15le\n", VECOUT( xyzrnu))); /**/

  return( xyzrnu);
}

void spudprint_raysh( SPUDR *spudr, SPUDF *spudf) {
long iv, ip, i0, i1, i2;
double *xyzrnu;

  printf( "name spud list\n");
  
  for ( ip=0; ip<spudf->nface; ip++) {
    printf(" triangle\n");

fflush(stdout);

#define VERTEXPRINT(I) \
    xyzrnu = xyztoxyznu1( spudr, spudf->Rxyz+(3*(I))); \
    printf( "  %20.12e%20.12e%20.12e\n  %20.12e%20.12e%20.12e\n" \
         , xyzrnu[0], xyzrnu[1], xyzrnu[2], xyzrnu[3], xyzrnu[4], xyzrnu[5])

    VERTEXPRINT( spudf->faceapices[ip]);
    VERTEXPRINT( spudf->oe[spudf->faceoeidx[ip]]);
    VERTEXPRINT( spudf->oe[spudf->faceoeidx[ip] + 1]);
  }
  printf( "end\n");
  return;
}
