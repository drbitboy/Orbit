/* spudshap_raysh.c */

#ifdef DEBUG
#undef DEBUG
#endif
/**/
/*
#ifndef DEBUG
#define DEBUG
#endif
/**/

#include "debug.h"

#include <stdio.h>
#include <math.h>

#include "spudshap.h"

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

double *lltornu0( spudr, ilat, ilon) SPUDR *spudr; int ilat, ilon; {
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

double *lltornu1( spudr, xlat, xlon) SPUDR *spudr; double xlat, xlon; {
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

double *xyztoxyznu1( spudr, xyz0) SPUDR *spudr; double *xyz0; {
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
