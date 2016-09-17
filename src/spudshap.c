/* spudshap.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "spudshap.h"

#ifdef VMS
#define FNAME1 "view.dat"
#define FNAME2 FNAME1
#else
#define FNAME1 "VIEW"
#define FNAME2 "view"
#endif

static char fname1[] = { FNAME1 };
static char fname2[] = { FNAME2 };

/* read text shape model, return -1 for error, 0/1 for west/east longitude */

int 
getviewByname( double *r, unsigned long *nlat, unsigned long *nlon
             , char *viewFN, char **outFN) {
int latsiz= *nlat, lonsiz= *nlon;
int ilat, ilon;
double latdel, londel;
double lat, lon, rad, oldlat, oldlon;
FILE *f;
int scanstat;
int more;

char *fname;
char str[255];

#define RTN(A) \
  return( fprintf( stderr, "%s, ilat/ilon=%d/%d\n", A, ilat, ilon) \
        , fflush(stderr) \
        , -1)

  ilat = ilon = -1;

  if ( outFN) *outFN = (char *) 0;

  /* try to open file viewFN, environment variable VIEW, view,
   * or file "VIEW" or "view"
   */
  fname = viewFN;
  if ( !fname) fname = getenv( "VIEW");
  /* if ( !fname) fname = getenv( "view"); */
  if ( !fname) fname = fname1;
  f = fopen( fname, "r");

  if ( !f && viewFN) {
    fprintf( stderr, "Problem opening shape model file:\n");
    RTN( viewFN);
  }

  if ( !f) fname = fname2;
  f = fopen( fname, "r");
  if ( !f) RTN( "Problem opening shape model file");

  if ( outFN) *outFN = fname;

/* get initial lat (-90), lon=0-360 */

#define SCAN3 \
  oldlat = lat; \
  oldlon = lon; \
  scanstat = fscanf( f, "%lf %lf %lf", &lat, &lon, r+(lonsiz*ilat)+ilon); \
  if ( scanstat != 3) RTN( "Problem reading lat, lon, R"); \
  ilon++

  ilat=0;
  ilon=0;
  SCAN3;
  if ( lat != -90. || lon != 0.) 
    RTN( "First line of shape model file not lat lon = -90 0");
  SCAN3;
  if ( lon <= 0. || lon > 180.0) RTN( "Bad delta longitude");
  londel = lon;

  while ( lat == oldlat) {
    SCAN3;
    if ( londel != (lon-oldlon)) if (lat == oldlat)
      RTN( "Irregular delta longitude (A)");
  }
  if ( lon != 0. || oldlon != 360.) RTN( "Bad longitiude transition");

/* move last lon to start of next lat */
  *nlon = ilon-1;
  r[lonsiz] = r[*nlon];
  ilat = 1;
  ilon = 1;
  latdel = lat - oldlat;
  if ( latdel <= 0. || latdel > 90.) RTN( "Bad delta latitude");

/* read rest of lines */

  while ( ilon != *nlon) {
    SCAN3;
    if ( ilon == 1) {
      if ( lon != 0.) RTN( "First lon not 0");
      if ( latdel != (lat-oldlat)) RTN( "Irregular delta latitude");
    } else {
      if ( londel != (lon-oldlon) ) RTN( "Irregular delta longitude (B)");
      if ( lat != oldlat) RTN( "Bad latitude");
      if ( ilon == *nlon) {
        if ( lon != 360.) RTN( "Last lon not 360");
        if ( lat < 90.) ilon = 0;
        else if ( lat > 90.) RTN( "Bad lat, > 90");
        ilat++;
      }
    }
  }
  *nlat = ilat;
  *str = '\0';
  fscanf( f, "%s", str);
  fclose( f);
  if ( !strcmp( str, "eastlon") || !strcmp( str, "EASTLON") ) return(1);
  else return(0);
}

/**********************************************************************/

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
/* lat,lon to Radius - 2d linear interpolation */

double lltor1( SPUDR *spudr, double xlat, double xlon) {
int ilat, ilon, ila1, ilo1;
double rrr;

  LATLONRANGE( xlat, xlon)

  xlat = (xlat + 90.) * (spudr->nlatR-1) / 180.;
  xlon = xlon * (spudr->nlonR-1) / 360.;

  ilat = (int) xlat;
  ilon = (int) xlon;
  xlat -= ilat;
  xlon -= ilon;

  ila1 = ilat + ((ilat < (spudr->nlatR-1)) ? 1 : 0);
  ilo1 = ilon + ((ilon < (spudr->nlonR-1)) ? 1 : 0);

  rrr = ((1.-xlat)*(1.-xlon) * spudr->Rmodel[ilat][ilon])
      + ((   xlat)*(1.-xlon) * spudr->Rmodel[ila1][ilon])
      + ((1.-xlat)*(   xlon) * spudr->Rmodel[ilat][ilo1])
      + ((   xlat)*(   xlon) * spudr->Rmodel[ila1][ilo1])
      ;

  return( rrr);
}

/**********************************************************************/
/* lat,lon to Radius - nearest neighbor interpolation */

double lltor0( spudr, xlat, xlon) SPUDR *spudr; double xlat, xlon; {
int ilat, ilon;

  LATLONRANGE( xlat, xlon)

  ilat = (int) (0.5 + (xlat + 90.) * (spudr->nlatR-1) / 180. );
  ilon = (int) (0.5 + xlon * (spudr->nlonR-1) / 360. );

  return( spudr->Rmodel[ilat][ilon]);
}
