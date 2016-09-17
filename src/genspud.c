#include <stdio.h>
#include <math.h>
#define MIN(A,B) ((A)<(B)?(A):(B))
int main( int argc, char **argv) {
/* int delat, delon, nlat, nlon, i, j; */
double delat, delon;
int nlat, nlon, i, j;
double a, b, c, dlat, dlon, rpd;
double sinlat, coslat, sinlon, coslon;
double x, y, z, sc, r;
char ccc[1000];

#define USAGE { \
  fprintf( stderr, \
  "Usage:  genspud delat delon A B C > newshapemodelfile\n A/B/C=semi-major axes (i.e. radii)\n"); \
  exit(1); \
  }

  if ( argc != 6) USAGE
  sprintf( ccc, "%s %s %s %s %s", argv[1], argv[2], argv[3], argv[4], argv[5]);
  /* if ( 5 != sscanf( ccc, "%d %d %lf %lf %lf", &delat,&delon,&a,&b,&c))USAGE*/
  if ( 5 != sscanf( ccc, "%lf %lf %lf %lf %lf", &delat,&delon,&a,&b,&c)) USAGE
  if ( MIN(a,MIN(b,c)) <= 0.0) USAGE
  if ( MIN(delat,delon) <= 0.0) USAGE

  nlat = 180 / delat;
  nlon = 360 / delon;
  /* if ( MIN(MIN( delat,delon), MIN(nlat,nlon)) < 1) USAGE */
  if ( MIN(nlat,nlon) < 1) USAGE

  dlat = 180.0 / nlat;
  dlon = 360.0 / nlon;

  rpd = atan(1.0) / 45.0;
#define deg *rpd

  for ( j=0; j<=nlon; ++j) 
    printf( "  -90.0000%10.4lf%10.4lf\n", j*dlon, c);

  for ( i=1; i<nlat; ++i) {
    coslat = cos((i*dlat - 90) deg);
    sinlat = sin((i*dlat - 90) deg);
    for ( j=0; j<=nlon; ++j) {
      coslon = cos(j*dlon deg);
      sinlon = sin(j*dlon deg);
      x = coslon * coslat / a;
      y = sinlon * coslat / b;
      z = sinlat / c;
      sc = 1.0 / sqrt( x*x + y*y + z*z);
      x *= (sc * a);
      y *= (sc * b);
      z *= (sc * c);
      r = sqrt( x*x + y*y + z*z);
      printf( "%10.4lf%10.4lf%10.4lf\n", i*dlat-90, j*dlon, r);
    }
  }

  for ( j=0; j<=nlon; ++j) 
    printf( "   90.0000%10.4lf%10.4lf\n", j*dlon, c);

  return(0);
}
