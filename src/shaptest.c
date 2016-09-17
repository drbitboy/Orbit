#include <stdio.h>
#include "spudshap.h"

main () {

int eastlon;
int nlon=SPUDRNLON;
int nlat=SPUDRNLAT;
SPUDR spudr;
#define r spudr.Rmodel

  eastlon = getview( r, &nlat, &nlon);

  if ( eastlon == -1) fprintf( stderr, "ERROR in getview()\n");

  printf( "Longitude: %s; Nlat = %d; Nlon = %d\n",
           (!eastlon)?"West":((eastlon==1)?"East":"Unknown"), nlat, nlon);
}
