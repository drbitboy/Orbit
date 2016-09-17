#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "spudshap.h"

int main( argc, argv) unsigned int argc; char **argv; {
SPUDR           spudr;
SPUDF           spudf;
void            spudprint_raysh(SPUDR*,SPUDF*);

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;
  spudr.eastlon = getview( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR); /**/
  if ( spudr.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;
  spudr.latdel = 180.0 / (spudr.nlatR-1);
  spudr.londel = 360.0 / (spudr.nlonR-1);

  rmod2face( &spudr, &spudf);

  spudprint_raysh( &spudr, &spudf);

  return 0;
}
