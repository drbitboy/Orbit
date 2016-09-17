#include <stdio.h>
#include <string.h>
#include <math.h>

#include "spudshap.h"

int main( argc, argv) unsigned int argc; char **argv; {
char *fnSpud = (argc > 1) ? argv[1] : (char *) 0;
SPUDR spudr;
SPUDF spudf;

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;
  spudr.eastlon = getviewByname( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR
                               , fnSpud, (char **) 0);
  if ( spudr.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;

  rmod2face( &spudr, &spudf);

  spudprint_plate( &spudf);

  return 0;
}
