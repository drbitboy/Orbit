#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "spudshap.h"

#define deg		*3.14159265358979323846/180.
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

void display_3d();

int main( argc, argv) unsigned int argc; char **argv; {
SPUDR		spudr;
SPUDF		spudf;
SPUDV		*spudv;
time_t		ttt;

  ttt=time( (time_t *)0);
  printf( "time=%d\n", ttt);

  spudr.nlatR = SPUDRNLAT;
  spudr.nlonR = SPUDRNLON;
  spudr.eastlon = getview( spudr.Rmodel, &spudr.nlatR, &spudr.nlonR); /**/
  if ( spudr.eastlon == -1) {
    fprintf( stderr, "Failed to load shape model\n");
    return(0);
  }
  if ( !spudr.eastlon) spudr.eastlon = -1;

  rmod2face( &spudr, &spudf);

  spudv = newspudv( &spudf);

  ttt=time( (time_t *)0);
  printf( "time=%d\n", ttt);

}
