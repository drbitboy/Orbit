#include <stdio.h>
#include "spudshap.h"

int 
main () {

SPUDR spudr;
SPUDF *spudf;
#define r spudr.Rmodel

  spudf = getplat( &spudr);

  if ( spudf) 
    printf( "nv nseg nface = %d %d %d\n", spudf->nv, spudf->nseg, spudf->nface);
  else
    fprintf( stderr, "getplat failed\n");
  return 0;
}
