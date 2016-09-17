#include <stdio.h>
/* #include "orbit3d.h" */
#include "spudshap.h"

int main( argc, argv) unsigned int argc; char **argv; {
SPUDV *spudv;
char *fnShape = (argc > 1) ? argv[1] : (char *) 0;
void spudprint_xplate(SPUDF*);

  if ( !(spudv = getSpudvByname( fnShape)) ) return 0;
  spudprint_xplate( spudv->_spudf);
  return 0;
}
