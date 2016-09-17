/* u2e.c determine start & stop times for a kernel
 * INPUTS:
 *   argv[1]  startUTC
 *   argv[2]  endDeterminedUTC
 *   stdin    (Optional) 0brief output:  One line of "SPKO: <fil.bsp> ET0 ET1
 *
 * OUTPUT
 *   stdout   <max(startUTC,ET0)> <endDeterminedUTC> 
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "debug.h"

#define _ORBITFORT_H_TYPESONLY_
#include "orbitfort.h"

#include "orbit_spice_names.h"

int
main(int argc, char **argv) {
double et2, et3;
fortint l;
fortint failed();
char c0[256], c1[256];
double etStdin2, etStdin3;

  if ( argc < 4) exit(-1);
  
  /* load leapseconds into kernel pool file e.g. near.tls */

  reset();

# define IFE(I) if ( failed() ) exit(I)
# define IFSA(AI,I) if ( (l=(fortint)strlen(argv[AI])) < 1) exit(I);

  ospice_clpool();                                 IFE(-2);
                                                   IFSA(1,-3);
  ospice_ldpool( &l, argv[1]);                     IFE(-4);

  /* convert UTC 1 to ET 1 */
                                                   IFSA(2,-5);
  ospice_utc2et( &et2, &l, argv[2]);               IFE(-6);

                                                   IFSA(3,-7);
  ospice_utc2et( &et3, &l, argv[3]);               IFE(-8);

  if ( 4 == fscanf( stdin, "%s %s %lf %lf", c0, c1, &etStdin2, &etStdin3)) {
    if ( !strcmp( c0, "SPKO:")) {
      et2 = (et2 > etStdin2) ? et2 : etStdin2;
      et3 = (et3 < etStdin3) ? et3 : etStdin3;
    }
  }

  fprintf( stdout, " %lf %lf\n", et2, et3);          return 0;
}
