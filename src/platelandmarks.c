#include <stdio.h>
#include <math.h>
#include "spudshap.h"
#include "orbit3d.h"

static void usage( char **ptr) {
  fprintf( stderr, "USAGE:\n");
  while ( *ptr) fprintf( stderr, "%s \\\n  ", *(ptr++));
  fprintf( stderr, "\n");
  return;
}

int
main ( int ac, char **av) {
int i;
SPUDR spudr;
SPUDF *spudf;
#define r spudr.Rmodel
char *rtnName;

char *inpPltPath = (char *) 0;
char *inpLmPath = (char *) 0;
FILE *inpLmFile = (FILE *) 0;
char *outPltPath = (char *) 0;
FILE *outPltFile = (FILE *) 0;

int doSave = 0;
double *pC;
double *vert;
double bigrad;
unsigned long nv, nf, lclNFace, lclNVert;
int errRtn = -1;
char inpLine[256];
char tail[20];

char *usageOpts[] = {
  *av
, "[-i <plateModelPath>]  #input plate model file; dflt:  use PLATES env var"
, "[-o <plateModelPath>]  #output plate model file; dflt:  use stdout"
, "[-l <landmarkPath>]    #landmark file; dflt:  use stdin"
, "[-s[ave]]              #save input plate model landmarks; dflt:  clear"
, "[-h[elp]]              #print out this help message & exit"
, (char *) 0
};

void spudprint_plateBareToFile(SPUDF*, FILE*);

#define ERRRTN(R) RTN(  usage(usageOpts);, R)

#define RTN(U,R) { \
  U \
  if (inpLmFile && inpLmFile != stdin) fclose(inpLmFile); \
  if (outPltFile && outPltFile != stdout) fclose(outPltFile); \
  return(R); \
}

  for ( i=1; i<ac; ++i) {
    switch ( *av[i]) {
    case '-':
      switch( av[i][1]) {
      case 'h': ERRRTN(0) break;
      case 'i':
        if ( !(inpPltPath=av[i][2]?(av[i]+2): ((++i<ac)?av[i]:(char *)0)) ) {
          ERRRTN(-1)
        }
        break;
      case 'o':
        if ( !(outPltPath=av[i][2]?(av[i]+2): ((++i<ac)?av[i]:(char *)0)) ) {
          ERRRTN(-2)
        }
        break;
      case 'l':
        if ( !(inpLmPath=av[i][2]?(av[i]+2): ((++i<ac)?av[i]:(char *)0)) ) {
          ERRRTN(-3)
        }
        break;
      case 's':
        doSave = 1;
        break;
      default:
        ERRRTN(-4)
        break;
      }
      break;
    default:
      ERRRTN(-5)
      break;
    }
  }

  spudf = getplatByname( &spudr, inpPltPath, &rtnName);

  if ( !spudf) {
    fprintf( stderr, "getplat failed\n");
    ERRRTN(-6);
  }

  if ( !(inpLmFile = inpLmPath ? fopen( inpLmPath, "r") : stdin) ) {
    fprintf( stderr, "Landmark input file fopen(`%s',`r') failed\n"
                   , inpLmPath ? inpLmPath : "<stdin?>");
    ERRRTN(-7)
  }

  fprintf( stderr, "nv nseg nface = %lu %lu %lu\n"
                 , nv=spudf->nv, spudf->nseg, nf=spudf->nface);

  if ( !doSave) {                                              /* init colors */
    lclNFace = nf;
    pC = spudf->platecolor + lclNFace;
    while ( lclNFace--) { *(--pC) = -1.0; }
  }

  vert = spudf->Rxyz + ((lclNVert = nv) * 3);
  bigrad = 10.0;            /* bigrad will be 10 if max radius is <= sqrt(10) */
  while ( lclNVert-- ) {
  double r2;
    vert -= 3;
    if ( (r2=VDOT(vert,vert)) > bigrad) bigrad = r2;
  }

  /* read each in landmark file, interpret those that have 
   *   5 numbers ( %ld %ld %lf %lf %lf ) on them as landmarks:
   *
   *   <LandmarkID#> <ignore#> <LandmarkAbfX>  <LandmarkAbfY>  <LandmarkAbfZ> 
   *       %ld          %ld         %lf             %lf             %lf
   * 
   * intersect plate model with nadir vector from point that is scaled 
   *   from landmark vector & is outside plate model
   * - set color of intersected plate to be landmark ID #
   */
  while ( fgets( inpLine, 255, inpLmFile) ) {                  /* read a line */
  char *cPtr;
  double scal, dist;
  unsigned long pltHit;
  VEC sc, bore;
  long lin[2];
    if ( (cPtr=strchr( inpLine, '\n')) ) *cPtr = '\0';
    if ( 5 == sscanf( inpLine, "%ld %ld %lf %lf %lf %10s"        /* scan line */
                             , lin, lin+1, sc, sc+1, sc+2, tail))/* for 5 #'s */

    if ( 0.0 < (dist=VLEN(sc)) ) {               /* if landmark is not origin */

      if ( 2.0 > (scal = bigrad / dist)) scal = 2.0;    /* scale at least 2:1 */
      VSCAL(scal,sc);                       /* scale landmark outside surface */
      VNEG2( sc, bore);                      /* point back at (0,0,0) (nadir) */

      spudf_intersect( spudf, sc, bore, &dist, &pltHit); /* find intersection */
      if ( pltHit < nf) {                       /* if intersection successful */

        pC = spudf->platecolor + pltHit;            /* get platecolor pointer */

        if ( *pC > -1.0) {      /* flag if this plate already has a landmark */
          fprintf( stderr, "***Old landmark %lf on plate %ld\n"
                         , *pC, spudf->platenum[pltHit]);
        }

        fprintf( stderr, "%s landmark %ld on plate %ld\n"
                       , (*pC>-1.0)?"Overwriting":"Adding"
                       , *lin, spudf->platenum[pltHit]);

        *pC = (double) *lin;                            /* set the platecolor */

      } else {                                           /* if pltHit<nf ... */ 
        fprintf( stderr, "***Error:  Landmark not on any plate:\n=> %s\n"
                       , inpLine);

      } /* if pltHit<nf ... else ... */
    } /* if (5 == sscanf()) if 0.0 < dist ... */
  } /* while fgets ... */

  if ( inpLmFile != stdin) fclose( inpLmFile);
  inpLmFile = (FILE *) 0;

  /* output plate model file */

  if ( !(outPltFile = outPltPath ? fopen( outPltPath, "w") : stdout) ) {
    fprintf( stderr, "Plate model output file fopen(`%s',`w') failed\n"
                   , outPltPath ? outPltPath : "<stdout?>");
    ERRRTN(-8)
  }

  spudprint_plateBareToFile( spudf, outPltFile);

  RTN(;,0);
}
