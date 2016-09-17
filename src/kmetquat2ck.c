/* convert flat file with lines containing  
 *
 *     [kMET|MET|SCLK] QUATX QUATY QUATZ QUATROT
 *
 * to a C Kernel
 *
 * - matrix from input quat converts inertial (J2k) vector to Spacecraft (SBF)
 *   vector when using SPICE MXV i.e.
 *
 * Q = (Qrot, Qx, Qy, Qz)
 * Vj2k = (Xj2k, Yj2k, Zj2k)     ! Vector in J2k
 * call q2m( Q, mtx)
 * call mxv( mtx, Vj2k, Vsc)     ! Vsc is in Spacecraft coordinates
 *
 * - if meaning of quat is opposite (SBF to J2k), specify -flip as last argument
 */
#include <stdio.h>
#include <string.h>
#include <math.h>
#define _ORBITFORT_H_TYPESONLY_
#include "orbitfort.h"
#include "orbit_spice_names.h"

#define USAGE \
  fprintf( stderr, "USAGE:\n  sclkquat2ck %s %s %s %s %s %s %s %s\n" \
    , "<output_CK_filename>" \
    , "ScID" \
    , "InstrID" \
    , "<SCLKKernelFilename>\n" \
    , "    <LEAPSECONDKernelFilename>" \
    , "[-i" \
    , "<input_filename>]" \
    , "[-flip]"); \
    return 

#define RTN( FMT, A, B) \
  fprintf( stderr, FMT, A, B); fflush( stderr); \
  if ( fIn && (fIn != stdin)) fclose( fIn); \
  if ( cIn) fclose( cIn); \
  return
#define ERRRTN( FMT, A, B) RTN( FMT, A, B) -1
#define OKRTN( FMT, A, B) RTN( FMT, A, B) 0

#ifndef MAXNCKREC
#define MAXNCKREC 5000
#endif

#define CALCAV( I0, I1, IAV) \
  sct2e( &scId, sclkarr+I0, &et0); \
  sct2e( &scId, sclkarr+I1, &et1); \
  ospice_qqt2av( quatarr[I1], quatarr[I0], &et1, &et0, avarr[IAV])

/* macro to write out a segment - does nothing if no records loaded yet */

#define WRITESEG \
  if ( nRec) { \
  fortint inew = (nSeg ? 0 : 1); \
  fortint writav = 1; \
    /********************************/ \
    /* calculate angular velocities */ \
    /********************************/ \
    if ( nRec == 1) { \
      avarr[0][0] = avarr[0][1] = avarr[0][2] = 0.0; \
    } else { \
      for ( i=0; i<nRec; ++i) { \
        if ( !i) { \
          CALCAV( 1, 0, i); \
        } else if ( i == (nRec-1)) { \
          CALCAV( i, i-1, i); \
        } else { \
          CALCAV( i+1, i-1, i); \
        } \
      } \
    } \
    /*********************/ \
    /* write the segment */ \
    /*********************/ \
    ospice_writeck( &inew, &instrId, &writav \
                  , &nRec, sclkarr, quatarr, avarr \
                  , &numIntervals, sclkarr, &ckfnlen, ckfn); \
    /*********************/ \
    if ( !(nSeg++)) { \
      fprintf( stderr, "Writing C Kernel file '%s' ...", ckfn); \
      fflush( stderr); \
    } \
  }

int
main( int argc, char **argv) {
FILE *fIn = stdin;
FILE *cIn = (FILE *) 0;
long nSeg;
fortint inew, instrId, nRec, numIntervals, ckfnlen;
fortint scId, i;
double sclkarr[MAXNCKREC], *sclkptr;
double quatarr[MAXNCKREC][4], *quatptr;
double avarr[MAXNCKREC][3];
double et0, et1, lastsclk;
double lineNo;
char *sclkFn;
fortint sclkFnLen;
char *leapFn;
fortint leapFnLen;
char *ckfn;
char *inpFn = { "standard input" };
char *cmtFn = (char *) 0;
char inpLine[1024];
fortint f4 = 4;
double umag;

int whoAmI;
enum { WHOAMI_SCLK=0, WHOAMI_kMET, WHOAMI_MET };
#define isSCLK (whoAmI==WHOAMI_SCLK)
#define isMET (whoAmI==WHOAMI_MET)
#define iskMET (whoAmI==WHOAMI_kMET)
char *cptr, *dpt;
char sclkch[255];
fortint lenSclkch;
#define kMETNAME "kmetquat2ck"
#define METNAME "metquat2ck"

int flipQuat;

  /* check if argv[0] to find kMETNAME, METNAME or not */

  whoAmI = WHOAMI_SCLK;

  if ( isSCLK) {
    cptr = argv[0] + strlen(argv[0]) - strlen(kMETNAME);
    if ( cptr >= argv[0] ) 
      if ( !strcmp( kMETNAME, cptr)) 
        whoAmI = WHOAMI_kMET;
  }

  if ( isSCLK) {
    cptr = argv[0] + strlen(argv[0]) - strlen(METNAME);
    if ( cptr >= argv[0] ) 
      if ( !strcmp( METNAME, cptr)) 
        whoAmI = WHOAMI_MET;
  }
 
  argc -= (flipQuat = (strcmp(argv[argc-1],"-flip") ? 0 : 1) );

  switch ( argc) {
  case 2:
    USAGE (strcmp( argv[1], "-h") ? -1 : 0);
    break;

  case 6:
    break;

  case 8:
  case 10:

    for ( i=6; i<argc; i+=2) {
      if ( !strcmp( "-i", argv[i]) ) {
        inpFn = argv[i+1];
        fIn = fopen( inpFn, "r");
        if ( !fIn) { 
          ERRRTN("Problem opening input file '%s'; exiting ...\n%s",inpFn,"");
        }
      } else if ( !strcmp( "-c", argv[i]) ) { 
        cmtFn = argv[i+1];
        cIn = fopen( cmtFn, "r");
        if ( !cIn) { 
          ERRRTN("Problem opening comment file '%s'; exiting ...\n%s",cmtFn,"");
        }
      } else { USAGE -1; }
    }
    break;

  default:
    USAGE -1;
    break;
  } /*  switch argc */

  ckfn = argv[1];
  ckfnlen = strlen( ckfn);
  scId = atoi( argv[2]);
  instrId = atoi( argv[3]);
  sclkFn = argv[4];
  sclkFnLen = strlen( sclkFn);
  leapFn = argv[5];
  leapFnLen = strlen( leapFn);

  ospice_clpool();
  ospice_ldpool( &sclkFnLen, sclkFn);
  ospice_ldpool( &leapFnLen, leapFn);

  remove( ckfn);    /* ensure it does not exist */

  nSeg = 0;                /* first call to ospice_writeck will create a file */
  lastsclk = -1.0;                                   /* ensure initialization */
  sclkarr[0] = 0.0;
  nRec = 0;   /* ensure no file written on initialization - see WRITECK macro */
  numIntervals = 1;

  sclkptr = sclkarr;
  quatptr = quatarr[0];

  lineNo = 0.0;

  fprintf( stderr, "Reading data from file %s ...\n", inpFn);

  while ( fgets( inpLine, 1024, fIn)) {
  double tmpSclk;

    lineNo += 1.0;

    if ( 1 != sscanf( inpLine, "%s", sclkch)) {
      ERRRTN( "Problem reading SCLK/kMET/MET from %s, line %1lg; exiting...\n"
           , inpFn, lineNo);
    }

    /* convert ascii MET/kMET/SCLK to SCLK tiks */

    if ( iskMET || isMET) {

      /* convert kMET to MET */

      if ( (dpt = strchr( sclkch, '.'))) {          /* look for decimal point */

        /* if a decimal point is found, treat as kMET:
         * - shift left the three characters after the decimal point
         * - append null terminator
         */

        strcat( sclkch, "000");  /* ensure enough digits follow decimal point */
        for ( i=0; i<3; ++i, ++dpt) *dpt = dpt[1];        /* do the shift */
        *dpt = '\0';

      } else if ( iskMET) strcat( sclkch, "000");

      /* convert MET string to tiks */

      lenSclkch = strlen( sclkch);
      ospice_scencd( &scId, &tmpSclk, &lenSclkch, sclkch);

    } else {
      sscanf( sclkch, "%lf", &tmpSclk);        /* isSCLK - read SCLK directly */
    }

    /* if time reverses or arrays fill up, write current segment & start next */

    if ( tmpSclk <= lastsclk || nRec == MAXNCKREC ) {
      WRITESEG

      if ( nRec && tmpSclk > lastsclk) {          /* overlap ends of segments */
        *sclkarr = lastsclk;
        memcpy( quatarr[0], quatptr-4, 4 * sizeof(double));
        nRec = 1;
        sclkptr = sclkarr+1;
        quatptr = quatarr[1];

      } else {                                           /* start new segment */

        nRec = 0;
        sclkptr = sclkarr;
        quatptr = quatarr[0];
      }
    }

    /* re-read [k]MET/SCLK & read rest of line (quaternion) */

    if ( 5 != sscanf( inpLine, "%s %lf %lf %lf %lf"
                    , sclkch, quatptr+1, quatptr+2, quatptr+3, quatptr+0)) {
      ERRRTN( "Problem reading SCLK & QUAT from %s line %1lg; exiting...\n"
            , inpFn, lineNo);
    }
    unormg( quatptr, &f4, quatptr, &umag);            /* normalize quaternion */

    if ( flipQuat) vminus( quatptr+1, quatptr+1);          /* flip quaternion */

    lastsclk = *sclkptr = tmpSclk;                               /* save sclk */

    sclkptr++;
    quatptr += 4;
    nRec++;
  }

  WRITESEG            /* flush segment */

  OKRTN( " done, %ld segment%s written\n", nSeg, (nSeg == 1) ? "" : "s");
}
