/* fixOrbdb.c - input orbit .frames file, output photometric geometry for
 * each plate from a shape model
 *
 * Usage:
 *
 *   fixOrbdb -orbdb <orbdb prefix> [input options] \
 *            -intype <inputType> [inTypeOptions]
 *
 * Purpose:
 *
 *   Read file with observation info on it, update instrPar & miscVals in orbdb
 *
 * Options:
 *
 *   -orbdb <orbdbfn>      Update <orbdbfn>_{met,plt}.orbdb
 * 
 *   -framesin <framesfn>  Input frames from file <framesfn> 
 *                         (default:  standard input, "" => none)
 *
 *   -verbose        output progress if orbdb is used
 *   -debug          output more detailed progress if orbdb is used
 *
 ***********************************************************************
 * ***N.B. THE FOLLOWING OPTION(S) MUST COME AFTER ALL OTHER OPTIONS 
 *         LISTED ABOVE
 ***********************************************************************
 *
 *   -intype <intype>  [intype options]
 *
 *                   type of input frames file & related options
 *
 *                   <intype>  
 *
 *                   string to indicate type of input file from which
 *                   to get frames' info.  some possible entries are kMET,
 *                   MET, ET, UTC, SCLK.  see file toiep.c for all possibilies. 
 *                   or run "fixOrbdb -intype list[full]".
 *                   defaults to original orbit frames file using 
 *                   fixOrbdb_readnextOrig() function in this module.
 *
 *                   [intype options] 
 *
 *                   options relating to <intype>; will be 
 *                   interpreted by the appropriate *2iep_init routine
 *                   run "fixOrbdb -intype listfull" to see available options.
 */

#define NEARPXLSIZ sqrt(161E-6 * 95E-6)  /* radians per pixel */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "debug.h"
#include "orbit_spice_names.h"
#include "orbit3d.h"
#include "orbit_stats.h"
#include "spudshap.h"
#include "orbdb.h"

#define MXFOVVTX 100
#define deg *M_PI/180.
#define r2d(a) (a*180.0/M_PI)
#if 0
#define deg *3.14159265358979323846/180.
#define r2d(a) (a*180.0/3.14159265358979323846)
#endif
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

/*************/
/* -<option> */

enum {
   OPT_VERBOSE
 , OPT_FRAMESIN
 , OPT_INTYPE
 , OPT_ORBDB
 , OPT_DEBUG
 , OPT_HELP
 , OPT_COUNT};

static OPTION opts[] = {
 "verbose", OPT_VERBOSE, "output progress if -orbdb used"
 , "framesin", OPT_FRAMESIN, "-framesin <frames filepath>"
 , "intype", OPT_INTYPE
           , "'-intype <intype> [opts]', run frm2iep -intype listfull for more"
 , "orbdb", OPT_ORBDB, "-orbdb <orbdb prefix>, write to ORBit DataBase"
 , "debug", OPT_DEBUG, "lots of progress output"
 , "h", OPT_HELP, "output this list"
 , "help", OPT_HELP, "output this list"
 , (char *) 0, OPT_COUNT, ""
 };

/****************************************************/
/* start of macros & routine to read a single frame */

long readNextLineno;

#include "toiep.h"

/* read next frame from frames file
 *  0 for all items read
 * -1 for end of file, nothing read
 * >0 read error - current istate | errorBIT
 * xyzBIT for error reading a line of type xyz e.g. a second line of that type
 */

long
fixOrbdb_readnextOrig( FILE *f, char *timch, VEC sc, VEC vsun, VEC p5
                    , long *fovcount, VEC campts[], char *metch
                    , double *obsParm, MTX cam2Abf, double *miscVals)
{

  printf( "fixOrbdb:  you must use -intype <intype>\n");
  return -1;
}

int
fixOrbdb_main(int argc, char **argv) {
int savargc = argc;
char **savargv = argv;

long instr = -93001;    /* default to NEAR MSI */
long instrPar = 0;
metCONTENT mC, *mCPtr;

char *readType = (char *) 0;                           /* -intype <inputType> */
TOIEPREADNEXT readnextFn = fixOrbdb_readnextOrig;
int verbose;                                                      /* -verbose */
int debug;                                                          /* -debug */

long success;
MTX m1, m2, m3;
MTX cam2Abf;
VEC sunFromAstAbf, scFromAstAbf, p5, campts[MXFOVVTX];
char timch[256] = { "<none>" };
char metch[256] = { "<none>" };
long fovcount;

long i, j, k, ii, iii;

double obsParm;

FILE *fInFrames;      /* input frames file stream (from orbit) */
char *fnInFrames;     /* input frames filename (from orbit) */
char *fnNull = { "" };

char *fnDB;           /* output ORBDB filename or filename prefix */
ORBDB_FILES *fDB;

long iopt, numPlates, ip;
long *pltBase, *pltPtr, *pltPtrEnd;
pltKEY *pK;

#define MAXMALLOC 100
void *savMalloc0[MAXMALLOC];
void **savMalloc = savMalloc0;

#define SAVMALLOC(A) (*(savMalloc++) = (void *) malloc(A))

#define RTN \
  if ( fInFrames && (fInFrames != stdin)) fclose( fInFrames); \
  if ( fDB)  orbdb_close( fDB); \
  while (savMalloc > savMalloc0) { \
    if ( *(--savMalloc)) free( *savMalloc); \
  } \
  return

#define ERRRTN RTN -1
#define OKRTN RTN 0

#define FPRINTFERRRTN(A) fprintf A; fflush(stderr); ERRRTN

  /* defaults */

  for ( savMalloc = savMalloc0+MAXMALLOC; savMalloc > savMalloc0; ) {
    *(--savMalloc) = (void *) NULL;
  }

  toIepGetReadnextFn( 0, (char **) 0);                    /* initialize toIep */

  verbose = 0;
  debug = 0;
  fInFrames = stdin;
  fnInFrames = (char *) NULL;
  fnDB = (char *) NULL;

  /*************************************************/
  /* process command line arguments */

  for ( argv++, argc--; argc > 0; ++argv, --argc) {
    if ( **argv != '-') {
      FPRINTFERRRTN(( stderr, "***fixOrbdb Unknown option:  %s; Returning ...\n"
                    , *argv));
    }
    TYPECHK( iopt, opts, (*argv)+1)
    switch ( iopt) {

    case OPT_HELP:
      fprintf( stderr, "USAGE:  %s [options]\nOPTIONS:\n", *savargv);
      frm2_outOptHelp( opts, stderr);
      return 0;

    case OPT_DEBUG:            /* ***N.B. -debug drops through & sets verbose */
      debug = 1;

    case OPT_VERBOSE:
      verbose = 1;
      break;

    case OPT_FRAMESIN:                        /* -framesin <framesInFilename> */
      if ( --argc < 1) break;
      argv++;
      fnInFrames = *argv;
      if ( *fnInFrames == '-')                      /* test for "-framesin -" */
        if ( !fnInFrames[1]) fnInFrames = (char *) 0;
      break;

    case OPT_INTYPE:                                   /* -intype <inputType> */
      if ( --argc < 1) break;
      argv++;
      readType = *argv;

      /***************************************************************
       * - use *argv to retrieve pointer to  *2iep_readnext() function
       * - call *2iep_init()
       * - remaining arguments are for the *2iep_init function
       */
      if ( !(readnextFn=toIepGetReadnextFn(argc,argv)) ) {
        if ( !strcmp( *argv, "list") || !strcmp( *argv, "listfull")) {
          fprintf( stderr, "Done listing intype's, fixOrbdb exiting ...\n");
          exit(0);
        }
        fprintf( stderr
               , "No *2iep_readnext function found to match '-intype %s'\n"
               , *argv);
        fprintf( stderr
               , "Try 'fixOrbdb -intype list[full]' to see valid options\n");
        fprintf( stderr, "fixOrbdb Exiting ...\n");
        exit(1);
      }
      argc = 1;            /* ensure no more arguments processed in this loop */
      break;

    case OPT_ORBDB:                                  /* -outiep <iepFilename> */
      if ( --argc < 1) break;
      argv++;
      fnDB = *argv;
      if ( *fnDB == '-')                              /* test for "-outiep -" */
        if ( !fnDB[1]) fnDB = (char *) 0;
      break;

    default:
      FPRINTFERRRTN(( stderr
                    , "***fixOrbdb Program error, contact programmer, %s%s\n"
                    , "code WSNBATGH-1-", *argv));
      break;

    } /* switch iopt */

    if ( argc < 1) {
      FPRINTFERRRTN(( stderr
        , "***fixOrbdb:  No following argument for %s; Returning...\n"
        , *argv));
    }

  } /* for argv++ ... */

  /* done interpreting command line argumments */
  /*********************************************/

  /* if DB filename pointer is non-null,
   *   if the string is non-null, open those files as the DB output files
   *   if the string is null, set fDB stream pointer to null
   */

  if ( fnDB) {
    if ( *fnDB) {
      fDB = orbdb_openForWrite( fnDB, (ORBDB_FILES *) NULL);
      if ( !fDB) {
        FPRINTFERRRTN(( stderr
          , "fixOrbdb:  Could not open ORBDB output files (%s); Returning...\n"
          , fnDB));
      }
      if ( (numPlates=orbdb_getNumPlates(fDB)) < 1) {
        FPRINTFERRRTN(( stderr
          , "fixOrbdb:  Could not get # of plates (%ld; %s); Returning...\n"
          , numPlates, fnDB));
      }
      if ( !(pltBase=(long *)SAVMALLOC(sizeof(long)*numPlates)) ) {
        FPRINTFERRRTN(( stderr
          , "fixOrbdb:  Error allocation plates; Returning...\n"
          ));
      }
    } else fDB = (ORBDB_FILES *) NULL;     /* name is NULL string (""), no DB */
  } else fDB = (ORBDB_FILES *) NULL;

  if ( !fDB) {
    FPRINTFERRRTN(( stderr
      , "fixOrbdb:  Could not open orbdb (%s); Returning...\n"
      , fnDB ? (*fnDB?fnDB:"<empty filename string>")
             : "<null filename pointer>" ));
  }

  /* do the same for input frames filename pointer, default to stdin */

  if ( fnInFrames) {
    if ( *fnInFrames) {
      fInFrames = fopen( fnInFrames, "r");
      if ( !fInFrames) {
        FPRINTFERRRTN(( stderr
          , "fixOrbdb:  Could not open framesin output file (%s); Returning\n"
          , fnInFrames));
      }
    } else fInFrames = (FILE *) NULL; /* name is NULL string (""), null input */
  } else fInFrames = stdin; /* default to stdin */

  /************************************************/
  /* loop through frames in .frames input,
   *   get sun, s/c & corner vectors for each frame
   */
  while ( !(success = readnextFn( fInFrames, timch
                    , scFromAstAbf
                    , sunFromAstAbf, p5, &fovcount, campts, metch, &obsParm
                    , cam2Abf, mC._miscVals))) {

    if ( 1 != sscanf( metch, "%lf", &mC._met) ) mC._met = -1.0;
    mC._instr = instr;
    mC._instrPar = instrPar;

    /* update database */

    if ( mC._met != -1.0) {
    int returnVal;
    static long icou;
      if ( !(mCPtr=orbdb_fetchMetContent( fDB, mC._met, mC._instr)) ) {
        continue;
      }

      /* transfer mCPtr->_visPlates array to pltBase */

      for ( pK=mCPtr->_visPlates, pltPtrEnd=(pltPtr=pltBase)+mCPtr->_nVisPlates
          ; pltPtr<pltPtrEnd
          ; ++pltPtr, ++pK) *pltPtr = orbdb_unloadPltKey( pK);

      /* copy mC parameters to *mCPtr */

      mCPtr->_instrPar = mC._instrPar;
      for ( i=0; i<5; ++i) mCPtr->_miscVals[i] = mC._miscVals[i];

      /* put modified mCPtr back to orbdb */

      if ( (returnVal=orbdb_addMet(fDB,mCPtr,pltBase)) ) {
        fprintf( stderr, "fixOrbdb:  orbdb_addMet returned %d\n", returnVal);
      }

      free( mCPtr);

      ++icou;
      if ( verbose  && (debug || ((icou % 100) == 0)) ) {
        fprintf( stderr, ".");
        if ( (icou % 5000) == 0) {
          fprintf( stderr, ".%ld:   MET=%lf\n", icou, mC._met);
        } else if ( debug ) {
          if ( (icou%100) == 0) fprintf( stderr, "%ld:", icou);
          fprintf( stderr, "%.0lf", mC._met);
        }
      }
    } /* if _met != -1.0 */

  } /* while success = readnextFn ... */

  if ( success != -1) { 
    fprintf( stderr, "%sProblem reading FRAMES input file:  status = %08xx\n"
           , "fixOrbdb:  "
           , success);
    fprintf( stderr, "  Last successful time read was\n\n  %s\n\n", timch);
    fprintf( stderr, "  Problem probably occurred %s FRAME with that time\n"
                   , (success&TIMBIT) ? "in" : "after");
    fprintf( stderr,   "Last line read was %ld line(s) after that time line\n"
                   , readNextLineno);
    fprintf( stderr, "  Check input file for %s\n"
           , (success&INCOMPLETE)  ? "missing lines - are campts in file?" 
           : ((success&NOT3)       ? "missing 1 or more of 3 vector elements"
           : ((success&NOT1)       ? "missing number of fov points"
           : ((success&CAMPTS)     ? "Problem interpreting fov vertices"
           : ((success&DOUBLELINE) ? "duplicate line types within one FRAME"
           : ((success&READERR)    ? "cause of I/O read error:"
           : "??? Contact programmer, Code WSNBATGH-1"
           ))))));
    if ( success&READERR) perror( "");
  } else {
    fprintf( stderr, "fixOrbdb:  Normal completion; Returning ...\n");
  }
  OKRTN;
} /* fixOrbdb_main(int argc, char **argv) { */

#define DUMYROUTINE( A, B) \
void *A () \
{ if ( *(B)) { fprintf(stderr,"Call to dummy routine:  %s\n",B); exit(1); } \
return 0; }

/* DUMYROUTINE( orbitgui_return_spudv_sc, "orbitgui_return_spudv_sc") */
/* DUMYROUTINE( orbitgui_return_boreroll, "orbitgui_return_boreroll") */

DUMYROUTINE( orbitgui_update_BoreRoll, "orbitgui_update_BoreRoll")

DUMYROUTINE( orbitgui_get1stCASFromCurItem, "")
DUMYROUTINE( orbit_CAS_ds40Vec, "")
DUMYROUTINE( orbit_CAS_ds56Vec, "")
DUMYROUTINE( orbit_CAS_TypeToName, "")
DUMYROUTINE( orbitgui_add_comment_sc, "")

int
main(int argc, char **argv) { return fixOrbdb_main(argc, argv); }

/**********************************************************/
