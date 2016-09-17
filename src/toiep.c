#include <stdio.h>
#include <string.h>

#ifdef vms
char *malloc();
char *realloc();
#else
#include <malloc.h>
#endif
#include "debug.h"

#include "toiep.h"  /* includes orbit3d.h */
#include "pointing.h"
#include "spudshap.h"

#define _ORBITFORT_H_TYPESONLY_
#include "orbitfort.h"

#include "orbit_spice_names.h"

#include "orbit_stats.h"   /* to get OPTION */

extern fortint failed();
extern void reset();

extern long readNextLineno;

/**********************************************************************/
enum { TOIEPFMT_KMET=0L
     , TOIEPFMT_DPMET
     , TOIEPFMT_SCLK
     , TOIEPFMT_DPET
     , TOIEPFMT_UTC
     , TOIEPFMT_COUNT
     };

/**********************************************************************/
typedef struct {
  SC *_sc;
  SPUDV *_spudv;    /* global variable */
  ORBIT _scOrbit, _asterOrbit, _earthOrbit;
  POINTING _bore, _roll;
  IMGFRM _currentImgfrm;
  int _timeColumnStart, _timeColumnEnd, _instrColumn, _mpColumn;
  int _mvColumns[5];
  int _inType;
  int _tryck;
  char *_fnFAIL;
  FILE *_fFAIL;
} ITEM;

static ITEM curItemStr;

static ITEM *cur_item;

#define sc cur_item->_sc
#define scOrbit cur_item->_scOrbit
#define asterOrbit cur_item->_asterOrbit
#define earthOrbit cur_item->_earthOrbit
#define bore cur_item->_bore
#define roll cur_item->_roll
#define currentImgfrm cur_item->_currentImgfrm
#define fFAIL cur_item->_fFAIL
#define fnFAIL cur_item->_fnFAIL

SPUDV *orbit_init(SC *);

/********************************************/
/* return bore and roll structures given SC structure */
void
orbitgui_return_boreroll( SC *inpSc, POINTING **inpBore, POINTING **inpRoll) {
ITEM *cur_item = (ITEM *) inpSc->_cur_item;
  *inpBore = &bore;
  *inpRoll = &roll;
  return;
}

/*****************************/
/* override cur_item->_spudv */
void
toIep_SetSpudv( SPUDV *inpSpudv) {

  if ( !inpSpudv) return;
  if ( cur_item->_spudv) {                             /* cleanup other model */
  SPUDF *dynamicSpudf = cur_item->_spudv->_spudf;
    spud_freeSpudv( cur_item->_spudv);
    spud_freeSpudf( dynamicSpudf, 1);
  }
  cur_item->_spudv = inpSpudv;
}

/*********************************************/
/* return spudv structure given SC structure */
void
orbitgui_return_spudv_sc( SC *inpSc, SPUDV **spudvPtr) {
  *spudvPtr = cur_item->_spudv;                      /* ***N.B. inpSc ignored */
  return;
}

/********************************************/
int
toIep_ScSpiceInit() {
int i;

  sc = (SC *) malloc( sizeof(SC));

  sc->_cur_item = (void *) cur_item;

  sc->_earthOrbit = &earthOrbit ;
  sc->_asterOrbit = &asterOrbit ;
  sc->_scOrbit = &scOrbit ;

  scOrbit._sc = (void *) sc;
  earthOrbit._sc = (void *) sc ;
  asterOrbit._sc = (void *) sc ;

  scOrbit._status = ORB_USESPK ;
  scOrbit._newutc[0] = '\0';

  scOrbit._agen._addGenFrames = ((void *)0) ;
  scOrbit._agen._MaxOrbits = 0.0;
  scOrbit._agen._MaxTime = 1400.0;
  scOrbit._agen._FrmOverlap = 0.01;
  scOrbit._agen._FrmOverlap2 = scOrbit._agen._FrmOverlap;
  scOrbit._agen._FrmOverlapTypeBits = (1L << OVERLAP_TYPE_ANN_BIT);
  scOrbit._framesoutBits[((FRAMESOUT_BIT_LAST + 4  - 1) / 4 ) ]  = 0;

  earthOrbit._status = ORB_USESPK;
  earthOrbit._newutc[0] = '\0';

  asterOrbit._status = ORB_USESPK ;
  asterOrbit._newutc[0] = '\0';

  cur_item->_spudv = orbit_init( sc );

  sc->_doFov100 = 0;
  sc->_tryck = cur_item->_tryck;                           /* sc->_tryck = 1; */
  orbit_set_instrument( sc, SC_MSI);

  scOrbit._agen._NISFOV = 1;
  scOrbit._agen._StartingStep = 72;
  scOrbit._agen._EndingStep = 79;
  scOrbit._agen._DeltaStep = 1;
  scOrbit._agen._TimePerStep = 10;
  scOrbit._agen._DelayPerStep = 2;
  scOrbit._agen._DelayPerRow = 5;
  currentImgfrm.imgfrmAlloced = 0;
  currentImgfrm._otherBodies = 0;

  currentImgfrm._ncampts = sc->_nVert[0];
  currentImgfrm._nClosedPts = sc->_nInstrClosedPtsPtr[0];
  for ( i=1; i< sc->_numInstr; ++i) {
    if ( currentImgfrm._ncampts < sc->_nVert[i]) {
      currentImgfrm._ncampts = sc->_nVert[i];
      currentImgfrm._nClosedPts = sc->_nInstrClosedPtsPtr[i];
    }
  }
  currentImgfrm.vec3 =
    (double *) malloc( currentImgfrm._ncampts*3*sizeof(double));

  sc->_sunFromAstAbf [0] = 1.0;
  sc->_sunFromAstAbf [1] = sc->_sunFromAstAbf [2] = 0.0;

  sc->_jjmode   = JJMODE_NORMAL;

  bore.aimpt.vec[0] = bore.aimpt.vec[1] = bore.aimpt.vec[2] = 0.0;
  bore.aimpt.type = Inadir;
  bore.scvec.vec[0] = bore.scvec.vec[1] = bore.scvec.vec[2] = 0.0;
  bore.scvec.type = Iinstr;

  roll.aimpt.vec[0] = roll.aimpt.vec[1] = roll.aimpt.vec[2] = 0.0;
  roll.aimpt.type = Ieci;
  roll.scvec.vec[0] = roll.scvec.vec[1] = roll.scvec.vec[2] = 0.0;
  roll.scvec.type = Ipanel;

  return 0;
}

/********************************************/
#define TOIEPLINBUFSIZ 2048
#define TOIEPFLDBUFSIZ 100
#define TOIEPNUMFLDS 30

/********************************************/
char *
toIep_getColumns( char *lin, int iColM, int iColN) {
char lcllin[TOIEPLINBUFSIZ+1];
char flds[TOIEPNUMFLDS+1][TOIEPFLDBUFSIZ+1];
char *cPtr;
int iFld, nCols, iCol;

  if ( iColN < iColM) { iCol = iColN; iColN = iColM; iColM = iCol; }
  if ( iColM < 0) return (char *) 0;
  if ( iColN >= TOIEPNUMFLDS) return (char *) 0;

  nCols = sscanf( lin
                , "%100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s %100s"
                , flds[0], flds[1], flds[2], flds[3], flds[4]
                , flds[5], flds[6], flds[7], flds[8], flds[9]
                , flds[10], flds[11], flds[12], flds[13], flds[14]
                , flds[15], flds[16], flds[17], flds[18], flds[19]
                , flds[20], flds[21], flds[22], flds[23], flds[24]
                , flds[25], flds[26], flds[27], flds[28], flds[29]);

  if ( nCols <= iColN) return (char *) 0;

  for ( (cPtr=lcllin), (iCol=iColM); iCol<=iColN; ++iCol) {
    strcpy( cPtr, flds[iCol]);
    cPtr += strlen( cPtr);
    *(cPtr++) = ' ';
  }

  if ( cPtr > lcllin) cPtr[-1] = '\0';
  if ( !*lcllin) return (char *) 0;

  return strdup(lcllin);

} /* toIep_getColumns( FILE *, int, int ) */

/**********************************************************************/
int
toIep_getEtSclkdp( char *lin, double *sclkdp) {
char *getChars;
int rtn;
int iColM, iColN;

  iColM = cur_item->_timeColumnStart;
  iColN = cur_item->_timeColumnEnd;

  if ( !(getChars=toIep_getColumns( lin, iColM, iColN)) ) return -1;

  reset();

  rtn = 0;
  switch ( cur_item->_inType) {
  case TOIEPFMT_KMET:
  case TOIEPFMT_DPMET:
    if ( sscanf( getChars, "%lf", sclkdp) != 1) {
      rtn = -1;
      break;
    }
    if ( *sclkdp < 1.0 ) { rtn = -1; break; }
    if ( cur_item->_inType == TOIEPFMT_KMET) *sclkdp *= 1000;
    break;
  case TOIEPFMT_SCLK:
    orbit_scencd( sc->_scid, getChars, sclkdp);
    break;
  case TOIEPFMT_DPET:
    if ( sscanf( getChars, "%lf", &sc->_et) != 1) { rtn = -1; break; }
    break;
  case TOIEPFMT_UTC:
    orbit_utc2et( getChars, &sc->_et);
    break;
  default:  /* just in case */
    fprintf( stderr, "toIep_getEtSclkdp:  Error, contact Programmer, code %s\n"
           , "WSNBATGH-0");
    fflush( stderr);
    rtn = -1;
    break;
  } /* switch cur_item->_inType */

  free( getChars);
  if ( rtn || failed() ) return rtn ? rtn : -1;

  switch ( cur_item->_inType) {
  case TOIEPFMT_KMET:
  case TOIEPFMT_DPMET:
  case TOIEPFMT_SCLK:
    sct2e( &sc->_scid, sclkdp, &sc->_et);
    break;
  case TOIEPFMT_DPET:
  case TOIEPFMT_UTC:
    sce2t( &sc->_scid, &sc->_et, sclkdp);
    break;
  } /* switch ( cur_item->_inType) { */

  if ( failed() ) return -1;
}  /* toIep_getEtSclkdp( char *lin, double *sclkdp) { */

/**********************************************************************/
/* anything */
double
toIep_getAnything( char *lin, int iCol) {
char *getChars;
double rtn = -999.0;

  if ( iCol < 0) return rtn;                    /* ignore for nonsense column */
  if ( !(getChars=toIep_getColumns( lin, iCol, iCol)) ) return rtn;

  sscanf( getChars, "%lf", &rtn);
  free( getChars);
  return rtn;
} /* toIep_getAnything( char *lin, int iCol) { */

/**********************************************************************/
/* instrument */
int
toIep_getInstr( char *lin) {
int iCol = cur_item->_instrColumn;
char *getChars;
int rtn;
fortint instr;

  if ( iCol < 0) return 0;                      /* ignore for nonsense column */
  if ( !(getChars=toIep_getColumns( lin, iCol, iCol)) ) return -1;

  rtn = 0;
  if ( !strcmp( getChars, "nis")) instr = SC_NIS;
  else if ( !strcmp( getChars, "niswide")) instr = SC_NIS2;
  else if ( !strcmp( getChars, "nis2")) instr = SC_NIS2;
  else if ( !strcmp( getChars, "msi")) instr = SC_MSI;
  else rtn = -1;

  free( getChars);

  if ( !rtn ) getChars = orbit_set_instrument( sc, instr);

  return rtn;
} /* toIep_getInstr( char *lin) { */

/**********************************************************************/
/* mirror position */
int
toIep_getMp( char *lin) {
int iCol = cur_item->_mpColumn;
char *getChars;
int rtn;
long mp;

  if ( iCol < 0) return 0;                      /* ignore for nonsense column */
  if ( !(getChars=toIep_getColumns( lin, iCol, iCol)) ) return -1;

  rtn = 0;
  if ( 1 != sscanf( getChars, "%ld", &mp)) rtn = -1;

  free( getChars);

  if ( !rtn ) if ( mp < 0 || mp > 350 ) rtn = -1;

  if ( !rtn ) sc->_nisStepAct = mp;

  return rtn;
} /* toIep_getMp( char *lin) { */

/**********************************************************************/
long
time2iep_readnext( FILE *f, char *timch, VEC vsc, VEC vsun, VEC p5
                 , long *fovcount, VEC campts[], char *metch
                 , double *obsParm, MTX cam2Abf, double *miscVals) {
char lin[1+TOIEPLINBUFSIZ-TOIEPNUMFLDS];
double sclkdp;

char doy[UTCLEN], cal[UTCLEN], jd[UTCLEN], sclkch[UTCLEN];
fortint doylenout, callenout, jdlenout, sclkchlenout;
int utclenm1 = (UTCLEN - 1);
long istate;

# define FAILFPRINTF if ( *lin ) { \
                       if (!fFAIL && fnFAIL) fFAIL = fopen(fnFAIL,"a"); \
                       if ( fFAIL) fprintf( fFAIL, "%s\n", lin); \
                     }
# define FAILCLOSE if (fFAIL) fclose( fFAIL); fFAIL = (FILE *) 0

# define RETURN(I,A) \
  { if (I) { fprintf A; FAILFPRINTF FAILCLOSE; } *lin = '\0'; return(I); }
# define CONTINUE(A) { fprintf A; FAILFPRINTF *lin = '\0'; continue; }

  *lin = '\0';
  if ( !f) RETURN(-1, (stdout, "input file not open\n"))

  while ( fgets( lin, TOIEPLINBUFSIZ-TOIEPNUMFLDS, f) ) {
  char *cPtr;
  long i;
  double *lclvec;
  int ps;

    if ( (cPtr=strchr(lin,'\n'))) *cPtr = '\0';

    if ( toIep_getEtSclkdp( lin, &sclkdp))
      CONTINUE((stdout,"getEtSclkDp failed\n"))
    if ( toIep_getInstr( lin)) 
      CONTINUE((stdout,"getInstr failed sclk=%lf\n", sclkdp))
    if ( toIep_getMp( lin))
      CONTINUE((stdout,"getMp failed sclk=%lf\n", sclkdp))
    miscVals[0] = miscVals[1] = miscVals[2] = miscVals[3] = miscVals[4] =-999.0;
    for ( i=0; i<5 && cur_item->_mvColumns[i]>0; ++i) {
      miscVals[i] = toIep_getAnything( lin, cur_item->_mvColumns[i]);
    }
    *timch = '\0';

    ps = pointing_solve( timch, &sc->_et, sc, &bore, &roll);
    if ( !ps) 
      CONTINUE((stdout,"pointing_solve failed sclk=%lf\n", sclkdp))
    if ( ps == -1 && cur_item->_tryck)              /* didn't find a c-matrix */
      CONTINUE((stdout,"pointing_solve failed (ck) sclk=%lf\n", sclkdp))

    if ( !loadImageFrame( sc, sc->_et, &currentImgfrm)) 
      CONTINUE((stdout,"loadImageFrame failed sclk=%lf\n", sclkdp))
    copyInstrument_ScToImgfrm( sc, &currentImgfrm);

    *obsParm = BADVAL;
    *timch = *metch = '\0';
    istate = 0;

    /* convert et to ascii times, save int timch */

    sprintf( timch, " %12.1lf -999.0000 ", sc->_et);
    cPtr = timch + strlen(timch);
    ospice_et2jd( &sc->_et, &utclenm1, &jdlenout, cPtr);
    cPtr += jdlenout;
    *(cPtr++) = ' ';
    ospice_et2doy( &sc->_et, &utclenm1, &doylenout, cPtr);
    cPtr += doylenout;
    *cPtr = '\0';
    SETBIT(TIMBIT);

    /* p5 point, spacecraft & sun vectors (all in ABF) */

    CPYVEC( currentImgfrm._boreVec, p5); SETBIT(P5BIT);
    VADD2( p5, currentImgfrm._boretoscVec, vsc); SETBIT(SCBIT);
    VADD2( p5, currentImgfrm._boretosunVec, vsun); SETBIT(SCBIT);

    /* cam2Abf matrix, SPICE convention
     * ***N.B. sc->_mcamtoabf matrix is backwards from SPICE
     */
    MT2( sc->_mcamtoabf, cam2Abf);

    /* FOV vertex count */

    *fovcount = currentImgfrm._ncampts; SETBIT(CPCOUNTBIT);

    SETBIT(CPSTARTBIT);

    if ( *fovcount < 3) 
      RETURN( CAMPTS|istate, (stdout, "fovcount(a) < 3; sclkdp=%lf\n", sclkdp))

    /* FOV vertices, ABF (campts) */

    for ( (i=0),(lclvec=currentImgfrm.vec3); i<*fovcount; ++i, lclvec+=3) {

      if ( i) {            /* test to exit loop if first campt == later campt */
      VEC zeroTest;
        VMINUS2( currentImgfrm.vec3, lclvec, zeroTest);
        if ( VDOT(zeroTest,zeroTest) < 1e-10) { /* current campt == first one */
          *fovcount = i;                                  /* set vertex count */
          break;                                             /* exit for loop */
        }
      }
      CPYVEC( lclvec, campts[i]);
    } /* for i=0, lclvec=.vec3 */

    if ( *fovcount < 3 )
      RETURN( CAMPTS|istate, (stdout, "fovcount(b) < 3; sclkdp=%lf\n", sclkdp))
    SETBIT(CPSTOPBIT);

    sprintf( metch, "%13lf", currentImgfrm._sclk);

    *lin = '\0';
    return 0;

  }
  if ( !feof(f)) 
    RETURN( READERR|istate, (stdout, "read error\n"))

  *lin = '\0';

  RETURN( -1, (stdout, "") );   /* end of input file */

} /* time2iep_readnext( FILE *f, char *timch, VEC vsc, VEC vsun, VEC p5
                      , long *fovcount, VEC campts[], char *metch
                      , double *obsParm) { */

#define FPRINTFERRRTN(A) fprintf A ; return 0
#define GETINTOPT(I) \
  if ( 1 != sscanf( *argv, "%ld", &lcllong)) { \
    FPRINTFERRRTN(( stderr \
                  , "***TOIEP Can't read integer: %s  %s; Returning ...\n" \
                    , argv[-1], *argv)); \
  } \
  I = lcllong

/*************************************************/
TOIEPREADNEXT
time2iep_init( int argc, char **argv, int inType) {
enum {
  TOIEPTIMEINITOPT_TCOL=0
, TOIEPTIMEINITOPT_ICOL
, TOIEPTIMEINITOPT_MPCOL
, TOIEPTIMEINITOPT_INSTR
, TOIEPTIMEINITOPT_MP
, TOIEPTIMEINITOPT_SPICESPEC
, TOIEPTIMEINITOPT_MISCVALS
, TOIEPTIMEINITOPT_NOCK
, TOIEPTIMEINITOPT_FFAIL
, TOIEPTIMEINITOPT_COUNT
};
static OPTION opts[] = {
  "timecol", TOIEPTIMEINITOPT_TCOL, "-timecol <col # of time>"
, "instrcol", TOIEPTIMEINITOPT_ICOL, "-instrcol <col # of instrument>"
, "mpcol", TOIEPTIMEINITOPT_MPCOL, "-mpcol <col # of mirror position>"
, "instr", TOIEPTIMEINITOPT_INSTR, "-instr msi|nis"
, "mp", TOIEPTIMEINITOPT_MP, "-mp <mirror position>"
, "spicespec", TOIEPTIMEINITOPT_SPICESPEC, "-spicespec <spicespec filepath>"
, "miscvals", TOIEPTIMEINITOPT_MISCVALS, "-miscvals col1[:col2...]"
, "nock", TOIEPTIMEINITOPT_NOCK, "-nock"
, "failfile", TOIEPTIMEINITOPT_FFAIL, "-failfile <failed times output filepath>"
, (char *) 0, TOIEPTIMEINITOPT_COUNT, ""
};
static OPTION instrOpts[] = {
  "msi", SC_MSI, "MSI instrument"
, "nis", SC_NIS, "NIS instrument"
, "niswide", SC_NIS2, "NISWIDE instrument"
, "nis2", SC_NIS2, "NISWIDE instrument"
, (char *) 0, 0, ""
};
long lcllong, lcllong2, ssCount;
char lclStr[20];
long scMPOpt = -999;
long scInstrOpt = -999;
int i;

  cur_item->_inType = inType;
  cur_item->_tryck = 1;
  cur_item->_timeColumnStart = cur_item->_timeColumnEnd = 0;
  cur_item->_instrColumn = cur_item->_mpColumn = -1;
  fFAIL = (FILE *) 0;
  fnFAIL = (char *) 0;
  readNextLineno = 0;

  for ( argv++, argc--; argc > 0; ++argv, --argc) {
  long iopt;
    if ( **argv != '-') {
      FPRINTFERRRTN(( stderr, "***TOIEP Unknown option:  %s; Returning ...\n"
                    , *argv));
    }
    TYPECHKRTN( iopt, opts, (*argv)+1, 0)
    switch ( iopt) {
    case TOIEPTIMEINITOPT_TCOL:                             /* -timecol M[:N] */
      if ( --argc < 1 ) break;
      argv++;
      *lclStr = '\0';
      ssCount = sscanf( *argv, "%ld%1s%ld", &lcllong, lclStr, &lcllong2);
      if ( ssCount == 3 && inType == TOIEPFMT_UTC && *lclStr==':') {
        cur_item->_timeColumnStart = (lcllong<lcllong2) ? lcllong : lcllong2;
        cur_item->_timeColumnEnd = (lcllong>lcllong2) ? lcllong : lcllong2;
      } else if ( ssCount == 1) {
        cur_item->_timeColumnStart = cur_item->_timeColumnEnd = lcllong;
      } else {
        FPRINTFERRRTN(( stderr
                      , "***TOIEP Error reading args: %s  %s; Returning ...\n"
                      , argv[-1], *argv));
      }
      cur_item->_timeColumnStart--; cur_item->_timeColumnEnd--;
      break;

    case TOIEPTIMEINITOPT_ICOL:                 /* -instrcol instrumentColumn */
      if ( --argc < 1 ) break;
      argv++;
      GETINTOPT( cur_item->_instrColumn);
      cur_item->_instrColumn--;
      break;

    case TOIEPTIMEINITOPT_MPCOL:               /* -mpcol mirrorPositionColumn */
      if ( --argc < 1 ) break;
      argv++;
      GETINTOPT( cur_item->_mpColumn);
      cur_item->_mpColumn--;
      break;

    case TOIEPTIMEINITOPT_INSTR:                    /* -instr msi|nis|niswide */
      if ( --argc < 1 ) break;
      argv++;
      TYPECHKRTN( scInstrOpt, instrOpts, *argv, 0);
      break;

    case TOIEPTIMEINITOPT_MP:                           /* -mp mirrorPosition */
      if ( --argc < 1 ) break;
      argv++;
      GETINTOPT( scMPOpt);
      break;

    case TOIEPTIMEINITOPT_MISCVALS:
                                /* -miscvals col1[:col2[:col3[:col4[:col5]]]] */
      if ( --argc < 1 ) break;
      argv++;
      cur_item->_mvColumns[0] = 
      cur_item->_mvColumns[1] = 
      cur_item->_mvColumns[2] = 
      cur_item->_mvColumns[3] = 
      cur_item->_mvColumns[4] = -1;
      if ( 0 == (i=sscanf( *argv, "%d:%d:%d:%d:%d"
                                , cur_item->_mvColumns
                                , cur_item->_mvColumns+1
                                , cur_item->_mvColumns+2
                                , cur_item->_mvColumns+3
                                , cur_item->_mvColumns+4)) ) {
        FPRINTFERRRTN(( stderr
                      , "***TOIEP Can't read option: %s  %s; Returning ...\n"
                    , argv[-1], *argv));
      }
      while ( i--) {
        if ( cur_item->_mvColumns[i] < 1 || 
             cur_item->_mvColumns[i] > TOIEPNUMFLDS) {
          FPRINTFERRRTN(( stderr
                        , "***TOIEP Bad option (%d%s%d): %s %s; Returning ...\n"
                        , cur_item->_mvColumns[i]
                        , (cur_item->_mvColumns[i] < 1) ? "<" : ">"
                        , (cur_item->_mvColumns[i] < 1) ? 1 : TOIEPNUMFLDS
                        , argv[-1], *argv));
        }
        cur_item->_mvColumns[i]--;
      }
      break;

    case TOIEPTIMEINITOPT_NOCK:                                      /* -nock */
      cur_item->_tryck = 0;
      break;

    case TOIEPTIMEINITOPT_FFAIL:                      /* -failfile <failfile> */
      if ( --argc < 1 ) break;
      argv++;
      if ( **argv) fnFAIL = *argv;
      break;

    case TOIEPTIMEINITOPT_SPICESPEC:          /* -spicespec spicespecFilepath */
      if ( --argc < 1 ) break;
      argv++;
      {
      char eev[1024];
      char *newval;
        if ( strlen( *argv) > 1000) {
          FPRINTFERRRTN(( stderr
                        , "***TOIEP SPICESPEC filename too long:  %s; %s ...\n"
                        , *argv
                        , "Returning"));
        }
        sprintf( eev, "SPICESPEC=%s", *argv);
        newval = strdup( eev);
        putenv( newval);
      }
      break;

    default:
      FPRINTFERRRTN(( stderr
                    , "***TOIEP Program error, contact programmer, %s%s\n"
                    , "code WSNBATGH-1-", *argv));
      break;
    } /* switch iopt */

    if ( argc < 1 ) {
      FPRINTFERRRTN(( stderr
        , "***FRM2IEP:  No following argument for %s; Returning...\n"
        , *argv));
    }

  } /* for argc/argv */

  toIep_ScSpiceInit();

  if ( scInstrOpt != -999) sc->_nisStepAct = scInstrOpt;
  if ( scMPOpt != -999) sc->_nisStepAct = scMPOpt;

  return time2iep_readnext;
}

#define TOIEPTIMEFUNCS( READNEXTFN, INITFN, INTYPE, TAG, EXPINFO) \
TOIEPREADNEXTDECLARE( READNEXTFN) { return -1; } \
TOIEPINITDECLARE( INITFN, TAG, EXPINFO); \
TOIEPLISTCHECK; \
  return time2iep_init( argc, argv, INTYPE); \
}


TOIEPTIMEFUNCS( kmet2iep_readnext, kmet2iep_init, TOIEPFMT_KMET, "kMET"
,  ":  list of kMETs e.g. 132098567.3240000\n\
       Options: [-timecol <column>]\n\
                [-instrcol <column>] [-mpcol <column>]\n\
                [-instr msi|nis|niswide] [-mp <mirrorPosition>]\n\
                [-miscvals <col1>[:<col2>[:<col3>[:<col4>[:<col5>]]]]]\n\
                [-nock]\n\
                [-spicespec <spicespecfile>]")

TOIEPTIMEFUNCS( met2iep_readnext, met2iep_init, TOIEPFMT_DPMET, "MET"
, ":   list of SCLKdp e.g. 132098567324.0000\n\
       Options: [-timecol <column>]\n\
                [-instrcol <column>] [-mpcol <column>]\n\
                [-instr msi|nis|niswide] [-mp <mirrorPosition>]\n\
                [-miscvals <col1>[:<col2>[:<col3>[:<col4>[:<col5>]]]]]\n\
                [-nock]\n\
                [-spicespec <spicespecfile>]")

TOIEPTIMEFUNCS( sclk2iep_readnext, sclk2iep_init, TOIEPFMT_SCLK, "SCLK"
,  ":  list of SCLKch e.g. 1/132098567324\n\
       Options: [-timecol <column>]\n\
                [-instrcol <column>] [-mpcol <column>]\n\
                [-instr msi|nis|niswide] [-mp <mirrorPosition>]\n\
                [-miscvals <col1>[:<col2>[:<col3>[:<col4>[:<col5>]]]]]\n\
                [-nock]\n\
                [-spicespec <spicespecfile>]")

TOIEPTIMEFUNCS( et2iep_readnext, et2iep_init, TOIEPFMT_DPET, "ET"
,":    list of ephemeris time (s past J2k) e.g. 3.14159e7\n\
       Options: [-timecol <column>]\n\
                [-instrcol <column>] [-mpcol <column>]\n\
                [-instr msi|nis|niswide] [-mp <mirrorPosition>]\n\
                [-miscvals <col1>[:<col2>[:<col3>[:<col4>[:<col5>]]]]]\n\
                [-nock]\n\
                [-spicespec <spicespecfile>]")

TOIEPTIMEFUNCS( utc2iep_readnext, utc2iep_init, TOIEPFMT_UTC, "UTC"
, ":   list of UTC e.g. 2000-043T12:00:00.000\n\
       Options: [-timecol <columnStart>[:<columnEnd>]]\n\
                [-instrcol <column>] [-mpcol <column>]\n\
                [-instr msi|nis|niswide] [-mp <mirrorPosition>]\n\
                [-miscvals <col1>[:<col2>[:<col3>[:<col4>[:<col5>]]]]]\n\
                [-nock]\n\
                [-spicespec <spicespecfile>]")


/***********************************************************************
 * interpret argv[0], convert to *2iep_{init,readnext} functions,
 * - call *2iep_init function
 * - return *2iep_readnext function
 * - return null if argv[0] null or empty
 *
 * - ***N.B. call with argv==0 to init cur_item
 */
TOIEPREADNEXT
toIepGetReadnextFn( int argc, char **argv) {
TOIEPINIT toIepInitArr[] = {
  kmet2iep_init
, met2iep_init
, sclk2iep_init
, et2iep_init
, utc2iep_init
, (TOIEPINIT) 0
};
TOIEPINIT *ptr;
TOIEPREADNEXT rtn;

  if ( !argv) {                /* initialize cur_item if argv is null pointer */
    if ( cur_item != &curItemStr) {
      cur_item = &curItemStr;
      cur_item->_spudv = (SPUDV *) 0;
      cur_item->_sc = (SC *) 0;
    }
    return 0;
  }

  if ( !*argv) return 0;             /* ... or pointer to null string pointer */
  if ( !**argv) return 0;        /* ... or pointer to pointer to empty string */

  for ( ptr = toIepInitArr; *ptr; ++ptr) {
    if ( (rtn=(*ptr)( argc, argv)) ) return rtn;             /* gotta love it */
  }
  return 0;                        /* return null if no matching string found */
}

#if 0
# define SETREADNEXTFUNC( STRING, INITFN, READNEXTFN) \
  if ( !strcmp( readType, STRING)) { \
  extern void INITFN( int, char **); \
  extern long READNEXTFN( FILE *f, char *timch, VEC vsc, VEC vsun, VEC p5 \
                        , long *fovcount, VEC campts[], char *metch \
                        , double *obsParm); \
    INITFN( argc, argv); \
    return READNEXTFN; \
  }

  SETREADNEXTFUNC( "kmet", kmet2iep_init, kmet2iep_readnext)
  SETREADNEXTFUNC( "met", met2iep_init, met2iep_readnext)
  SETREADNEXTFUNC( "utc", utc2iep_init, utc2iep_readnext)
  SETREADNEXTFUNC( "sclkch", sclkch2iep_init, sclkch2iep_readnext)

  return 0;                        /* return null if no matching string found */

} /* toIepGetReadnextFn( char *readType) { */
#endif /* #if 0 */
