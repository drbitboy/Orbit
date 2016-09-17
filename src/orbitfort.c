/* orbitfort.c - C-to-FORTRAN interface routines */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/utsname.h>

#define rad *(180.0/M_PI)

#ifdef vms
char *malloc();
char *realloc();
#else
#include <malloc.h>
#endif

#include "spudshap.h"
#include "orbit_util.h"
#include "orbitfort.h"
#include "pointing.h"
#include "orbit_spice_names.h"

                       /*   load=1   , handle,    fn length, filename */
extern void ospice_spkluef( fortint *, fortint *, fortint *, char *);
extern void ospice_cklupf(  fortint *, fortint *, fortint *, char *);
extern void ospice_pckluof( fortint *, fortint *, fortint *, char *);

#include "debug.h"

#define LEN255 255L
static char utcInit[LEN255];
static SPICEOPTS *spiceOpts;

/***********************************************************************
 * look for a (SPICEOPT *), starting with (*startSpiceOpt), in a linked 
 * list with a given type
 * - return (SPICEOPTS **) P where (*P)->type == type
 * - return (SPICEOPTS **) 0 if no matches found
 * - if skipCurrent == 0, then look at (*startSpiceOpt) first
 * - if skipCurrent != 0, then look at (*startSpiceOpt)->next first
 */
SPICEOPTS **
orbit_matchSpiceOpt( SPICEOPTS **startSpiceOpt, int type, int skipCurrent)
{
SPICEOPTS **lclppso = (SPICEOPTS **) 0;

  if ( !startSpiceOpt) return (SPICEOPTS **) 0;
  if ( !*startSpiceOpt) return (SPICEOPTS **) 0;

  lclppso = startSpiceOpt;

  if (skipCurrent) lclppso = &(*lclppso)->next;

  while ( *lclppso)
  {
    if ( (*lclppso)->_type == type) return lclppso;
    lclppso = &(*lclppso)->next;
  }
  return (SPICEOPTS **) 0;
}

/***********************************************************************/
/* get the angle in degress between two unit vectors, but protect against
 * roundoffs where the dot product of the vectors goes beyond the valid
 * range of acos()
 */
#define ACOSDDOTU( ANG, V1,V2) \
  ANG = VDOT( V1, V2); \
  if ( ANG >= 1.0) ANG=0.0; \
  else if (ANG <= -1.0) ANG = 180.0; \
       else ANG = r2d( acos( ANG))

/*********************************/
/* and also for non-unit vectors */

#define ACOSDDOT( ANG, V1,V2) \
  if ( (ANG=(VDOT(V1,V1)*VDOT(V2,V2))) > 0.0) ANG = VDOT(V1,V2)/sqrt(ANG); \
  else ANG=2.0;                  /* force ANG=0 degrees if V0 or V2 is null */ \
  if ( ANG >= 1.0) ANG=0.0; \
  else if (ANG <= -1.0) ANG = 180.0; \
       else ANG = r2d( acos( ANG))

static VEC nullVec = { 0.0, 0.0, 0.0 };

double dpr_fac;
#define d2r(x) ((x) / dpr_fac)
#define r2d(x) ((x) * dpr_fac)
#define ur2d(x) ((x) * dpr_fac * 1e-6)

static int failedonce = 0;  /* for sct2e call */
fortint failed();

#if 0
int
main(int argc, char **argv) {
  orbit_argv0 = *argv;
  the_real_c_startup_routine(argc, argv);
}
int c_orbit () {
  the_real_c_startup_routine();
}
#endif

#define scid sc->_scid
#define asterid sc->_asterid
#define sunid sc->_sunid
#define earthid sc->_earthid
#define asterad sc->_asterad
#define maxrad sc->_maxrad
#define ncampts sc->_ncampts

#define numInstr sc->_numInstr
#define numInstrAlloc sc->_numInstrAlloc
#define instrVertPtrs sc->_instrVertPtrs
#define instrAbfPtrs sc->_instrAbfPtrs
#define fov100Pts sc->_fov100Pts
#define fov100InstrPtsPtrs sc->_fov100InstrPtsPtrs
#define nClosedPts sc->_nClosedPts
#define nMsiClosedPts sc->_nMsiClosedPts
#define nNisClosedPts sc->_nNisClosedPts
#define nInstrClosedPtsPtr sc->_nInstrClosedPtsPtr
#define nFov100Pts sc->_nFov100Pts
#define nFov100InstrPtsPtr sc->_nFov100InstrPtsPtr

#define boresight sc->_boresight
#define panel sc->_panel
#define updir sc->_updir
#define sampdir sc->_sampdir
#define camvert sc->_camvert
#define camabf sc->_camabf
#define fovy sc->_fovy
#define fovx sc->_fovx
#define instrument sc->_instrument
#define instrName sc->_instrName
#define scB0 sc->_scB0

#define msifovx sc->_msifovx
#define msifovy sc->_msifovy
#define msiB0 sc->_msiB0
#define msiSampDir sc->_msiSampDir
#define msiUpDir sc->_msiUpDir
#define nmsipts sc->_nmsipts
#define msivert sc->_msivert
#define msiabf sc->_msiabf

#define nisNomB0 sc->_nisNomB0
#define nisB0 sc->_nisB0
#define nisNomUpDir sc->_nisNomUpDir
#define nisUpDir sc->_nisUpDir
#define nisNomSampDir sc->_nisNomSampDir
#define nisSampDir sc->_nisSampDir
#define nisAxis sc->_nisAxis
#define nisfovy sc->_nisfovy
#define nisfovx sc->_nisfovx
#define nisStepAngle sc->_nisStepAngle
#define nisvert sc->_nisvert
#define nisabf sc->_nisabf
#define nnispts sc->_nnispts
#define nisStepNom sc->_nisStepNom
#define nisStepMax sc->_nisStepMax
#define nisStepAct sc->_nisStepAct

#define msctocam sc->_msctocam
#define mcamtosc sc->_mcamtosc
#define msctoabf sc->_msctoabf
#define mabftoj2k sc->_mabftoj2k
#define scOrbit sc->_scOrbit
#define asterOrbit sc->_asterOrbit
#define earthOrbit sc->_earthOrbit
#define jjmode sc->_jjmode
#define scSo scOrbit->_so
#define scEph scOrbit->_status
#define scSotoj2k scOrbit->_toj2k
#define asterSo asterOrbit->_so
#define asterEph asterOrbit->_status
#define asterSotoj2k asterOrbit->_toj2k
#define earthSo earthOrbit->_so
#define earthEph earthOrbit->_status
#define earthSotoj2k earthOrbit->_toj2k

/* setup the SC struct for a given instrument */

static double **camvertptr, **camabfptr; /* used later in orbit_init */

char *
orbit_set_instrument( SC *sc, fortint instr) {
static char nisname[] = "NIS";
static char niswidename[] = "NISWIDE";
static char msiname[] = "MSI";
char *rtname;
int nisfac;

  if ( instr>=0 && instr<numInstr) {
    nFov100Pts = nFov100InstrPtsPtr+instr;
    fov100Pts = fov100InstrPtsPtrs[instr];
    nClosedPts = nInstrClosedPtsPtr+instr;
  }

  switch( instr) {
  case SC_NIS:
  case SC_NIS2:
    nisfac = (instr==SC_NIS2) ? 2 : 1;
    fovx = nisfovx * nisfac; fovy = nisfovy;
    boresight = nisB0; sampdir = nisSampDir; updir = nisUpDir;
    ncampts = nnispts; camvert = nisvert; camabf = nisabf;
    nClosedPts = &nNisClosedPts;

    /* use scan mirror position & axis to rotate nominal vectors */

    if ( nisvert) {
    double *nomVec, *destVec;
    int i;
    MTX m;
      nomVec = nisvert + (3 * nnispts * nisfac);
      destVec = nisvert;
      rotation3D( nisAxis, (nisStepAct-nisStepNom)*nisStepAngle, m);
      for ( i=0; i<nnispts; i++, nomVec+=3, destVec+=3) {
        vxm( nomVec, m, destVec);
      }
      vxm( nisNomB0, m, boresight);
      vxm( nisNomSampDir, m, sampdir);
      vxm( nisNomUpDir, m, updir);

      /* scan mirror rotation of nominal fov points */

      if ( (destVec=fov100Pts) ) {
        nomVec = destVec + (3*MAXFOV100PTS);
        for ( i=0; i<*nFov100Pts; ++i, nomVec+=3, destVec+=3) {
          vxm( nomVec, m, destVec);
        }
      }

    } else {
      CPYVEC( nisNomB0, boresight);
      CPYVEC( nisNomSampDir, sampdir);
      CPYVEC( nisNomUpDir, updir);
    }

    camvertptr = &nisvert; camabfptr = &nisabf;
    rtname = (instr==SC_NIS) ? nisname : niswidename;
    break;

  case SC_MSI:
    fovx = msifovx; fovy = msifovy;
    boresight = msiB0; sampdir = msiSampDir; updir = msiUpDir;
    ncampts = nmsipts; camvert = msivert; camabf = msiabf;
    camvertptr = &msivert; camabfptr = &msiabf;
    nClosedPts = &nMsiClosedPts;
    rtname = msiname;
    break;

  default:  /* We Should Not Be Able To Get Here */
    fprintf( stderr
           , "Program error, contact programmer:  code WSNBATGH-%s-%ld\n"
           , "orbit_set_instrument", instr);
    exit(-1);
  }

  CPYVEC( sampdir, msctocam);
  CPYVEC( boresight, msctocam+3);
  CPYVEC( updir, msctocam+6);
  MT2( msctocam, mcamtosc);

  instrument = instr;
  strcpy( sc->_instrName, rtname);
  return( rtname);
} /* orbit_set_instrument */

/*********************************************/
/* optionally load spice kernels based on et */
void
orbit_loadSpiceOpts( double et, int type) {
SPICEOPTS *lclso;
int newoneloaded = 0;  /* keep track of any kernels that have been loaded */
fortint lenfn;
fortint doload = 1;
fortint dounload = 0;

void (*lclLoadKernel)() = (type == SPICEOPTS_SPKO) ? ospice_spkluef :
                          (type == SPICEOPTS_CKO) ? ospice_cklupf :
                          (type == SPICEOPTS_ORIBPCKO) ? ospice_pckluof : 0;

  if ( !lclLoadKernel) {
    fprintf( stderr, "***PROGRAM ERROR:  Contact programmer, %s\n"
                   , "Code WSNBATGH-orbit_loadSpiceOpts-1");
    return;
  }

  /* unload all loaded kernels of this type for which et is out of range */

  for ( lclso = spiceOpts; lclso; lclso = lclso->next) {
    if ( type == lclso->_type && lclso->_handle != SPICEOUT 
      && (et < lclso->_ets[0] || et > lclso->_ets[1]) ) {
      lenfn = strlen( lclso->_absPathName);
      lclLoadKernel( &dounload, &lclso->_handle, &lenfn, lclso->_absPathName);
      lclso->_handle = SPICEOUT;
    }
  }

  newoneloaded = 0;

  for ( lclso = spiceOpts; lclso; lclso = lclso->next) {

    /* load kernel file if type matches & et is in range ... */

    if ( type == lclso->_type && et >= lclso->_ets[0] && et <= lclso->_ets[1]) {
      lenfn = strlen( lclso->_absPathName);

      /* once a currently-unloaded kernel is found, it and all future kernels 
       * in this loop (that have et in range) must be reloaded to maintain 
       * the spicelib search order
       */

      if ( lclso->_handle == SPICEOUT) newoneloaded = 1;

      if ( newoneloaded) {
        lclLoadKernel( &doload, &lclso->_handle, &lenfn, lclso->_absPathName);
      }

    } /* if type= ->_type & et in range */

  } /* for lclso=spiceOpts */

  return;
} /* orbit_loadSpiceOpts */

/********************************/
/* macros for orbit_allocScPtrs */

#define OMALLOC( O, T) O = (T *) malloc( sc->_numInstrAlloc * sizeof(T))
#define OREALLOC( O, T) O = (T *) realloc( O, sc->_numInstrAlloc * sizeof(T))
#define OEITHERALLOC( O, T) \
  if ( sc->_numInstr) { OREALLOC( O, T); } else { OMALLOC( O, T); } \
  for ( i=sc->_numInstr; i<sc->_numInstrAlloc; ++i) O[i] = (T) NULL

/*****************************************************************/
/* malloc/realloc pointers of SC struct to make room at numInstr */
void
orbit_allocScPtrs( SC *sc) {
int i;

  if ( sc->_numInstr < sc->_numInstrAlloc) return;

  sc->_numInstrAlloc = 2 *
    ((sc->_numInstrAlloc>SC_NUMINSTR) ? sc->_numInstrAlloc : SC_NUMINSTR);

  OEITHERALLOC( instrVertPtrs,      double *);
  OEITHERALLOC( instrAbfPtrs,       double *);
  OEITHERALLOC( fov100InstrPtsPtrs, double *);
  OEITHERALLOC( sc->_nVert,         fortint);
  OEITHERALLOC( nInstrClosedPtsPtr, fortint);
  OEITHERALLOC( nFov100InstrPtsPtr, fortint);

  return;
}

/***********************************/
/* output spicespec info to a file */

void 
orbit_writeSpiceOpts( FILE *f, char *pfx, int writeENDSO) {
SPICEOPTS *lclso;
IDFLAGS *idFlag;
int isOptional;
struct utsname name;

  for ( lclso=spiceOpts; lclso; lclso=lclso->next) {
    idFlag = id2Char( lclso->_type, spiceFlags);
    isOptional = 0;
    if ( pfx) if (*pfx) fprintf( f, "%s", pfx);
    switch( lclso->_type) {
    case SPICEOPTS_SPKO:
    case SPICEOPTS_CKO:
    case SPICEOPTS_ORIBPCKO:
      isOptional = 1;
    case SPICEOPTS_TK:
    case SPICEOPTS_SPK:
    case SPICEOPTS_CK:
    case SPICEOPTS_ORIBPCK:
      fprintf( f, "%s ", idFlag->_flag);
    default:
      fprintf( f, "%s", lclso->_absPathName);
      if ( isOptional) fprintf( f, " %lf %lf", lclso->_ets[0], lclso->_ets[1]);
      fprintf( f, "\n");
      break;
    }
  }
  if ( pfx) if (*pfx) fprintf( f, "%s", pfx);
  fprintf( f, "HOST: %s\n", (!uname(&name)) ? name.nodename : "UNKNOWN");

  /* indicate end of SpiceOpts in this file so any SpiceOpts reader does not 
   * read the rest of the file
   */
  if ( writeENDSO) {
  IDFLAGS *endSoIDF = id2Char( SPICEOPTS_ENDSO, spiceFlags);
    if ( pfx) if (*pfx) fprintf( f, "%s", pfx);
    fprintf( f, "%s\n", endSoIDF->_flag ? endSoIDF->_flag 
                        : "PROGRAM ERROR - MISSING IDFLAG:  SPICEOPTS_ENDSO");
  }

  return;
}

/**************************/
/* initialize a SC struct */

SPUDV *
orbit_init( SC *sc) {
char *filnam;
fortint lenfilnam;

double fov[4];
double *campts, xxx;

fortint aster_utclen, aster_reflen;
fortint c255len= LEN255;
char c255[LEN255];
fortint c255alen;
char c255a[LEN255];
fortint i;
fortint lenfn;
int ii, gotenv, commentNum;
static SPUDV *spudv;
static SPUDF *spudf;
SPUDR spudr;
void *spicespecf;

static int noInitialViewEnv;
static int noInitialPlatesEnv;

static notfirst = 0;

  if ( !notfirst) {   /* only read SPICESPEC once */

    noInitialViewEnv = 0;
    noInitialPlatesEnv = 0;

    *utcInit = '\0';      /* init static char utcInit[LEN255] to empty string */

#ifndef CTOFORT_f2c
#ifdef hpux
    fpsetdefaults();
#endif
#endif
  
    dpr_fac = 45.0 / atan(1.0);
  
    filnam = getenv( "SPICESPEC");
    if ( !filnam) {
      gotenv = 0;
      filnam = "spicespec";
    } else gotenv=1;
    lenfilnam = strlen( filnam);
    /* ospice_init0( &lenfilnam, filnam);       /* replaced by code below */

/***********************************************************************
 * Copy the spicespec information to the linked list of SPICEOPTS
 *   using the absolute pathname
 */
#   define SOCOPYAPN( C) \
    { \
    char *apn; \
      if ( (apn=getAbsolutePath( C))) { \
        SOCOPY(apn, 0); \
        free( apn); \
      } \
    }

/***********************************************************************
 * Copy the spicespec information to the linked list of SPICEOPTS
 * - copy the BODYID argument to ((SPICEOPTS *)a)->_handle, this is only
 *   relevant for the case where (SPICEOPTS_OTHERBODYID == idFlag->_type)
 */
#   define SOCOPY( C, BODYID) \
    { \
    SPICEOPTS **pLastSO = &spiceOpts; \
    int lenF, lenC; \
      while( *pLastSO) pLastSO = &(*pLastSO)->next; \
      lenC = strlen( C); \
      lenF = strlen( idFlag->_flag); \
      if ( (*pLastSO=(SPICEOPTS *)malloc(sizeof(SPICEOPTS)+lenF+lenC+2))) { \
        (*pLastSO)->next = (SPICEOPTS *) 0; \
        (*pLastSO)->_type = idFlag->_type; \
        strcpy((*pLastSO)->_absPathName=(char *)((*pLastSO)+1),idFlag->_flag); \
        (*pLastSO)->_absPathName[lenF] = ' '; \
        strcpy((*pLastSO)->_absPathName+lenF+1,C); \
        (*pLastSO)->_filename = (char *) 0; \
        (*pLastSO)->_handle = BODYID; \
      } \
    }

#   define FOUNDFLAG( FLAG) (!strncmp( c255, FLAG, strlen(FLAG)))

    /*********************************************/
    /* putenv() seems more universal than setenv(), 
     * but i like the arguments & no-replace option of setenv() better 
     * - allocate space for string to putenv, it becomes part of the environment
     */

#   define setenv( E, V, O) \
    { \
    char *oldval = (char *) 0; \
    char eev[1024]; \
    char *newval; \
      if ( !O) oldval = getenv( E); \
      if ( !oldval) { \
        sprintf( eev, "%s=%s", E, V); \
        /* strcpy( newval = malloc( strlen( eev) + 1), eev); */ \
        newval = strdup( eev); \
        /* putenv( eev); */ \
        putenv( newval); \
      } \
    }

    /*********************************************************************/
    /* parse line to get filename:  "SPUD:<whitespace>filname<ws>[\n]\0"
     * - skip over flag & any whitespace that follows
     * - find next whitespace and terminate string there
     * - set env variable ENVNAM (e.g. "VIEW") if string is not empty
     */

#   define PARSEIT( ENVNAM, NOINITIALENV) \
    PARSEIT0; \
      for ( cptr2 = cptr; *cptr2 > ' '; ++cptr2) ; \
      *cptr2 = '\0'; \
      if ( *cptr) setenv( ENVNAM, cptr, NOINITIALENV) /* don't overwrite run-time ENVNAM */

    /************************/
    /* simple parse - skip white space */

#   define PARSEIT0 \
    for (cptr = c255 + strlen(idFlag->_flag); *cptr && *cptr<=' '; ++cptr)

    /***************************************************/
    /* sscanf first number after idFlag->_flag using format FMT
     * - read into NUM if one number read
     * - put BADVAL in NUM if nothing read
     */

#   define PARSENUM( FMT, NUM, BADVAL) \
    if ( !sscanf( c255+strlen(idFlag->_flag), FMT, &NUM) ) NUM = BADVAL

#   define RDSPUD \
    spudr.nlatR = SPUDRNLAT; \
    spudr.nlonR = SPUDRNLON; \
    spudr.eastlon = getview( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR); \
    if ( spudr.eastlon == -1) { \
      fprintf( stderr, "Failed to load shape model\n"); \
      exit(1); \
    } \
    if ( !spudr.eastlon) spudr.eastlon = -1; \
    \
    /* allocate face structure, convert spud to face */ \
    if ( !(spudf = (SPUDF *) malloc( sizeof(SPUDF))) ) { \
      fprintf( stderr, "Failed to allocate space for face structure\n"); \
      exit(1); \
    } \
    rmod2face( &spudr, spudf)

    spudf = (SPUDF *) 0;
    spudv = (SPUDV *) 0;
    spiceOpts = (SPICEOPTS *) 0;
    commentNum = 0;

    /* read a line at a time from SPICESPEC & its INCLUDE:'ed files */

    ospice_clpool();
    scid = asterid = earthid = sunid = 0;

    spicespecf = initReadFiles( filnam, (void *) 0);
    while( fgetsReadFiles( c255, LEN255, &spicespecf) ) {
    char *cptr, *cptr2;
    IDFLAGS *idFlag;
    int isOptional;
    fortint aFortInt;

      isOptional = 1;  /* used later by SPICEOPTS_*K[0]: */

      /* add current line as comment IFF it's non-null & commentNum > 0 */

      if ( strlen(c255) && commentNum > 0) {
        orbitgui_add_comment_sc( sc, commentNum, c255);
        commentNum = 0;  /* reset commentNum */
        continue;
      }

      idFlag = char2Id( c255, spiceFlags);               /* determine flag id */

      switch ( idFlag->_type) {
      case SPICEOPTS_LAST:                            /* line not interpreted */
        break;

      case SPICEOPTS_ENDSO:        /* no more SPICESPEC stuff in CURRENT file */
        fgetsReadFiles( (char *) 0, 0, &spicespecf); /* => close CURRENT file */
        break;

      case SPICEOPTS_INCLUDE:
        /* fgetsReadFiles missed an include file */
        fprintf( stderr, "WARNING:  SPICESPEC INCLUDE: file not found:\n%s\n"
                       , c255);
        break;

      case SPICEOPTS_UTC:              /* save UTC in static variable utcInit */
        PARSEIT0;                           /* - used later in orbit_init_utc */
        strncpy( utcInit, cptr, LEN255-1);
        utcInit[LEN255-1] = '\0';                             /* just in case */
        SOCOPY( cptr, 0) \
        break;

#     define CASEPARSEBODYID( OPT, ID) \
      case OPT: \
        PARSEBODYID( ID) \
        break

#     define PARSEBODYID( ID) \
        PARSEIT0; \
        ID = 0; \
        if ( 1!=sscanf( cptr, "%254s", c255a)) break; \
        c255alen = strlen( c255a); \
        ospice_obody( &ID, &c255alen, c255a); \
        SOCOPY( cptr, ID)

      CASEPARSEBODYID(  SPICEOPTS_SCID, scid);
      CASEPARSEBODYID(  SPICEOPTS_ASTID, asterid);
      CASEPARSEBODYID(  SPICEOPTS_EARTHID, earthid);
      CASEPARSEBODYID(  SPICEOPTS_OTHERBODYID, aFortInt);

      case SPICEOPTS_COMMENT:
        PARSENUM( "%d", commentNum, 0);
        break;

      case SPICEOPTS_SPUD:                  /* read in SPUD model (lat,lon,R) */

        /* never override setenv VIEW */
        if ( getenv("VIEW") && !noInitialViewEnv) break;

        noInitialViewEnv = 1;

#       define FREESPUD \
        if ( spudv) spud_freeSpudv( spudv); \
        if ( spudf) spud_freeSpudf( spudf, 1); \
        spudv = (SPUDV *) 0; \
        spudf = (SPUDF *) 0

        FREESPUD;

        PARSEIT( "VIEW", noInitialViewEnv);
        RDSPUD;
        if ( spudf) SOCOPYAPN( getenv("VIEW"))
        break;

      case SPICEOPTS_PLATES:          /* read in plate model x y z / v1 v2 v3 */

        /* never override setenv VIEW */
        if ( getenv("PLATES") && !noInitialPlatesEnv) break;

        noInitialPlatesEnv = 1;

        FREESPUD;

        PARSEIT( "PLATES", noInitialPlatesEnv);
        spudf = getplat( (SPUDR *) 0);
        if ( spudf) SOCOPYAPN( getenv("PLATES"))
        break;

      /* read in (past tense) one of the types of non-optional SPICE kernels */
      /* ***N.B. isOptional is set to 1 after each fgetsReadFiles() above */

      case SPICEOPTS_SPK:
      case SPICEOPTS_CK:
      case SPICEOPTS_ORIBPCK:
      case SPICEOPTS_TK:
        isOptional = 0;         /* clear isOptional & drop through */

      /* read in (past tense) one of the types of optional SPICE kernels */
      /* ".*O:<ws>NAME<ws>et0<ws>et1<ws>.*\n" */

      case SPICEOPTS_SPKO:
      case SPICEOPTS_CKO:
      case SPICEOPTS_ORIBPCKO:
      {
      int n;
      char fn[LEN255];
      double ets[2];
      SPICEOPTS *lclso, *lastso;
      char *apn;                                        /* absolute path name */
      int lenapn = 0;

        *fn = '\0';
        PARSEIT0;
        n = sscanf( cptr, isOptional ? "%s %lf %lf" : "%s", fn, ets, ets+1);

        if ( n == (isOptional ? 3 : 1) ) {
          if ( !(apn = getAbsolutePath( fn))) {
            fprintf( stderr, "***orbit_init:  %s%s\n   '%s'\n"
                           , "Skipping file for which absolute path name "
                           , "could not be determined; SPICESPEC line:"
                           , c255);
            break;
          }
          lenfn = strlen(fn); \
          lenapn = strlen(apn);
          if ( !(lclso=(SPICEOPTS *)malloc(sizeof( SPICEOPTS)+lenfn+lenapn+2))){
            fprintf( stderr, "***orbit_init:  %s\n"
                           , "Memory allocation failure, code lclso\n");
          }
        } else {
          lclso = (SPICEOPTS *) 0;
          fprintf( stderr, "***orbit_init:  %s\n"
                         , "Could not read tokens after %s\n", idFlag->_flag);
        }

        if ( !lclso) {
          fprintf( stderr, "***orbit_init:  %s\n  '%s'\n"
                         , "Problem setting up SPICE kernel; SPICESPEC line:"
                         , c255);
          break;
        }

        /* add to linked list if there is one ... */
        if ( spiceOpts) { 
          for ( lastso = spiceOpts; lastso->next; lastso = lastso->next) ;
          lastso->next = lclso;

        /* ... else this is the new start of the list */
        } else spiceOpts = lclso;

        /* fill in structure */
        lclso->next = (SPICEOPTS *) 0;
        lclso->_filename = (char *) (lclso + 1);
        strcpy( lclso->_filename, fn);
        lclso->_absPathName = lclso->_filename + strlen(fn) + 1;
        strcpy( lclso->_absPathName, apn);
        free( apn);
        lclso->_type = idFlag->_type;
        lclso->_handle = SPICEOUT;

        if ( isOptional) {
          lclso->_ets[0] = ets[0];
          lclso->_ets[1] = ets[1];

        } else {                                    /* non-optional kernels */
        fortint doload = 1;

          lclso->_ets[0] = lclso->_ets[1] = 0.0;

#         define LDBINKER(T,F) LDKER(T,F(&doload,&lclso->_handle,&lenfn,fn))

#         define LDKER(T,F) \
          case T: \
            F; \
            break

          switch( lclso->_type) {
          LDBINKER( SPICEOPTS_SPK, ospice_spkluef);
          LDBINKER( SPICEOPTS_CK, ospice_cklupf);
          LDBINKER( SPICEOPTS_ORIBPCK, ospice_pckluof);
          LDKER( SPICEOPTS_TK, ospice_ldpool(&lenfn,fn));
          }
        }

        /* in the future we may look at the kernels themselves to set _ets*/
        switch ( idFlag->_type) {
        case SPICEOPTS_SPKO:
        case SPICEOPTS_CKO:
        case SPICEOPTS_ORIBPCKO:
          break;
        case SPICEOPTS_SPK:
        case SPICEOPTS_CK:
        case SPICEOPTS_ORIBPCK:
        case SPICEOPTS_TK:
          break;
        default:
          fprintf( stderr, "***PROGRAM ERROR:  Contact programmer, Code %s\n"
                         , "WSNBATGH-orbit_init-SPICEOPTS-1\n");
          break;
        }
      } /* case SPICEOPTS_*K[O]: */

      } /* switch idFlag->_type */

      /* if we have SPUDF, convert it to spudv - once */
      if ( spudf && !spudv) spudv = newspudv( spudf);

    } /* while fgetsReadFiles */

    /* No SPUD or PLATES found in spicespec, try default PLATES, then SPUD */
    if ( !spudv) { 
    IDFLAGS *idFlag;
      spudf = getplat( (SPUDR *) 0);
      if ( spudf) {
        idFlag = id2Char(SPICEOPTS_PLATES,spiceFlags);
        if (SPICEOPTS_PLATES == idFlag->_type) {
          SOCOPYAPN( getenv("PLATES"))
        }
      } else {
        RDSPUD; 
        if ( spudf) {
          idFlag = id2Char(SPICEOPTS_SPUD,spiceFlags);
          if (SPICEOPTS_SPUD == idFlag->_type) {
            SOCOPYAPN( getenv("VIEW"))
          }
        }
      }
      spudv = newspudv( spudf);
    }
    orbit_writeSpiceOpts( stdout, (char *) 0, 0);
  } /* if !notfirst */

  sc->_numInstr = sc->_numInstrAlloc = 0; /* tell orbit_allocScPtrs to malloc */
  orbit_allocScPtrs( sc);                      /* allocate SC struct pointers */
  sc->_numInstr = SC_NUMINSTR;                         /* default instruments */

  ospice_init1( &scid, &asterid, &earthid, &sunid
	     , asterad, asterOrbit->_aa
             , msiB0, panel, msiUpDir, msiSampDir, fov, &nmsipts
             , nisNomB0, nisAxis, nisNomUpDir, nisNomSampDir
             , &nisfovx, &nisfovy, &nisStepAngle
             , &nnispts, &nisStepNom, &nisStepMax
             , &aster_utclen, &aster_reflen
             , &c255len, c255);    /* c255 contains UTC & new orbit ref frame */

  CPYVEC( msiB0, scB0);   /* set nominal S/C boresight equal to MSI boresight */

  /* copy  2 string fields from c255 - UTC & Asteroid Orbit reference frame */

  c255[aster_utclen+aster_reflen] = '\0';
  strcpy( asterOrbit->_newrefrm, c255+aster_utclen);
  c255[aster_utclen] = '\0';
  strcpy( asterOrbit->_newutc, c255);
  if ( aster_utclen && aster_reflen) {
    for ( i=0; i<7; i++) asterOrbit->_newvals[i] = asterOrbit->_aa[i];
  }

  maxrad = (asterad[0]>asterad[1]) ? 
              ((asterad[2]>asterad[0]) ? asterad[2] : asterad[0] ) :
              ((asterad[2]>asterad[1]) ? asterad[2] : asterad[1] );

  /* convert from MSI # pixels ([0],[1]) and uRadians/pixel ([2],[3]) 
   * to MSI field of view in radians
   */
  msifovx = fov[0] * fov[2] * 1e-6;
  msifovy = fov[1] * fov[3] * 1e-6;

  /* set nominal pointing, clear vertices' pointer so no rotation will be
   * done in orbit_set_instrument
   */
  nisStepAct = nisStepNom;
  nisvert = (double *) 0;

  /* get corners of polygon that represent each instrument's frame */

  /* separated SC_NIS2 from SC_NIS:  for ( i=SC_NIS; i<SC_NIS2; i++) { */
  for ( i=0; i<SC_NUMINSTR; ++i) {
  char *lclinstrname;
  fortint lename;
  int nisfactor, nAllCampts;
  double b2;

    lclinstrname = orbit_set_instrument( sc, i);
    lename = strlen( lclinstrname);
    if (  i == SC_NIS2) {
      lename -= 4;                                             /* drop "WIDE" */
      fovx = fovx / 2;                                 /* correct NIS2 to NIS */
    }

    sc->_nVert[i] = ncampts;

    campts = (double *) malloc( sizeof(double) * 2 * ncampts);
    ospice_init2( &scid, &ncampts, campts, &lename, lclinstrname);

    /* add 2 sets of vertices to camvert because NIS moves:
     * - vertices starting at camvert+0 will be those determined by step pos'n
     * -  " camvert+(3*ncampts) will be the normal width vertices
     * -  " camvert+(6*ncampts) will be the double width vertices
     */
    if ( i == SC_NIS || i == SC_NIS2) {
      nisfactor = 2;
      CPYVEC( nisNomB0, nisB0);
      CPYVEC( nisNomSampDir, nisSampDir);
      CPYVEC( nisNomUpDir, nisUpDir);
    } else {
      nisfactor = 1;
    }
    nAllCampts = ((nisfactor==1) ? 1 : 3) * 3 * ncampts;

    fov100Pts =
    fov100InstrPtsPtrs[i] = 
      (double *) malloc( nisfactor*sizeof(double)*3*MAXFOV100PTS);

    instrVertPtrs[i] =
    *camvertptr = camvert = (double *) malloc( sizeof(double) * nAllCampts);

    instrAbfPtrs[i] =
    *camabfptr = camabf = (double *) malloc( sizeof(double) * 3 * ncampts);

    *nClosedPts = ncampts;

    b2 = VDOT(boresight,boresight);                       /* this should be 1 */

    for ( ii=0; ii<ncampts; ii++) {                  /* loop through vertices */
    double *c, *cc, *ccc;
    double b2ccScale;
    VEC difVec;

      c = campts+(2*ii);
      cc = camvert+(3*ii);
      vmxpb( fovy*c[1], updir, boresight, cc);
      vmxpb( fovx*c[0], sampdir, cc, cc);

      b2ccScale = b2/VDOT(cc,boresight);    /* scale cc to plane of boresight */
      VSCAL( b2ccScale, cc);

      /* look for 1st duplicate of 1st vertex 
       * to get # of verts in closed polygon
       */
      if ( ii>2 && (*nClosedPts==ncampts)) {
        VMINUS2( cc, camvert, difVec);
        if ( VDOT(difVec,difVec) == 0.0) *nClosedPts = ii;
      }

      /* special NIS - store nominal normal & double width FOV's in camvert */

      if ( i == SC_NIS || i == SC_NIS2) {
        ccc = cc + (3 * ncampts);
        CPYVEC( cc, ccc);
        cc += (6 * ncampts);
        vmxpb( fovy*c[1], updir, boresight, cc);
        vmxpb( 2.0 * fovx*c[0], sampdir, cc, cc);
        b2ccScale = b2/VDOT(cc,boresight);  /* scale cc to plane of boresight */
        VSCAL( b2ccScale, cc);
      }
    } /* for ii=0 ... (vertices) */
    free( campts);

    if ( i==SC_NIS2) {   /* move nominal NIS2 camverts to camvert for fillFov */
      for ( ii=0; ii<ncampts; ++ii) {
        CPYVEC( camvert+(3*((2*ncampts)+ii)), camvert+(3*ii));
      }
    }
    orbit_fov_fillFov( sc);                           /* fill FOV with points */

    if ( nisfactor > 1) {                 /* for NIS, save nominal FOV points */
    double *ptr1 = fov100Pts;
    double *ptr2 = ptr1+(3*MAXFOV100PTS);
      for ( ii=0; ii<*nFov100Pts; ++ii, ptr1+=3, ptr2+=3) {
        CPYVEC( ptr1, ptr2);
      }
    }
  } /* for i=0 ... */

  /* s/c to MSI rotation and back */

  CPYVEC( msiSampDir, msctocam);
  CPYVEC( msiB0, msctocam+3);
  CPYVEC( msiUpDir, msctocam+6);
  MT2( msctocam, mcamtosc);

  scOrbit->_so_precess = scOrbit->_newprecess = 0;

  orbit_set_instrument( sc, SC_MSI);

  return spudv;
} /* orbit_init */


void
orbit_spkez( double *et, char *utc, SC *sc
           , VEC6 scv, MTX scmx
	   , VEC6 asterv, MTX astermx
           , VEC6 sunv, MTX sunmx
	   , VEC6 earthv, MTX earthmx, fortint *success, fortint *besthope) {

fortint utclen;

  utclen = (utc) ? strlen(utc) : 0;
  
  if ( utc && utclen > 0) ospice_utc2et( et, &utclen, utc);
  orbit_loadSpiceOpts( *et, SPICEOPTS_SPKO);

  if ( scEph == ORB_USESPK
     && asterEph == ORB_USESPK
     && earthEph == ORB_USESPK ) {
    ospice_spkez4( &scid, scv, scmx
                 , &asterid, asterv, astermx
                 , &sunid, sunv, sunmx
                 , &earthid, earthv, earthmx
                 , success, besthope
                 , et, &utclen, utc);
  } else {

    /* handle precession of lnode and argp */

    double lnode = scSo[SO_LNODE];
    double argp = scSo[SO_ARGP];

    scSo[SO_LNODE] = lnode + ((*et - scSo[SO_T0]) * scSo[SO_LNODEDOT]);
    scSo[SO_ARGP] = argp + ((*et - scSo[SO_T0]) * scSo[SO_ARGPDOT]);

    ospice_elts4( &scid, scv, scmx, &scEph, scSo, scSotoj2k
                , &scOrbit->_so_J2, &scOrbit->_so_Rpl, &scOrbit->_period
                , scOrbit->_so_uPA0, scOrbit->_so_uTP0
                , &asterid, asterv, astermx, &asterEph, asterSo, asterSotoj2k
                , &sunid, sunv, sunmx
                , &earthid, earthv, earthmx, &earthEph, earthSo, earthSotoj2k
                , success, besthope
                , et, &utclen, utc);

    scSo[SO_LNODE] = lnode;
    scSo[SO_ARGP] = argp;
  }
 
  return;

} /* orbit_spkez */

/*********************************/
/* convert et to a utc structure */

void
orbit_et2utc( double inpet, UTC *inpUtc) {
fortint utclenm1 = (UTCLEN - 1);
fortint utclenout;
int i;

  ospice_et2doy( &inpet, &utclenm1, &utclenout, inpUtc->_utcstr);
  inpUtc->_utcstr[ utclenout] = '\0';
  sscanf( inpUtc->_utcstr, "%d - %d T %d : %d : %d"
        , &inpUtc->_year, &inpUtc->_doy , &inpUtc->_hour
        , &inpUtc->_minute, &inpUtc->_second);

  return;
}

/******************************/
/* convert et to a utc string */

void
orbit_et2doy( double inpet, char *outUtc, long maxLenUtc) {
fortint utclenm1 = (maxLenUtc - 1);
fortint utclenout;

  ospice_et2doy( &inpet, &utclenm1, &utclenout, outUtc);
  outUtc[ utclenout] = '\0';
  return;
}

/*****************************************/
void 
orbit_utc2et( char *utc, double *et) {
fortint lenutc = strlen( utc);

  ospice_utc2et( et, &lenutc, utc);
  return;
}

/*******************************/
/* get the initial time to use 
 * - store it in UTC type
 *   - _utcstr                                  DOY string
  *  - _et                                      ephem. time
 *   - _year, _doy, _hour, _minute, _second     numerics
 */

void
orbit_init_utc( UTC *init_utc) {
char c255[LEN255];
int ilenutc = strlen( utcInit);       /* static variable char utcInit[LEN255] */

  if ( ilenutc < 1) {                       /* no initial value, use defaults */
    UTCLOAD( *init_utc);                              /* _year/... to _utcstr */
    orbit_utc2et( init_utc->_utcstr, &init_utc->_et);       /* _utcstr to _et */
  } else {
    orbit_utc2et( utcInit, &init_utc->_et);           /* saved utcInit to _et */
    orbit_et2doy( init_utc->_et, c255, LEN255);           /* et to doy string */
    sscanf( c255, "%d - %d T %d : %d : %d"   /* doy string to _year, _doy, ...*/
          , &init_utc->_year, &init_utc->_doy
          , &init_utc->_hour, &init_utc->_minute
          , &init_utc->_second);
    UTCLOAD( *init_utc);                              /* _year/... to _utcstr */
  }
  return;
}

/**************************************************/
/* matrix to euler angles
 *** N.B. vec[0/1/2] -> Axes 3/1/2 => Z/X/Y rotation
 *** N.B. vec returned in DEGREES
 */

void
c_m2eul( MTX mtx, VEC vec) {
static fortint ax1=3, ax2=1, ax3=2;

  m2eul( mtx, &ax3, &ax2, &ax1, vec+2, vec+1, vec);

  vec[0] = r2d(vec[0]);
  vec[1] = r2d(vec[1]);
  vec[2] = r2d(vec[2]);

  while ( vec[0] < 0.0) vec[0] += 360.0;
  while ( vec[2] < 0.0) vec[2] += 360.0;

  return;
}

/**************************************************************/
/* copy instrument info from IMGFRM structure to SC structure */

void
copyInstrument_ImgfrmToSc( IMGFRM *imgfrm, SC *sc) {
POINTING *scBore, *scRoll;

  if ( !sc || !imgfrm) return;

  orbitgui_return_boreroll( sc, &scBore, &scRoll);
  CPYVEC( imgfrm->_boreAimptVec, scBore->aimpt.vec);
  CPYVEC( imgfrm->_boreScvecVec, scBore->scvec.vec);
  CPYVEC( imgfrm->_rollAimptVec, scRoll->aimpt.vec);
  CPYVEC( imgfrm->_rollScvecVec, scRoll->scvec.vec);

  scBore->aimpt.type = imgfrm->_boreAimptType;
  scBore->scvec.type = imgfrm->_boreScvecType;
  scRoll->aimpt.type = imgfrm->_rollAimptType;
  scRoll->scvec.type = imgfrm->_rollScvecType;

  strcpy( sc->_instrName, imgfrm->_instrName);
  sc->_instrument = imgfrm->_instrument;
  switch ( imgfrm->_instrument) { 
  case SC_NIS: 
  case SC_NIS2: 
    sc->_nisStepAct = imgfrm->_nisPosition; 
    break; 
  case SC_MSI: 
  default: 
    break; 
  } 

  orbitgui_update_BoreRoll( sc);
  return;
}

/**********************************************************************/
/* copy instrument info from SC structure to IMGFRM structure */

void
copyInstrument_ScToImgfrm( SC *sc, IMGFRM *imgfrm) {
POINTING *scBore, *scRoll;

  if ( !sc || !imgfrm) return;

  orbitgui_return_boreroll( sc, &scBore, &scRoll);
  CPYVEC( scBore->aimpt.vec, imgfrm->_boreAimptVec);
  CPYVEC( scBore->scvec.vec, imgfrm->_boreScvecVec);
  CPYVEC( scRoll->aimpt.vec, imgfrm->_rollAimptVec);
  CPYVEC( scRoll->scvec.vec, imgfrm->_rollScvecVec);

  imgfrm->_boreAimptType = scBore->aimpt.type;
  imgfrm->_boreScvecType = scBore->scvec.type;
  imgfrm->_rollAimptType = scRoll->aimpt.type;
  imgfrm->_rollScvecType = scRoll->scvec.type;

  strcpy( imgfrm->_instrName, sc->_instrName);
  imgfrm->_instrument = sc->_instrument;

  switch ( imgfrm->_instrument) {
  case SC_NIS:
  case SC_NIS2:
    imgfrm->_nisPosition = sc->_nisStepAct;
    break;
  case SC_MSI:
  default:
    break;
  }
  return;
}

/**********************************************************************
/* write a linked list of image frame structures to a file
 * - assumes last item in list has ->nextif of NULL or points to first item
 */

void
write_imgfrm_list( IMGFRM *imgfrm, char *file, int eastlon, int litframesonly
                 , int minHits4, long *framesoutBits, SPUDF *spudf) {
IMGFRM *lclimgfrm = imgfrm;
IMGFRM *nextimgfrm;
FILE *outfile;
char *orbit_CAS_TypeToName( int);

char doy[UTCLEN], cal[UTCLEN], jd[UTCLEN], sclkch[UTCLEN];
int doylenout, callenout, jdlenout, sclkchlenout;
int utclenm1 = (UTCLEN - 1);
int i;
int allinfo;

double lcllen, p5len, sclen, sunlen, boreJ2klen, sctosunJ2klen;
double lcllat, p5lat, sclat, sunlat, boreJ2klat, sctosunJ2klat;
double lcllon, p5lon, sclon, sunlon, boreJ2klon, sctosunJ2klon;
double boreoffsunang;
double paneloffsunang;
double *lclvec;

#define MAXNCKREC 5000
double sclkarr[MAXNCKREC], *sclkptr, quatarr[MAXNCKREC][4], *quatptr;
double avarr[MAXNCKREC][3], *avptr;
double lastsclk;
fortint *intstartptr;
long nRec;
long numIntervals;
char ckfn[LEN255];
fortint ckfnlen, nSeg, nearscid;

VEC p5vec, scvec, sunvec, boreJ2k, sctosunJ2k;

  if ( strcmp( file, "-") ) {
    outfile = fopen( file, "w");
    if ( !outfile) {
      fprintf( stderr, "Error opening %s\n", file);
      return;
    }
    fprintf( stderr, "\nWriting %s ...\n", file);
  } else {
    outfile = stderr;
  }
  fflush( stderr);

  orbit_writeSpiceOpts( outfile, (char *) 0, 1);

  /* loop through linked list of image frames */
  while ( lclimgfrm ) {
  int hideone;
    hideone = (litframesonly && ( lclimgfrm->_incid > 90.0));
    hideone |= minHits4 > lclimgfrm->_nhits4;
	    if ( !hideone ) {

	      /* convert et to ascii times */
	      ospice_et2doy( &lclimgfrm->_et, &utclenm1, &doylenout, doy);
	      ospice_et2jd( &lclimgfrm->_et, &utclenm1, &jdlenout, jd);
	      ospice_scdecd( &lclimgfrm->_scid, &lclimgfrm->_sclk
			   , &utclenm1, &sclkchlenout, sclkch);
	      doy[doylenout] = '\0';
	      jd[jdlenout] = '\0';
	      sclkch[sclkchlenout] = '\0';

	#define LENLATLON( V, LE,LA,LO,S) \
	      LE = VLEN( V); \
	      LA = (LE>0.0) ? r2d(asin( (V)[2] / LE) ) : -999.0; \
	      LO = (LE==0.0) ? 0.0 : r2d( eastlon * atan2( (V)[1], (V)[0]) )

	      /* lat, lon, range of p5 point, sc from body center, & sun */
	      CPYVEC( lclimgfrm->_boreVec, p5vec);
	      LENLATLON( p5vec, p5len, p5lat, p5lon, "p5vec");
	      VADD2( p5vec, lclimgfrm->_boretoscVec, scvec);
	      LENLATLON( scvec, sclen, sclat, sclon, "scvec");
	      VADD2( p5vec, lclimgfrm->_boretosunVec, sunvec);
	      LENLATLON( sunvec, sunlen, sunlat, sunlon, "sunvec");

	      /* J2000 vectors, locally eliminate eastlon so angles are RA & DEC */
	#define eastlon 1
	      CPYVEC( lclimgfrm->_boresightJ2k, boreJ2k);
	      LENLATLON( boreJ2k, boreJ2klen, boreJ2klat, boreJ2klon, "boreJ2k");
	      CPYVEC( lclimgfrm->_sunFromScJ2k, sctosunJ2k);
	      LENLATLON( sctosunJ2k, sctosunJ2klen, sctosunJ2klat, sctosunJ2klon
		       , "sctosunJ2k");
	#undef eastlon

	#define V2TODEG( ANG, V1, V2) \
	      ANG = VDOT(V1,V2) /  \
		sqrt(VDOT(V1,V1) * VDOT(V2,V2)); \
	      \
	      if ( ANG >= 1.0) ANG = 1.0; \
	      else if ( ANG <= -1.0) ANG = -1.0; \
	      \
	      ANG = r2d( acos( ANG) )

	      V2TODEG( boreoffsunang, sctosunJ2k, boreJ2k);
	      V2TODEG( paneloffsunang, sctosunJ2k, lclimgfrm->_panelJ2k);

	#define VEC3LIST(A) (A)[0],(A)[1],(A)[2]
	#define IFBIT( BIT) \
		if ( allinfo || ( (1L << (BIT % FRAMESOUT_BITSPERLONG)) \
		     & framesoutBits[BIT / FRAMESOUT_BITSPERLONG]) )

		allinfo = 0;
		IFBIT( FRAMESOUT_BIT_ALLINFO) allinfo = 1;

	#define AIMPTNAME(TYP) \
	  ((TYP) == Iabf ? "ABF" : \
	  ((TYP) == Iaci ? "ACI" : \
	  ((TYP) == Inadir ? "NADIR" : \
	  ((TYP) == Ij2k ? "J2KI" : \
	  ((TYP) == Ieci ? "ECI" : \
	  ((TYP) == Isci ? "SCI" : "???"))))))
	#define BOREVNAME(TYP) \
	  ((TYP) == Iinstr ? "INSTR" : \
	  ((TYP) == Ipanel ? "PANEL" : \
	  ((TYP) == Ix ? "X'" : \
	  ((TYP) == Iy ? "Y'" : \
	  ((TYP) == Iz ? "Z" : \
	  ((TYP) == Iuser ? "USER" : "???"))))))

              /* start with a blank line, then print the longitude conversion */

              fprintf( outfile
                , "\n%12d :conversion factor to east longitude (unlabeled lon's are %s lon)\n"
                , eastlon
                , eastlon == 1 ? "EAST" : (eastlon == -1 ? "WEST" : "UNKNOWN")
                );

	      IFBIT( FRAMESOUT_BIT_GENINFO)
		fprintf( outfile, "%12ld %1d :%s (%s %s)\n"
		       , lclimgfrm->_instrument, lclimgfrm->_nisDarkFollows
		       , FRAMESOUT_SFX_GENINFO
		       , lclimgfrm->_instrName
		       , (lclimgfrm->_nisDarkFollows ? "Yes" : "No"));
	      IFBIT( FRAMESOUT_BIT_POINTING) {
		fprintf( outfile, "%2d %2d %2d %2d :%s (%5s %5s %5s %5s)\n"
		       , lclimgfrm->_boreAimptType
		       , lclimgfrm->_boreScvecType
		       , lclimgfrm->_rollAimptType
		       , lclimgfrm->_rollScvecType
		       , FRAMESOUT_SFX_POINTING_TYPE
		       , BOREVNAME( lclimgfrm->_boreAimptType)
		       , BOREVNAME( lclimgfrm->_boreScvecType)
		       , AIMPTNAME( lclimgfrm->_rollAimptType)
		       , AIMPTNAME( lclimgfrm->_rollScvecType));
		fprintf( outfile, "%12lg %12lg %12lg                 :%s\n"
		       , VEC3LIST( lclimgfrm->_boreAimptVec)
		       , FRAMESOUT_SFX_POINTING_AIMPT);
		fprintf( outfile, "%12lg %12lg %12lg                 :%s\n"
		       , VEC3LIST( lclimgfrm->_boreScvecVec)
		       , FRAMESOUT_SFX_POINTING_VBORE);
		fprintf( outfile, "%12lg %12lg %12lg                 :%s\n"
		       , VEC3LIST( lclimgfrm->_rollAimptVec)
		       , FRAMESOUT_SFX_POINTING_ROLLAIMPT);
		fprintf( outfile, "%12lg %12lg %12lg                 :%s\n"
		       , VEC3LIST( lclimgfrm->_rollScvecVec)
		       , FRAMESOUT_SFX_POINTING_ROLLSCVEC);
	      }
	      IFBIT( FRAMESOUT_BIT_TIME_ALT)
		fprintf( outfile, "%14.3lf %13lf :%s %s %s\n"
		       , lclimgfrm->_et, lclimgfrm->_sclk
		       , FRAMESOUT_SFX_TIME_ALT
		       , doy, sclkch);
	      IFBIT( FRAMESOUT_BIT_TIME)
		fprintf( outfile, "%12.1lf %10.4lf %s %s :%s\n"
		       , lclimgfrm->_et, lclimgfrm->_orbitnum, jd, doy
		       , FRAMESOUT_SFX_TIME);
	      IFBIT( FRAMESOUT_BIT_NISINFO)
		fprintf( outfile, "%12d %12d %12d %12d :%s\n"
		       , lclimgfrm->_nisPosition
		       , lclimgfrm->_nisScannum
		       , lclimgfrm->_nisSpecnum
		       , lclimgfrm->_nisDuration
		       , FRAMESOUT_SFX_NISINFO);
	      IFBIT( FRAMESOUT_BIT_P5PLATE) {
	      VEC p5PltVec, uvP5PltVec, uvToSun;
	      double p5PltLen;
	      unsigned long plateIdx;
	      double p5PltRad, p5PltLat, p5PltLon;
	      double incid, emiss, phase;
	#     define ACOSWLIM(B) ((B>1.0) ?0.0 :((B<-1.0) ? 180.0 : (acos(B) rad)))

		p5PltLen = -1.0 / VLEN( lclimgfrm->_boretoscVec);  /* convert bore>sc */
		VSCAL2( p5PltLen, lclimgfrm->_boretoscVec, uvP5PltVec);/* to unit vec */
		spudf_intersect( spudf, scvec, uvP5PltVec, &p5PltLen, &plateIdx);
		if ( plateIdx < spudf->nface) {
		  vmxpb( p5PltLen,uvP5PltVec,scvec,p5PltVec);  /* plate intersect ABF */
		  vhat( lclimgfrm->_boretosunVec, uvToSun);
		  incid = VDOT( uvToSun, spudf->uvnorms+(3*plateIdx));
		  emiss = - VDOT( uvP5PltVec, spudf->uvnorms+(3*plateIdx));
		  phase = - VDOT( uvP5PltVec, uvToSun);
		  incid = ACOSWLIM(incid);
		  emiss = ACOSWLIM(emiss);
		  phase = ACOSWLIM(phase);
		} else { 
		  CPYVEC( p5vec, p5PltVec);
		  incid = emiss = phase = -999.0;
		}
		LENLATLON( p5PltVec, p5PltRad, p5PltLat, p5PltLon, "p5Plt");
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf %7ld"
		       , VEC3LIST( p5PltVec), p5PltRad, p5PltLat, p5PltLon
		       , (plateIdx < spudf->nface) ? spudf->platenum[plateIdx] : -1);
		fprintf( outfile, " %7.2lf %7.2lf %7.2lf :%s\n"
		       , incid, emiss, phase
		       , "p5plt xyz,r,lat,lon,plt#,i,e,p");
	      }
	      IFBIT( FRAMESOUT_BIT_P5PHOTOM)
		fprintf( outfile, "%12lg %12lg %12lg      %7.2lf %7.2lf %7.2lf :%s\n"
		       , VEC3LIST( lclimgfrm->_p5normVec)
		       , lclimgfrm->_incid, lclimgfrm->_emiss, lclimgfrm->_phase
		       , FRAMESOUT_SFX_P5PHOTOM);
	      IFBIT( FRAMESOUT_BIT_P5VEC)
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%s\n"
		       , VEC3LIST( p5vec), p5len, p5lat, p5lon
		       , FRAMESOUT_SFX_P5VEC);
	      IFBIT( FRAMESOUT_BIT_SCVEC)
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%s\n"
		       , VEC3LIST( scvec), sclen, sclat, sclon
		       , FRAMESOUT_SFX_SCVEC);
	      IFBIT( FRAMESOUT_BIT_SUNVEC) 
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%s\n" 
		       , VEC3LIST( sunvec), sunlen, sunlat, sunlon 
		       , FRAMESOUT_SFX_SUNVEC);
	      IFBIT( FRAMESOUT_BIT_BOREJ2K)
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%s\n"
		       , VEC3LIST( boreJ2k), boreJ2klen, boreJ2klon, boreJ2klat
		       , FRAMESOUT_SFX_BOREJ2K);
	      IFBIT( FRAMESOUT_BIT_SC2SUNJ2K)
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%s\n"
		       , VEC3LIST(sctosunJ2k), sctosunJ2klen,sctosunJ2klon,sctosunJ2klat
		       , FRAMESOUT_SFX_SC2SUNJ2K);
	      IFBIT( FRAMESOUT_BIT_SCLKQUAT) {
		fprintf( outfile, "%13lf %17.15lf %17.15lf %17.15lf %17.15lf :%s\n"
		       , lclimgfrm->_sclk
		       , lclimgfrm->_scQuat[0] , lclimgfrm->_scQuat[1]
		       , lclimgfrm->_scQuat[2] , lclimgfrm->_scQuat[3]
		       , FRAMESOUT_SFX_SCLKQUAT);
		fprintf( outfile, "%17.15lf %17.15lf %17.15lf %17.15lf :%s\n"
		       , lclimgfrm->_abfQuat[0] , lclimgfrm->_abfQuat[1]
		       , lclimgfrm->_abfQuat[2] , lclimgfrm->_abfQuat[3]
		       , FRAMESOUT_SFX_ABFQUAT);
	      }
	      IFBIT( FRAMESOUT_BIT_OFFSUN) {
	      double tmplen, tmplat, tmplon;

		fprintf( outfile, "%12lg        :%s\n"
		       , boreoffsunang
		       , FRAMESOUT_SFX_OFFSUN_BORE);
		fprintf( outfile, "%12lg        :%s\n"
		       , paneloffsunang
		       , FRAMESOUT_SFX_OFFSUN_PANEL);

		/* special for testing - output X' and Z axis in J2K */
		/* ***N.B. ASSUMES BORESIGHT IS X' AXIS */

	#define eastlon 1
		LENLATLON( lclimgfrm->_panelJ2k, tmplen, tmplat, tmplon, "xxx");
	#undef eastlon
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%s\n"
		       , VEC3LIST( lclimgfrm->_panelJ2k), tmplen, tmplon, tmplat
		       , "Panel(Z)J2k xyz, rRADEC");
	      }

	      IFBIT( FRAMESOUT_BIT_EARTHVEC) {
	      double tmplen, tmplat, tmplon;
	      int earthInFan;
		/* fanbeam FOV calculation - roughly centered toward Z axis
		 *   (1) +/- 4 degrees out of XZ plane i.e. toward +/- Y axis
		 *   (2) +25 degrees out of YZ plane i.e. toward +X axis
		 *   (3)  -5 degrees out of YZ plane i.e. toward -X axis
		 * for purposes of this calculation:
		 *   (1)  +tmplat = rotation of Z out of XZ plane toward +Y
		 *   (2)  +tmplon = rotation around Y out of YZ plane toward +X
		 *   (3)  -180 < tmplon <= 180.0; compare to -5 to +25 range
		 *   (4)  tmplon = tmplat = -999.0 on error
		 */
		tmplen = VLEN(lclimgfrm->_earthFromScSC);
		tmplat = (tmplen == 0.0) ? -999.0 :
			 r2d(asin( lclimgfrm->_earthFromScSC[1] / tmplen) );
		tmplon = (tmplen == 0.0) ? -999.0 : 
			 ( (lclimgfrm->_earthFromScSC[0] == 0.0 &&
			    lclimgfrm->_earthFromScSC[2] == 0.0) ? 0.0 :
			    r2d( atan2( lclimgfrm->_earthFromScSC[0]
				      , lclimgfrm->_earthFromScSC[2]))
			 );
		if (tmplon > 180.0) tmplon -= 360.0;  /* does not affect -999.0 */
		earthInFan = (tmplon >= -5.0 && tmplon <= 25.0 && fabs(tmplat) <= 4.0);
		fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf %s :%s %s\n"
		       , VEC3LIST( lclimgfrm->_earthFromScSC), tmplen, tmplon, tmplat
		       , earthInFan ? "1" : "0"
		       , FRAMESOUT_SFX_EARTHVEC
		       , earthInFan ? "(Yes)" : "(No)");
	      } /* FRAMESOUT_BIT_EARTHVEC) */

	      IFBIT( FRAMESOUT_BIT_AGENINFO) {
	      AGEN *agen = lclimgfrm->_agen;
		fprintf( outfile, "%12ld :%s <%s>\n"
		       , lclimgfrm->_seqNum + 1 /* 1-based for Annbo */
		       , FRAMESOUT_SFX_AGENINFO
		       , (agen) ? lclimgfrm->_agen->_name : "not an autogen sequence" 
		       );
		if ( agen) 
	#define IFO(A) (agen->_typeAgen == (A))
		if ( IFO(AGEN_TYPE_OLAP) || IFO(AGEN_TYPE_OLAP2)) {
		  fprintf( outfile, "%12ld %12lf :%s\n"
			 , lclimgfrm->_etDelta, lclimgfrm->_Overlap
			 , FRAMESOUT_SFX_AGENINFO_RESULT
			 );
		}
		/* output frame autogen info for first frame only */
		if ( agen && !lclimgfrm->_seqNum) {
		  fprintf( outfile, "%12ld %12.1lf :%s %s\n"
			 , agen->_refs, agen->_EtStart
			 , FRAMESOUT_SFX_AGENINFO_START
			 , agen->_utc);
		  fprintf( outfile, "%12lf %12lf %5d :%s (%s)\n"
			 , agen->_MaxTime, agen->_MaxOrbits, agen->_typeAgen
			 , FRAMESOUT_SFX_AGENINFO_DURTYP
			 , IFO(AGEN_TYPE_OLAP) ? "Overlap" :
			  (IFO(AGEN_TYPE_OLAP2) ? "Overlap2" :
			  (IFO(AGEN_TYPE_TIMING) ? "Constant Timing" :
			  (IFO(AGEN_TYPE_FRAG) ? orbit_CAS_TypeToName(agen->_fragType)
					       : "Unknown Agen Type")))
			 );
	#define IFI(A) (agen->_FrmOverlapTypeBits == (1L<<(A)))
		  switch (agen->_typeAgen) {
		  case AGEN_TYPE_OLAP:
		  case AGEN_TYPE_OLAP2:
		    fprintf( outfile, "%12lf %12lf %5ld :%s (%s)\n"
			   , agen->_FrmOverlap
			   , IFO(AGEN_TYPE_OLAP) ?agen->_FrmOverlap :agen->_FrmOverlap2 
			   , agen->_FrmOverlapTypeBits
			   , FRAMESOUT_SFX_AGENINFO_OLAPSPEC
			   , IFI(OVERLAP_TYPE_ANN_BIT) ? "Annbo" :
			    (IFI(OVERLAP_TYPE_JIM_BIT) ? "Jimbo" :
			    (IFI(OVERLAP_TYPE_FOV100_BIT) ? "FOV100" :
			    (IFI(OVERLAP_TYPE_JIM_NEW_BIT) ? "Jimbo2" : "???")))
			   );
		    break;
	#undef IFI
	#undef IFO
		  case AGEN_TYPE_TIMING:
		    switch (lclimgfrm->_instrument) {
		    case SC_MSI:
		      fprintf( outfile, "%17.4lf :%s\n" /* "%12d :%s\n" */
			     , agen->_TimePerMSIFrm
			     , FRAMESOUT_SFX_AGENINFO_MSITIMESPEC);
		      break;
		    case SC_NIS:
		    case SC_NIS2:
		      fprintf( outfile, "%7d %7d %7d :%s\n"
			     , agen->_StartingStep
			     , agen->_EndingStep
			     , agen->_DeltaStep
			     , FRAMESOUT_SFX_AGENINFO_NISTIMESPEC1);
		      fprintf( outfile, "%7d %7d %7d :%s\n"
			     , agen->_TimePerStep
			     , agen->_DelayPerStep
			     , agen->_DelayPerRow
			     , FRAMESOUT_SFX_AGENINFO_NISTIMESPEC2);
		      break;
		    default:
		      break;
		    } /* switch _instrument */
		    break;

		  case AGEN_TYPE_FRAG:
		    fprintf( outfile, " :autogen FRAG %s,%s\n"
			   , orbit_CAS_TypeToName(agen->_fragType) 
			   , agen->_fragArgs ? agen->_fragArgs : "???");
		    for ( i=0; i<12; ++i) {
		      if ( agen->_seqDefArgs[i]) 
			fprintf( outfile, " :autogen SEQDEF %d %s,%s\n"
			       , i+1
			       , orbit_CAS_TypeToName(agen->_seqDefType[i]) 
			       , agen->_seqDefArgs[i] ? agen->_seqDefArgs[i] : "???");
		    }
		    break;

		  default:
		    break;
		  } /* switch _typeAgen */
		} /* seqNum == 0 */
	      } /* FRAMESOUT_BIT_AGENINFO */

	      IFBIT( FRAMESOUT_BIT_CAMPTVECS) {
		fprintf( outfile, "%12d %12d:%s\n"
		       , lclimgfrm->_ncampts
		       , lclimgfrm->_nhits4
		       , FRAMESOUT_SFX_CAMPTVECS_NCAMPTS);

		for (i=0; i<lclimgfrm->_ncampts; i++) {
		static char lclvecnam[40];
		  lclvec = lclimgfrm->vec3 + (3 * i);
		  LENLATLON( lclvec, lcllen, lcllat, lcllon, lclvecnam);
		  fprintf( outfile, "%12lg %12lg %12lg %12lg %7.2lf %7.2lf :%d %s\n"
			 , VEC3LIST( lclvec), lcllen, lcllat, lcllon, i
			 , FRAMESOUT_SFX_CAMPTVECS_PTFMT);
		}
	      } /* FRAMESOUT_BIT_CAMPTVECS */

	      IFBIT( FRAMESOUT_BIT_TARGVECSBF) {
	      double tmplen, tmpdec, tmpra;
		/* vector to center of target in SBF coordinates
		 * for purposes of this calculation:
		 *   (1)  +tmpdec = rotation out of XZ plane toward +Y (atan(y/x))
		 *   (2)  +tmpra = rotation around Y of +XY plane toward -Z (atan(-z/x))
		 *   (3)  -180 < tmpra <= 180.0
		 *   (4)  tmpra = tmpdec = -999.0 on error
		 */
		tmplen = VLEN(lclimgfrm->_astFromScSC);
		tmpdec = (tmplen == 0.0) ? -999.0 :
			 r2d(asin( lclimgfrm->_astFromScSC[1] / tmplen) );
		tmpra = (tmplen == 0.0) ? -999.0 : 
			 ( (lclimgfrm->_astFromScSC[0] == 0.0 &&
			    lclimgfrm->_astFromScSC[2] == 0.0) ? 0.0 :
			    r2d( atan2( -lclimgfrm->_astFromScSC[2]
				      , lclimgfrm->_astFromScSC[0]))
			 );
		if (tmpra > 180.0) tmpra -= 360.0;  /* does not affect -999.0 */
		fprintf( outfile, "%12lg %12lg %12lg %12lg %8.3lf %8.3lf :%s\n"
		       , VEC3LIST( lclimgfrm->_astFromScSC), tmplen, tmpra, tmpdec
		       , FRAMESOUT_SFX_TARGVECSBF);
	      } /* FRAMESOUT_BIT_TARGVECSBF */

	      /*****************************************************************
	       * vectors and rotations to other bodies
	       */
	      IFBIT( FRAMESOUT_BIT_OTHERBODIES) {
	      double tmplen, tmpdec, tmpra;
	      OTHERBODIES *pOB;
	      int iBody = 0;

		for ( pOB=lclimgfrm->_otherBodies; pOB; iBody++, pOB=pOB->next)
		{
		  fprintf( outfile
			 , "%12ld %d :Body=%ld: SPICE ID (%s), Body sequence #\n"
			 , pOB->_bodyId
			 , iBody
			 , pOB->_bodyId
			 , pOB->_bodyName
			 );

	#         define eastlon 1

		  CPYVEC(pOB->_scToBodyJ2k, lclvec);
		  LENLATLON( lclvec, lcllen, lcllat, lcllon, "scToBodyJ2k");
		  fprintf( outfile
			 ,"%12lg %12lg %12lg %12lg %7.2lf %7.2lf :Body=%ld: %s\n"
			 , VEC3LIST( lclvec), lcllen, lcllon, lcllat
			 ,  pOB->_bodyId
			 , "scToBodyJ2k xyz,rRADEC"
			 );

		  CPYVEC(pOB->_scToBodySbf, lclvec);
		  LENLATLON( lclvec, lcllen, lcllat, lcllon, "scToBodySbf");
		  fprintf( outfile
			 ,"%12lg %12lg %12lg %12lg %7.2lf %7.2lf :Body=%ld: %s\n"
			 , VEC3LIST( lclvec), lcllen, lcllon, lcllat
			 ,  pOB->_bodyId
			 , "scToBodySbf xyz,rRADEC"
			 );

	#         undef eastlon
	#         define eastlon -1 /* force West longitude */

		  CPYVEC(pOB->_bodyToScBf, lclvec);
		  LENLATLON( lclvec, lcllen, lcllat, lcllon, "bodyToScBf");
		  fprintf( outfile
			 ,"%12lg %12lg %12lg %12lg %7.2lf %7.2lf :Body=%ld: %s\n"
			 , VEC3LIST( lclvec), lcllen, lcllat, lcllon
			 ,  pOB->_bodyId
			 , "bodyToScBf xyz,r,lat,lon(West)"
                 );

          fprintf( outfile
                 ,"%7.2lf :Body=%ld: %s\n"
                 ,  pOB->_angleBodyToBore
                 ,  pOB->_bodyId
                 , "angleBodyToBoresight, deg"
                 );

          fprintf( outfile
                 , "%17.15lf %17.15lf %17.15lf %17.15lf :Body=%ld: %s\n"
                 , pOB->_bodyQuat[0] , pOB->_bodyQuat[1]
                 , pOB->_bodyQuat[2] , pOB->_bodyQuat[3]
                 ,  pOB->_bodyId
                 , "SPICE Body quaternion(a,x,y,z) (J2k->Body)"
                 );
#         undef eastlon
        } /* for ( pOB=lclimgfrm->_otherBodies; pOB; iBody++, pOB=pOB->next) */

      } /* FRAMESOUT_BIT_OTHERBODIES */

    } /* if ( !hideone) ( litframesonly ...) */

    /* next frame, ensure end of loop even if last ->nextif is not null */
    lclimgfrm = (lclimgfrm->nextif==imgfrm) ?(IMGFRM *)NULL : lclimgfrm->nextif;
  }

  if ( outfile != stderr) {
    fclose( outfile);
    fprintf( stderr, "done\n");
  }
  fflush( stderr);

  /* skip C Kernel writing if Write out CK" bit is not set */

  if ( !(framesoutBits[FRAMESOUT_NUMROWS] & 1L)) return;

  /* write C kernel
   * - store times & quaternions, up to MAXNCKREC, and save as an interval
   */

  strcpy( ckfn, file);
  strcat( ckfn, ".bc");
  ckfnlen = strlen( ckfn);

  remove( ckfn);    /* ensure it does not exist */

  nSeg = 0;                /* first call to ospice_writeck will create a file */
  nearscid = imgfrm->_scid * 1000;

  lastsclk = imgfrm->_sclk;                          /* ensure initialization */
  nRec = 0;   /* ensure no file written on initialization - see WRITECK macro */
  numIntervals = 1;
  lclimgfrm=imgfrm;

  /* macro to write out a segment - does nothing if no records loaded yet */

#define WRITESEG \
  if ( nRec) { \
  fortint inew = (nSeg ? 0 : 1); \
  fortint writav = 1; \
    ospice_writeck( &inew, &nearscid, &writav \
                  , &nRec, sclkarr, quatarr, avarr \
                  , &numIntervals, sclkarr, &ckfnlen, ckfn); \
    if ( !(nSeg++)) { \
      fprintf( stderr, "Writing C Kernel file %s ...", ckfn); \
    } \
  }

  while ( lclimgfrm) {

    /* if time reverses or arrays fill up, write current segment & start next */

    if ( lclimgfrm->_sclk <= lastsclk || nRec == MAXNCKREC ) {
      WRITESEG

      if ( nRec && lclimgfrm->_sclk > lastsclk) {/* overlap ends of segments */
        *sclkarr = lastsclk;
        memcpy( quatarr[0], quatptr-4, 4 * sizeof(double));
        memcpy( avarr[0], avptr-3, 3 * sizeof(double));
        nRec = 1;
        sclkptr = sclkarr + 1;
        quatptr = quatarr[1];
        avptr = avarr[1];

      } else {                                   /* first pass initialization */

        nRec = 0;
        sclkptr = sclkarr;
        quatptr = quatarr[0];
        avptr = avarr[0];
      }
    }

    lastsclk = *sclkptr = lclimgfrm->_sclk;                      /* copy sclk */
    memcpy( quatptr, lclimgfrm->_scQuat, 4 * sizeof(double));       /* & quat */
    memcpy( avptr, lclimgfrm->_av, 3 * sizeof(double));               /* & av */

    sclkptr++;
    quatptr += 4;
    avptr += 3;
    nRec++;

    /* next frame, ensure end of loop even if last ->nextif is not null */
    lclimgfrm = (lclimgfrm->nextif==imgfrm) ?(IMGFRM *)NULL : lclimgfrm->nextif;
  }

  WRITESEG            /* flush segment */

  if ( nSeg) fprintf( stderr, " done, %ld segments written\n", nSeg);
  return;
}

/**********************************************************************/
/* free a single image frame structure */

void
freeImageFrame( IMGFRM *imgfrm) {
  if ( !imgfrm) return;
  if ( imgfrm->imgfrmAlloced) free( imgfrm);
  return;
}

/**********************************************************************/
/* free a linked list of image frame structures
 * - assumes last item in list has ->nextif of NULL or points to first item
 */

void
free_imgfrm_list( IMGFRM **imgfrm) {
IMGFRM *lclimgfrm = *imgfrm;
IMGFRM *nextimgfrm;

  while ( lclimgfrm ) {
    nextimgfrm = lclimgfrm->nextif;
    freeImageFrame( lclimgfrm);
    lclimgfrm = nextimgfrm;
    if ( lclimgfrm == *imgfrm) lclimgfrm = NULL;
  }
  *imgfrm = 0;
  return;
}

/***********************************************************************
 * cleanup linked list of OTHERBODIES
 * - leaves (OTHERBODIES *) 0 in pointed-to argument
 */
void
orbit_cleanupOtherBodies( OTHERBODIES **ppob)
{
OTHERBODIES *pTmp;
  if ( !ppob) return;
  while ( *ppob)
  {
    pTmp = (*ppob)->next; /* save pointer to next item */
    free( *ppob);         /* free this item */
    *ppob = pTmp;         /* move on to next item */
  } /* while ( *ppob) */
}  /* orbit_cleanupOtherBodies( OTHERBODIES **ppob) */

/**********************************************************************/
/* Optionally create image frame structure */

IMGFRM *
allocImageFrame( fortint nCampts, IMGFRM *imgfrm) {
IMGFRM *lclimgfrm;

  if ( nCampts < 0) nCampts = 0;                              /* just in case */

  /* allocate image frame & vec3 if required */

  if ( !imgfrm) {
    if ( !(lclimgfrm = (IMGFRM *) malloc( sizeof( IMGFRM) 
                                        + (sizeof(double)*3*nCampts) )) ) {
      fprintf( stderr, "Failed malloc (1) in allocImageFrame\n"),
      fflush( stderr);
      return( (IMGFRM *) 0);
    }
    lclimgfrm->vec3 = nCampts ? (double *) (lclimgfrm+1) 
                              : (double *) NULL;         /* hang vec3 on back */
    lclimgfrm->nextif = (IMGFRM *) 0;
    lclimgfrm->previf = (IMGFRM *) 0;
    lclimgfrm->_agen = (AGEN *) 0;
    lclimgfrm->imgfrmAlloced = 1;
    lclimgfrm->_nFov100Pts =
      lclimgfrm->_nFov100Plates =
                               0;
    lclimgfrm->_seqNum = -1;
    lclimgfrm->_nhits4 = 
      lclimgfrm->_nisPosition = 
      lclimgfrm->_nisScannum = 
      lclimgfrm->_nisSpecnum = 
      lclimgfrm->_nisDuration =
      lclimgfrm->_nisDarkFollows  = 0;
      lclimgfrm->_etDelta = 
      lclimgfrm->_Overlap = 
                            -999.0;
    lclimgfrm->_otherBodies = (OTHERBODIES *) 0;
  }
  else lclimgfrm = imgfrm;

  orbit_cleanupOtherBodies( &lclimgfrm->_otherBodies);

  return( lclimgfrm);
} /* allocImageFrame */

/*****************************************************/
/* find intersections of fov points with plate model */

void 
loadImageFrame_fov100( SC *sc, IMGFRM *imgfrm) {
VEC lclVec;
long i;
SPUDV *spudv;
void orbitgui_return_spudv_sc( SC *, SPUDV **);
double lclVecLen;
unsigned long plateIdx;

  imgfrm->_nFov100Pts = imgfrm->_nFov100Plates = 0;
  if ( !sc->_doFov100) return;

  imgfrm->_nFov100Pts = *nFov100Pts;
  orbitgui_return_spudv_sc( sc, &spudv);

  for ( imgfrm->_nFov100Plates=i=0; i<*nFov100Pts; ++i) {
    vxm( fov100Pts+(i*3), msctoabf, lclVec);
    spudf_intersect( spudv->_spudf, sc->_scFromAstAbf, lclVec
                   , &lclVecLen, &plateIdx);

    /* save fov point (abf) & plate index */
    if ( plateIdx < spudv->_spudf->nface) {
      vmxpb( lclVecLen, lclVec, sc->_scFromAstAbf
           , imgfrm->_fov100Pts[imgfrm->_nFov100Plates]);
      imgfrm->_fov100Plates[imgfrm->_nFov100Plates++] = plateIdx;
    }
  }
  return;
} /* locaImageFrame_fov100 */

/***********************************************************************
 * return linked list of OTHERBODIES with geometry at sc->_et
 * if (OTHERBODIES **)cleanupPPOb is non-null, clean it up
 */
OTHERBODIES *
orbit_otherBodies( SC *sc, double *pEt, OTHERBODIES **cleanupPPOb)
{
OTHERBODIES *newObPtr = 0; /* pointer to newly malloc'ed (OTHERBODIES *) */
OTHERBODIES *rtnPtr = 0; /* returned pointer to first item in the list */
OTHERBODIES **nextPP = &rtnPtr; /* where to store the next pointer */
SPICEOPTS **lclppso = &spiceOpts;
SPICEOPTS *lclpso;
int skip = 0; /* skipCurrent, third arg to orbit_matchSpiceOpt() */

  orbit_cleanupOtherBodies( cleanupPPOb);

  if ( !lclppso) return (OTHERBODIES *) 0;

  while ( (lclppso=orbit_matchSpiceOpt( lclppso, SPICEOPTS_OTHERBODYID, skip)) )
  {
  VEC6 scToBodyJ2k;
  MTX mBodyToJ2k, mJ2kToBody;
  fortint success, besthope;

    skip = 1;   /* skip this (SPICEOPTS *) *lclppso next time */

    lclpso = *lclppso;

    ospice_spkezj2k( &lclpso->_handle, pEt, &scid
                   , scToBodyJ2k, mBodyToJ2k
                   , &success, &besthope);

    if ( success == besthope)
    {
      if ( !( newObPtr=(OTHERBODIES *)malloc( sizeof(OTHERBODIES) +
                                           strlen( lclpso->_absPathName) + 1)) )
      {
        fprintf(stderr, "WARNING:  Could not allocate OTHERBODIES structure\n");
        continue;
      }

      newObPtr->_bodyId = lclpso->_handle;
      newObPtr->_bodyName = (char *)(newObPtr + 1);
      strcpy( newObPtr->_bodyName, lclpso->_absPathName);

      CPYVEC( scToBodyJ2k, newObPtr->_scToBodyJ2k);

      vxm( scToBodyJ2k, sc->_mj2ktosc, newObPtr->_scToBodySbf);

      MT2( mBodyToJ2k, mJ2kToBody);

      vxm( scToBodyJ2k, mJ2kToBody, newObPtr->_bodyToScBf);
      VNEG( newObPtr->_bodyToScBf);

      ACOSDDOT( newObPtr->_angleBodyToBore, newObPtr->_scToBodySbf, boresight);

      m2q( mBodyToJ2k, newObPtr->_bodyQuat); /* SPICE:  J2k => Body */

      newObPtr->next = 0;

      *nextPP = newObPtr;
      nextPP = &newObPtr->next;
    }  /* if ( success == besthope) */
  }  /* while ( (lclppso=orbit_matchSpiceOpt(... */

  return rtnPtr;

} /* orbit_otherBodies( SC *sc) */

/**********************************************************************/
/* Optionally create, then load image frame structure
 * Special Case:  JJ Mosaic mode
 *                - resultant frame will be in a plane perpendicular 
 *                  to s/c vector and will be between ellipsoid & s/c
 */

IMGFRM *
loadImageFrame_JJMosaic( SC *sc, double etIn, IMGFRM *imgfrm) {
IMGFRM *lclimgfrm;
double xlen;
double *surfPt, *v0, *v1, *v2;
double *scVec = sc->_scFromAstAbf;
VEC lclvec, usun, usc, boreAbf, v0to1, v1to2;
double framerad;
double *usurfnm;
double xxx;
int i, i1, i2, i3;
VEC pVec, pMscVec;  /* Point in plane of frame, (pVec - scVec) */
double pmscdotn;  /* (P-SC) dot Normal */
double udotn;     /* UnitVector dot Normal */

  if ( !(lclimgfrm = allocImageFrame( ncampts, imgfrm)) ) return( (IMGFRM *) 0);

  /* sc vector is normal to image frame
   * place frame at maximum radius of ellipsoid from center of model
   * define point in the plane of that frame, calculate dot product
   * to use for finding intersection of vectors with that plane
   */

  vhat( scVec, lclimgfrm->_normVec);
  vmxpb( maxrad, lclimgfrm->_normVec, nullVec, pVec);
  VMINUS2( pVec, scVec, pMscVec);
  pmscdotn = VDOT( pMscVec, lclimgfrm->_normVec);

  /* isn't pmscdotn = maxrad - VLEN(scVec) easier? */

  /* boresight intersection WITH PLANE */

  vxm( boresight, msctoabf, boreAbf);
  udotn = VDOT( boreAbf, lclimgfrm->_normVec);
  if ( udotn != 0.0) xlen = pmscdotn / udotn;
  else xlen = 0.0;

  if ( xlen < 0.0) xlen = -xlen;                   /* ensure xlen is positive */
  else if ( xlen == 0.0) xlen = maxrad;

  vmxpb( xlen, boreAbf, scVec, lclimgfrm->_boreVec);

  /* other camera vector intersections WITH PLANE */

  for ( i=0, v0=camabf, v1=lclimgfrm->vec3; i<ncampts; i++, v0 += 3, v1 += 3) {
    udotn = VDOT( v0, lclimgfrm->_normVec);
    if ( udotn != 0.0) xlen = pmscdotn / udotn;
    else xlen = 0.0;
    vmxpb( xlen, v0, scVec, v1);
  } /* for i<ncampts */

  /* fill in other vectors */

  lclimgfrm->_ncampts = ncampts;
  lclimgfrm->_nClosedPts = *nClosedPts;

  CPYVEC( scVec, lclimgfrm->_boretoscVec);
  VMINUS2( scVec, lclimgfrm->_boreVec, lclimgfrm->_boretoscVec);
  VMINUS2( sc->_sunFromAstAbf, lclimgfrm->_boreVec, lclimgfrm->_boretosunVec);

  vxm( lclimgfrm->_boretoscVec, mabftoj2k, lclimgfrm->_boresightJ2k);
  vmxpb( -1.0, lclimgfrm->_boresightJ2k, nullVec, lclimgfrm->_boresightJ2k);
  vxm( sc->_sunFromScAbf, mabftoj2k, lclimgfrm->_sunFromScJ2k);

  vxm( sc->_panel, sc->_msctoj2k, lclimgfrm->_panelJ2k);
  vxm( sc->_earthFromScAbf, sc->_mabftosc, lclimgfrm->_earthFromScSC);
  vxm( sc->_scFromAstAbf, sc->_mabftosc, lclimgfrm->_astFromScSC);
  VNEG( lclimgfrm->_astFromScSC);

#define QUATNSCLK \
  m2q( sc->_mabftoj2k, lclimgfrm->_abfQuat); /* SPICE:  J2k => ABF */ \
  m2q( sc->_msctoj2k, lclimgfrm->_scQuat);   /* SPICE:  J2k => S/C */ \
  lclimgfrm->_av[0] = lclimgfrm->_av[1] = lclimgfrm->_av[2] = 0.0; \
  lclimgfrm->_scid = scid; \
  if ( !failedonce) { \
    sce2t( &scid, &etIn, &lclimgfrm->_sclk); \
    if ( failed()) { \
      reset(); \
      failedonce = 1; \
      lclimgfrm->_sclk = 0.0; \
    } \
  } else lclimgfrm->_sclk = 0.0

  QUATNSCLK;    /* quaternion & sclk */
  
  /* fill in et and orbit fraction, if applicable */

  lclimgfrm->_et = etIn;

  if ( scEph == ORB_USESO) {
    xxx = pow( scSo[SO_RP] / ( 1.0 - scSo[SO_ECC]), 3.0); /* mean radius^3 */
    xxx = 360.0 / r2d( sqrt( scSo[SO_MU] / xxx));         /* orbit period, s */
    lclimgfrm->_orbitnum = (etIn - scSo[SO_T0]) / xxx;
  } else {
    lclimgfrm->_orbitnum = -999.0;
  }

  /* - unit normal to frame - use first two sides, assume clockwise order */

  /* get photmetric info (meaningless here, but fill it in anyway */

  usurfnm = lclimgfrm->_p5normVec;
  surfnm( asterad, asterad+1, asterad+2, lclimgfrm->_boreVec, usurfnm);
  vhat( lclimgfrm->_boretosunVec, usun);
  vhat( lclimgfrm->_boretoscVec, usc);
  ACOSDDOTU( lclimgfrm->_incid, usurfnm, usun);
  ACOSDDOTU( lclimgfrm->_emiss, usurfnm, usc);
  ACOSDDOTU( lclimgfrm->_phase, usun, usc);

  loadImageFrame_fov100( sc, lclimgfrm);

  lclimgfrm->_otherBodies = 
    orbit_otherBodies( sc, &lclimgfrm->_et, &lclimgfrm->_otherBodies);

  return( lclimgfrm);
} /* loadImageFrame_JJMosaic */

/**********************************************************************/
/* Optionally create, then load image frame structure
 * Special Case:  JJ Orbit mode
 *                - resultant frame will be in a plane parallel to 
 *                  the ellipsoid surface at one of the points of
 *                  intersection of the camera vectors 
 */


IMGFRM *
loadImageFrame_JJOrbit( SC *sc, double etIn, IMGFRM *imgfrm) {
IMGFRM *lclimgfrm;
double xlen;
static fortint nptsAlloc = 0, *hitsEllipse, borehits, hits;
double l[2];
double *surfPt, *v0, *v1, *v2;
double *scVec = sc->_scFromAstAbf;
double *fromSC;
double *usurfnm;
double xxx;
VEC lclvec, usun, usc, boreAbf, v0to1, v1to2;
int i, i1, i2, i3;
double avglen, borelen, sclen;
double pmscdotn, udotn;

  if ( !(lclimgfrm = allocImageFrame( ncampts, imgfrm)) ) return( (IMGFRM *) 0);

  /* allocate structure to hold which points hit the ellipsoid */

  if ( nptsAlloc < ncampts) {

    if ( nptsAlloc && hitsEllipse) free( hitsEllipse);
    nptsAlloc = ncampts;
    if ( !(hitsEllipse = (fortint *) malloc( nptsAlloc * sizeof( fortint))) ) {
      nptsAlloc = 0;
      if ( !imgfrm) {
        freeImageFrame( lclimgfrm);
      }
      return( (IMGFRM *) 0);
    }
  }

  /* test boresight intersection with ellipsoid */

  vxm( boresight, msctoabf, boreAbf);
  borehits = 0;
  ospice_ellint( asterad, scVec, boreAbf, l, &hits);
  fromSC = boreAbf;

  /* if boresight does not intersect, test other camera vectors */

  for ( i=0; i<ncampts && !hits; i++) {
    fromSC = camabf+(3*i);
    ospice_ellint( asterad, scVec, fromSC, l, &hits);
  } /* for i<ncampts */

  /* if we have a hit, find the surface normal and make it the normal to the
   * camera frame plane
   */

  if ( hits) {
    vmxpb( l[0], fromSC, scVec, lclvec);
    surfnm( asterad, asterad+1, asterad+2, lclvec, lclimgfrm->_normVec);

  /* if no hits, use sc vector, extend it to max radius of ellipsoid */

  } else {
    vhat( scVec, lclimgfrm->_normVec);
    l[0] = maxrad - VDOT(scVec, lclimgfrm->_normVec);  /* reversed sign */
    vmxpb( l[0], lclimgfrm->_normVec, scVec, lclvec);
  }

  pmscdotn = VDOT(lclvec,lclimgfrm->_normVec) - VDOT(scVec,lclimgfrm->_normVec);

  /* at this point, lclvec is a point in the plane of the frame and
   * normVec is the normal to that plane.  wrt the ellipsoid,
   * lclvec is a point on the surface of the ellipsoid and
   * normVec is the surface normal at that point, with the exception that
   * if none of the camera frame vectors intersected the ellipsoid, then
   * lclvec is a point maxrad of the way from the ellipsoid center to the 
   * sc, and normVec is the unit sc vector
   */

  /* for all camera frame vectors, calculate their intersection with the
   * lclvec/normVec plane
   */

  /* boresight intersection WITH PLANE */

  vxm( boresight, msctoabf, boreAbf);
  udotn = VDOT( boreAbf, lclimgfrm->_normVec);
  if ( udotn != 0.0) xlen = pmscdotn / udotn;
  else xlen = 0.0;

  if ( xlen < 0.0) xlen = -xlen;                   /* ensure xlen is positive */
  else if ( xlen == 0.0) xlen = maxrad;

  vmxpb( xlen, boreAbf, scVec, lclimgfrm->_boreVec);

  /* other camera vector intersections WITH PLANE */

  for ( i=0, v0=camabf, v1=lclimgfrm->vec3; i<ncampts; i++, v0 += 3, v1 += 3) {
    udotn = VDOT( v0, lclimgfrm->_normVec);
    if ( udotn != 0.0) xlen = pmscdotn / udotn;
    else xlen = 0.0;
    vmxpb( xlen, v0, scVec, v1);
  } /* for i<ncampts */

  /* fill in other vectors */

  lclimgfrm->_ncampts = ncampts;
  lclimgfrm->_nClosedPts = *nClosedPts;

  CPYVEC( scVec, lclimgfrm->_boretoscVec);
  VMINUS2( scVec, lclimgfrm->_boreVec, lclimgfrm->_boretoscVec);
  VMINUS2( sc->_sunFromAstAbf, lclimgfrm->_boreVec, lclimgfrm->_boretosunVec);

  vxm( lclimgfrm->_boretoscVec, mabftoj2k, lclimgfrm->_boresightJ2k);
  vmxpb( -1.0, lclimgfrm->_boresightJ2k, nullVec, lclimgfrm->_boresightJ2k);
  vxm( sc->_sunFromScAbf, mabftoj2k, lclimgfrm->_sunFromScJ2k);

  vxm( sc->_panel, sc->_msctoj2k, lclimgfrm->_panelJ2k);
  vxm( sc->_earthFromScAbf, sc->_mabftosc, lclimgfrm->_earthFromScSC);
  vxm( sc->_scFromAstAbf, sc->_mabftosc, lclimgfrm->_astFromScSC);
  VNEG( lclimgfrm->_astFromScSC);

  QUATNSCLK;    /* quaternion & sclk */

  /* fill in et and orbit fraction, if applicable */

  lclimgfrm->_et = etIn;

  if ( scEph == ORB_USESO) {
    xxx = pow( scSo[SO_RP] / ( 1.0 - scSo[SO_ECC]), 3.0); /* mean radius^3 */
    xxx = 360.0 / r2d( sqrt( scSo[SO_MU] / xxx));         /* orbit period, s */
    lclimgfrm->_orbitnum = (etIn - scSo[SO_T0]) / xxx;
  } else {
    lclimgfrm->_orbitnum = -999.0;
  }

  /* - unit normal to frame - use first two sides, assume clockwise order */

  /* get photmetric info (meaningless here, but fill it in anyway */

  usurfnm = lclimgfrm->_p5normVec;
  surfnm( asterad, asterad+1, asterad+2, lclimgfrm->_boreVec, usurfnm);
  vhat( lclimgfrm->_boretosunVec, usun);
  vhat( lclimgfrm->_boretoscVec, usc);
  ACOSDDOTU( lclimgfrm->_incid, usurfnm, usun);
  ACOSDDOTU( lclimgfrm->_emiss, usurfnm, usc);
  ACOSDDOTU( lclimgfrm->_phase, usun, usc);

  loadImageFrame_fov100( sc, lclimgfrm);

  lclimgfrm->_otherBodies = 
    orbit_otherBodies( sc, &lclimgfrm->_et, &lclimgfrm->_otherBodies);

  return( lclimgfrm);
} /* IMGFRM *loadImageFrame_JJOrbit( SC *sc, double etIn, IMGFRM *imgfrm) */

/**********************************************************************/
/* Optionally create, then load image frame structure */

IMGFRM *
loadImageFrame( SC *sc, double etIn, IMGFRM *imgfrm) {
IMGFRM *lclimgfrm;
static fortint nptsAlloc = 0, *hitsEllipse, borehits, hits;
unsigned long intplate;
double l[2];
double *surfPt, *v0, *v1, *v2;
double *scVec = sc->_scFromAstAbf;
int max4;
VEC lclvec, usun, usc, boreAbf, v0to1, v1to2;
double *usurfnm;
double xxx, xxx1, xxx2, xxx3;
int i, i1, i2, i3;
double avglen, borelen, sclen;
SPUDV *spudv;
void orbitgui_return_spudv_sc( SC *, SPUDV **);

  if ( jjmode == JJMODE_MOSAIC)
    return( loadImageFrame_JJMosaic( sc, etIn, imgfrm));
  if ( jjmode == JJMODE_ORBIT)
    return( loadImageFrame_JJOrbit( sc, etIn, imgfrm));

  if ( !(lclimgfrm = allocImageFrame( ncampts, imgfrm)) ) return( (IMGFRM *) 0);

  orbitgui_return_spudv_sc( sc, &spudv);

  /* allocate structure to hold which points hit the ellipsoid */

  if ( nptsAlloc < ncampts) {

    if ( nptsAlloc && hitsEllipse) free( hitsEllipse);
    nptsAlloc = ncampts;
    if ( !(hitsEllipse = (fortint *) malloc( nptsAlloc * sizeof( fortint))) ) {
      nptsAlloc = 0;
      if ( !imgfrm) {
        freeImageFrame( lclimgfrm);
      }
      return( (IMGFRM *) 0);
    }
  }

  /* find length to sc vector & boresight intersections with ellipsoid */
  VNEG2( scVec, lclvec);
  vhat( lclvec, lclvec);

  /* 20010129:  intersect spud model, not ellipse any more */
  spudf_intersect( spudv->_spudf, scVec, lclvec, l, &intplate);
  hits = (intplate < spudv->_spudf->nface) ? 1 : 0;
  sclen = hits ? l[0] : VLEN(scVec);

  /* 20010129 old: ospice_ellint( asterad, scVec, lclvec, l, &hits); */
  /* 20010129 old: sclen = l[0]; /* scVec MUST intersect ellipsoid */

  vxm( boresight, msctoabf, boreAbf);

  /* 20010129:  intersect spud model, not ellipse any more */
  spudf_intersect(spudv->_spudf, scVec, boreAbf, l, &intplate);
  borehits = ( intplate < spudv->_spudf->nface) ? 1 : 0;

  /* 20010129 old: borehits = 0; */
  /* 20010129 old: ospice_ellint( asterad, scVec, boreAbf, l, &borehits); */

  if ( borehits) borelen = l[0];

  /* find which vectors hit the ellipsoid - make note of 1st 4 or fewer hits */

  max4 = (ncampts<4) ? ncampts : 4;
  avglen = 0.0;

  for ( lclimgfrm->_nhits4 = i=0; i<ncampts; i++) {

    /* 20010129:  intersect spud model, not ellipse any more */
    spudf_intersect( spudv->_spudf, scVec, camabf+(3*i), l, &intplate);
    hitsEllipse[i] = (intplate < spudv->_spudf->nface) ? 1 : 0;

    /* 20010129 old: */
    /* ospice_ellint( asterad, scVec, camabf+(3*i), l, hitsEllipse+i); */

    if ( hitsEllipse[i]) {
      hitsEllipse[i] = 1;  /* ignore second solution */
      if( i<max4) {
        lclimgfrm->_nhits4++;
        avglen += l[0];
      }
      surfPt = lclimgfrm->vec3 + (3 * i);
      vmxpb( l[0], camabf+(3*i), scVec, surfPt);
    }
  } /* for i<ncampts */

  /* calculate average length to ellipsoid of first 4 or fewer
   * - use range if no hits
   */

  if ( lclimgfrm->_nhits4) { 
    avglen /= (double) lclimgfrm->_nhits4; 
    if ( !borehits) borelen = avglen;
  } else {
    if ( borehits) { avglen = borelen; }
    else { avglen = borelen = sclen; }
  }

  /* fill in misses in first 4 or fewer 
   * - if max4<4 || nhits4==1 or 0, use avglen ( = range if nhits4==0)
   * - if ncampts = 4, try to reconstruct rectangle
   */

  if ( lclimgfrm->_nhits4<max4) {
    if (max4<4 || lclimgfrm->_nhits4<2) for ( i=0; i<max4; i++) {
      if ( !hitsEllipse[i]) {
        surfPt = lclimgfrm->vec3 + (3 * i);
        vmxpb( avglen, camabf+(3*i), scVec, surfPt);
      }
    }

    /* 4 corners: assume square for 2 hits, rectangle for 3 */

    else {
      switch (lclimgfrm->_nhits4) {

      /* 2 hits, avglen not affected */
      case 2:

        /* 2 hits:  opposite corners => use avglen for other corners */

        if ( (hitsEllipse[0] && hitsEllipse[2]) || 
             (hitsEllipse[1] && hitsEllipse[3])) {
          for ( i=0; i<max4; i++) if ( !hitsEllipse[i]) {
            surfPt = lclimgfrm->vec3 + (3 * i);
            vmxpb( avglen, camabf+(3*i), scVec, surfPt);
          }

        /* 2 hits:  adjacent corners => make opposite side parallel */

        } else {

        /* - find adjacent corner hits */

        int hit1, hit2, missby3;
          for ( hit1=hit2= i=0; !(hit1 && hit2) && i<5; i++) {
            hit1 = hit2;
            hit2 = hitsEllipse[i%4];
          }

          /* - first missing corner is after second hit */

          missby3 = 3 * (i % 4); /* "3 *" => offset to vector */

          i = (missby3+9) % 12;
          VMINUS2( scVec, camabf+i, lclvec);
          xxx = vlen( lclvec);

          surfPt = lclimgfrm->vec3 + missby3;
          vmxpb( xxx, camabf+missby3, scVec, surfPt); 

          /* - second missing corner is after first */

          missby3 = (missby3+3) % 12;

          i = (missby3+3) % 12;
          VMINUS2( scVec, camabf+i, lclvec);
          xxx = vlen( lclvec);

          surfPt = lclimgfrm->vec3 + missby3;
          vmxpb( xxx, camabf+missby3, scVec, surfPt); 
        }
        break;
        /* end 2 hits */

      /* 3 hits, avglen will be changed */

      case 3: /* switch lclimgfrm->_nhits4 */

        /* - find missing corner, label succeeding corners 1, 2, 3 */

        for ( i=0; hitsEllipse[i]; i++) ;
        i *= 3;
        i1 = (i+3) %12;
        i2 = (i+6) %12;
        i3 = (i+9) %12;

        /* - find lengths to other corners */

        VMINUS2( scVec, lclimgfrm->vec3+i1, lclvec); xxx1 = vlen( lclvec);
        VMINUS2( scVec, lclimgfrm->vec3+i2, lclvec); xxx2 = vlen( lclvec);
        /* VMINUS2( scVec, lclimgfrm->vec3+i2, lclvec); xxx3 = vlen( lclvec); */
        VMINUS2( scVec, lclimgfrm->vec3+i3, lclvec); xxx3 = vlen( lclvec);

        /* - bilinear extrapolation to get missing length */

        xxx = xxx1 + (xxx3 - xxx2);
        vmxpb( xxx, camabf+i, scVec, lclimgfrm->vec3+i); 

        /* - update avglen */
        avglen = ((avglen * 3.0) + xxx) / 4.0;
        break;

      default: /* we should not be able to get here */
        fprintf( stderr
        , "Program error, contact programmer:  code WSNBATGH-loadImageFrame\n"
        );
        fprintf( stderr, "nhits4=%d max4=%d ncampts=%ld\n"
                       , lclimgfrm->_nhits4, max4, ncampts);
        fflush( stderr);
        if ( !imgfrm) {
          freeImageFrame( lclimgfrm);
        }
        return( (IMGFRM *) 0);
      } /* switch lclimgfrm->_nhits4 */
    } /* if max4<4 || lclimgfrm->_nhits4<2 */
  } /* if lclimgfrm->_nhits4 < max4 */

  /* fill in misses after first 4 */

  for ( i=max4; i<ncampts; i++) if ( !hitsEllipse[i]) 
  {
    surfPt = lclimgfrm->vec3 + (3 * i);
    vmxpb( avglen, camabf+(3*i), scVec, surfPt); 
  }

  /* fill in other vectors */

  lclimgfrm->_ncampts = ncampts;
  lclimgfrm->_nClosedPts = *nClosedPts;

  CPYVEC( scVec, lclimgfrm->_boretoscVec);

  if ( borelen < 0.0) borelen = -borelen;       /* ensure borelen is positive */
  else if ( borelen == 0.0) borelen = maxrad;

  vmxpb( borelen, boreAbf, scVec, lclimgfrm->_boreVec);
  VMINUS2( scVec, lclimgfrm->_boreVec, lclimgfrm->_boretoscVec);
  VMINUS2( sc->_sunFromAstAbf, lclimgfrm->_boreVec, lclimgfrm->_boretosunVec);

  vxm( lclimgfrm->_boretoscVec, mabftoj2k, lclimgfrm->_boresightJ2k);
  vmxpb( -1.0, lclimgfrm->_boresightJ2k, nullVec, lclimgfrm->_boresightJ2k);
  vxm( sc->_sunFromScAbf, mabftoj2k, lclimgfrm->_sunFromScJ2k);

  vxm( sc->_panel, sc->_msctoj2k, lclimgfrm->_panelJ2k);
  vxm( sc->_earthFromScAbf, sc->_mabftosc, lclimgfrm->_earthFromScSC);
  vxm( sc->_scFromAstAbf, sc->_mabftosc, lclimgfrm->_astFromScSC);
  VNEG( lclimgfrm->_astFromScSC);

  QUATNSCLK;    /* quaternion & sclk */

  /* fill in et and orbit fraction, if applicable */

  lclimgfrm->_et = etIn;

  if ( scEph == ORB_USESO) {
    xxx = pow( scSo[SO_RP] / ( 1.0 - scSo[SO_ECC]), 3.0); /* mean radius^3 */
    xxx = 360.0 / r2d( sqrt( scSo[SO_MU] / xxx));         /* orbit period, s */
    lclimgfrm->_orbitnum = (etIn - scSo[SO_T0]) / xxx;
  } else {
    lclimgfrm->_orbitnum = -999.0;
  }

  /* - unit normal to frame - use first two sides, assume clockwise order */

  v0 = lclimgfrm->vec3;
  v1 = lclimgfrm->vec3 + 3;
  v2 = lclimgfrm->vec3 + 6;
  VMINUS2( v0, v1, v0to1);
  VMINUS2( v1, v2, v1to2);
  ucrss( v1to2, v0to1, lclimgfrm->_normVec);

  /* get photometric info */

  usurfnm = lclimgfrm->_p5normVec;
  surfnm( asterad, asterad+1, asterad+2, lclimgfrm->_boreVec, usurfnm);
  vhat( lclimgfrm->_boretosunVec, usun);
  vhat( lclimgfrm->_boretoscVec, usc);
  ACOSDDOTU( lclimgfrm->_incid, usurfnm, usun);
  ACOSDDOTU( lclimgfrm->_emiss, usurfnm, usc);
  ACOSDDOTU( lclimgfrm->_phase, usun, usc);

  loadImageFrame_fov100( sc, lclimgfrm);

  lclimgfrm->_otherBodies = 
    orbit_otherBodies( sc, &lclimgfrm->_et, &lclimgfrm->_otherBodies);

  return( lclimgfrm);
} /* IMGFRM *loadImageFrame( SC *sc, double etIn, IMGFRM *imgfrm) */

/****************************************************/
/* time conversion - tparse for tdt, utc2et for utc */

void 
orbit_tparse( char *tdt, double *et) {
fortint lentdt = strlen( tdt);

  ospice_tparse( et, &lentdt, tdt);
  return;
}

/****************************************************************/
void 
orbit_sclkch2et( fortint scidarg, char *sclkch, double *et) {
fortint lensclkch = strlen( sclkch);

  ospice_sclkch2et( &scidarg, et, &lensclkch, sclkch);
  return;
}

/****************************************************************/
void 
orbit_scencd( fortint scidarg, char *sclkch, double *sclkdp) {
fortint lensclkch = strlen( sclkch);

  ospice_scencd( &scidarg, sclkdp, &lensclkch, sclkch);
  return;
}

/*******************************************************/
void 
orbit_conics( double elts, double *et, VEC6 state) {

  conics( elts, et, state);
  return;
}

/************************************************************************/
/* get matrix that converts to J2000 from either 
 * - another reference frame 
 *  OR 
 * - a planetary body's (id>0) frame
 */

void 
orbit_getmtx( fortint id, char *refrm, double *et, MTX toj2k) {
fortint lenrefrm = (refrm) ? strlen( refrm) : 0;

  if ( id) {

    /* load optional PCK's */
    if ( id > 0 ) orbit_loadSpiceOpts( *et, SPICEOPTS_ORIBPCKO);
    ospice_getbodymtx( &id, et, toj2k);

  } else {
    if ( lenrefrm) {
      ospice_getrefrmmtx( toj2k, &lenrefrm, refrm);
    } else {
       toj2k[0] = toj2k[4] = toj2k[8] = 1.0;
       toj2k[1] = toj2k[2] = toj2k[3] = 
       toj2k[5] = toj2k[6] = toj2k[7] = 0.0;
    }
  }
  return;
}

/************************************************************************/
/* get angular velocities for linked list of IMGFRM's 
 * - end of linked list is either null ->nextif or ->nextif == starting IMGFRM
 */
void 
orbit_calc_imgfrmAv( IMGFRM *inpImgfrm) {
IMGFRM *lastif, *thisif, *nextif;

  /* return if there are less than two IMGFRM's in the list */

  if ( !inpImgfrm) return;
  if ( !inpImgfrm->nextif || inpImgfrm->nextif == inpImgfrm) return;

  /* loop through list */

  for ( lastif = thisif = inpImgfrm; thisif;
        thisif = (thisif->nextif != inpImgfrm) ? thisif->nextif : (IMGFRM *)0) {

    /* set nextif to be thisif->nextif except when thisif is last item in list
     * in which case thisif->nextif will either be null or be the initial IMGFRM
     */
    if ( thisif->nextif && (thisif->nextif != inpImgfrm))
      nextif = thisif->nextif;

    /* calculate the average velocities */

    ospice_qqt2av( nextif->_scQuat, lastif->_scQuat, &nextif->_et, &lastif->_et
                 , thisif->_av);

    /* lastif will trail thisif by one item in list after first time through */

    lastif = thisif;

  }
  return;
}
