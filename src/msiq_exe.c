/* Read lines from stdin, write files

   NEW INPUT FORMAT AS OF APPROX SEPTEMBER, 1999
                                                                           Files
Line (typical, # = numerics)                                   Description Types
============================================================   ==============  =
#,T,#,T,#,#,MSI ,Zcquisition,Sequence,#,sent,to,imager,#,...   MSI SEQ ACQ     0
#,T,#,T,#,#,MSI ,Automatic,exposure,image,#,taken,with,filter,#MSI Exposure    1
#,T,#,T,#,#,MSI ,Manual,exposure,image,#,taken,with,filter,#   MSI Exposure    1
#,T,#,T,#,#,MSI ,Quaterion:,#,#,#,#                            Quaternion      2
#,T,#,T,#,#,NIS ,RT,f#,NI_SHUTR_IN_OUT,BOTH_OUT                Set NIS Shutter -
#,T,#,T,#,#,NIS ,RT,f#,NI_SHUTR_IN_OUT,SLIT                    Set NIS Shutter -
#,T,#,T,#,-,Seq:,#,Pos:,#,Quat:,#,#,#,#                        NIS Obs + Quat  3
0 1 2 3 4 5 6666 7

T = #:#:#

Fields:
 0  Day of year
 1  HH:MM:SS
 2  Day of year
 3  HH:MM:SS
 4  kMET
 5  Number or dash

 6  MSI - Acquisition of Sequence sent to imager - 37 fields total
          - Field 7 = "Zcquisition" - so it sorts after Quaternion
          - Field 8 = "Sequence"
          - Field 9 = sequence #
          - Field 10 = "sent"
          - Field 11 = "to"
          - Field 12 = "imager"
          - Field 13 = # of images in sequence

    MSI - exposure - 15 fields total
          - Field 8 = "exposure"
          - Field 9 = "image"
          - Field 10 = image # in sequence
          - Field 11 = "taken"
          - Field 14 = filter #

    MSI - quaternion - 12 fields total
          - Field 7 = "Quaterion:"
          - Fields 8-11 = q0-q3

    NIS - Set shutter - 11 fields total
          - Field 9 = "NI_SHUTR_IN_OUT"
          - Field 10 = "SLIT" or "BOTH_OUT" - assume narrow if not BOTH_OUT

    Seq: - NIS observation + Quaternion - 15 fields total
           - Field 8 = "Pos:"
           - Field 9 = mirror position
           - Field 10 = "Quat:";
           - Fields 11-14 = q0-q3

File types:

  0       YYDDD.msi.seq
  1       YYDDD.msi.met
  2       YYDDD.bc
  3       YYDDD.nis.met & YYDDD.bc

 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define MAXSTRSIZE 1024

/*****************************************************************/

typedef struct {
  char *_listFn;                 /* name of files with data filenames in them */
  FILE *_listFile;                         /* (FILE *) of _listFn when opened */
  char *_dataFn;      /* name of data file, including any prefix from _listFn */
  char *_postPfx;     /* first character of _dataFn after prefix from _listFn */
  FILE *_dataFile;                          /* (FILE *) of dataFn when opened */
  int _newData;
} FILEFILE;

#define listFile ff->_listFile
#define dataFile ff->_dataFile
#define newData ff->_newData

/********************************************/

void *
FFclose( FILEFILE *ff) {
  if ( !ff) return;
  if ( listFile) { fclose( listFile); listFile = (FILE *) NULL; }
  if ( dataFile) { fclose( dataFile); dataFile = (FILE *) NULL; }
  ff->_newData = 0;
  return;
}

/********************************************/

void *
FFfree( FILEFILE **ff) {
  if ( !ff) return;
  if ( !*ff) return;
  FFclose( *ff);
  free( *ff);
  *ff = (FILEFILE *) NULL;
  return;
}

/********************************************/

FILEFILE *
FFinit( char *inputListFn) {
FILEFILE *ff = (FILEFILE *) NULL;
size_t lFnLen;
#define FFINITERRRTN(MSG) \
  if (MSG) { fprintf( stderr, "%s\n", MSG); fflush(stderr); } \
  if ( ff) free( ff); \
  return (FILEFILE *) NULL

  if ( !inputListFn) { FFINITERRRTN( "***FFinit - Program error 0"); }
  if ( !*inputListFn) { FFINITERRRTN( "***FFinit - Program error 1"); }

  lFnLen = strlen(inputListFn) + 1;      /* room for inputListFn + terminator */

  /* malloc space for (FILEFILE) *ff + * inputListFn string +
   *   dataFn string (up to MAXSTRSIZE longer than inputListFn)
   */
  ff = (FILEFILE *) malloc(sizeof(FILEFILE) + lFnLen + lFnLen+MAXSTRSIZE);
  if (!ff) {
    FFINITERRRTN( "***FFinit - malloc failure");
  }

  /* set list file name pointer to area after ff, copy inputListFn to there
   * - use sscanf to trim spaces & do a validity check
   */
  if ( 1 != sscanf( inputListFn, "%s", ff->_listFn=(char *)(ff+1)) ) {
    FFINITERRRTN( (char *) NULL);
  }

  newData = 0;/* so it is not <0 which means we've been through all data files*/

# if 0           /* don't open here, let FFgets open listFile so we can reset */
  /* open list file, return on error */
  if ( !(listFile=fopen(ff->_listFn,"r") ) {
    FFINITERRRTN( (char *) NULL);
  }
# endif

  /* set data file name pointer to area after list file name, 
   * ***N.B. assume data file names in list file are RELATIVE to directory
   *         that contains list fil
   *         - must save list file directory prefix
   *         - must copy RELATIVE data file names (i.e. that don't start with 
   *           a '/') AFTER that prefix
   * - copy list file name to data file name
   */
  strncpy( ff->_dataFn=(ff->_listFn+lFnLen), inputListFn, lFnLen);

  /* - set postPfx pointer to data file name position after last '/' in
   *   inputListFn 
   * OR, if there is no '/' in inputListFn, 
   * - set it to start of data file name
   *   ***N.B. relative pathnames will be copied to postPfx;
   *           absolute pathnames will be copied to dataFn
   */
  if ( (ff->_postPfx=strrchr(ff->_dataFn,'/')) ) {
    ff->_postPfx++;                      /* prefix position is after last '/' */
  } else {
    ff->_postPfx = ff->_dataFn;         /* prefix position is start of dataFn */
  }

  dataFile = (FILE *) NULL;

  return (FILEFILE *) ff;
} /* FFinit */
  
/*********************************************************************/
/* read one line via fgets from the series of files whose filenames 
 * are in ff->_listFile
 * RETURNS same as fgets()
 */

char *
FFgets( char *s, int size, FILEFILE *ff, int recursing) {
char *ffgets;
char inpLine[MAXSTRSIZE];
char *cPtr;

  if ( !ff) return (char *) NULL;
  if ( newData < 0) return (char *) NULL;                    /* recursion end */

#  define FFGETS_RECURSE return FFgets( s, size, ff, recursing+1)
#  define FFGETS_DONE(NEWDATA) newData = (NEWDATA); return (char *) NULL

  /* ***N.B. after this point, newData cannot be < 0 unless set locally
   *         - setting newData < 0 implies there is nothing more to be 
   *           read from the list file or data files
   */

  if ( !recursing) newData = 0;    /* at 0th level, assume a dataFile is open */

  /* if no current data file, open next filename in listFile & call self 
   * recursively 
   * ***N.B. must FFGETS_RECURSE or FFGETS_DONE from this clause
   */
  if ( !dataFile) {
    if (!listFile) {
      if ( !(listFile=fopen(ff->_listFn,"r")) ) {            /* open listFile */
        FFGETS_DONE(-2);                              /* set failure & return */
      }
    }

    /* get one line from listFile */

    if ( !fgets( inpLine, MAXSTRSIZE, listFile)) {/* if EOF ... */
      fclose( listFile);                          /* - close filenames file */
      listFile = (FILE *) NULL;                   /* - stop infinite recursion*/
      FFGETS_DONE(-1);                            /* - set done and return */
    }
    if ( (cPtr=strchr( inpLine, '\n'))) *cPtr = '\0';     /* terminate string */

    /* sscanf data file name from inpLine to _dataFn
     * if _postPfx != _dataFn && filename is not absolute (doesn't start w/ '/')
     *   1) copy _listFn to _dataFn so prefix is there
     *   2) sscanf filename from inpLine again, but to _postPfx this time
     */
    if ( 1 == sscanf( inpLine, "%s", ff->_dataFn)) {
      if ( (ff->_postPfx != ff->_dataFn) && (*ff->_dataFn != '/')) {
      size_t pfxLen = ff->_postPfx - ff->_dataFn;
        strncpy( ff->_dataFn, ff->_listFn, pfxLen);
        sscanf( inpLine, "%s", ff->_postPfx);
      }
      /* have a new dataFn; open it, flag new data file ... */

      if ( (dataFile=fopen(ff->_dataFn,"r")) ) newData = 1;
    }
    /* ... and call this routine again with an increased recursion level
     * - ***N.B. if dataFile is still NULL (1!=sscanf() or NULL==fopen()),
     *           then this call will read next filename from listFile
     */
    FFGETS_RECURSE;
  }

  /* beyond here, there must be an open dataFile
   * - read next line of it into argument s
   * - if EOF
   *   - close it 
   *   - return with recursive call to this routine
   * - trim \n at end
   */
  if ( !(ffgets=fgets( s, size, dataFile))) {            /* if EOF ... */
    fclose( dataFile);                                   /* - close data file */
    dataFile = (FILE *) NULL;                            /* - null it out */
    FFGETS_RECURSE;
  }
  if ( (cPtr=strchr(inpLine,'\n')) ) *cPtr = '\0';       /* drop \n at end */

  return ffgets;                /* successfully read a line from a data file! */
} /* FFgets */

/****************************************************************/

typedef struct {
  int _type;
  int _nTok;
  char **_testChar;
  int *_testFld;
} LINEMATCH;

#define NTOK_MSIACQ 37
#define NTOK_MSIEXP 15
#define NTOK_MSIQUAT 12
#define NTOK_SETNISSHUT 11
#define NTOK_NISOBSQUAT 15

enum { 
  LINE_MSIACQ=0, LINE_MSIEXP, LINE_MSIQUAT
, LINE_SETNISSHUT, LINE_NISOBSQUAT
};

static char *testMSIAcqChar[] = { "MSI", "Zcquis\n", "Sequence", "sent", "to" };
static int testMSIAcqFld[]    = { 6,     7,          8,          10,     11,-1};
static char *testMSIExpChar[] = { "MSI", "exposure", "image", "taken" };
static int testMSIExpFld[]    = { 6,     8,          9,       11,  -1 };
static char *testMSIQuatChar[] = { "MSI", "Quater\n" };
static int testMSIQuatFld[]    = { 6,     7,      -1 };
static char *testNISShutChar[] = { "NIS", "NI_SHUTR_IN_OUT" };
static int testNISShutFld[]   =  { 6,     9,             -1 };
static char *testNISObsQuatChar[] = { "Seq:", "Pos:", "Quat:" };
static int testNISObsQuatFld[]   =  { 6,      8,      10,  -1 };

static LINEMATCH lineMatch[] = {
    { LINE_MSIACQ, NTOK_MSIACQ, testMSIAcqChar, testMSIAcqFld }
  , { LINE_MSIEXP, NTOK_MSIEXP, testMSIExpChar, testMSIExpFld }
  , { LINE_MSIQUAT, NTOK_MSIQUAT, testMSIQuatChar, testMSIQuatFld }
  , { LINE_SETNISSHUT, NTOK_SETNISSHUT, testNISShutChar, testNISShutFld }
  , { LINE_NISOBSQUAT, NTOK_NISOBSQUAT, testNISObsQuatChar, testNISObsQuatFld }
  , { -1, -1, (char **) 0, (int *) 0 }
};

/******************************************************************************/
/* match number of comma-separated tokens on a line and match selected tokens */

LINEMATCH *
matchLine( char *copyLine, char **tokPtr, LINEMATCH *lineMatch) {
int nTok;
LINEMATCH *lm;
int i, iFld;
int orbit_commaParse( char *, char**);

  nTok = orbit_commaParse( copyLine, tokPtr);

  for ( lm=lineMatch; lm->_testChar; ++lm) {

    if ( lm->_nTok == nTok) {                            /* match # of tokens */

      /* match tokens */
      for ( iFld=lm->_testFld[i=0]; iFld != -1; iFld=lm->_testFld[++i]) {

        if ( strcmp( tokPtr[iFld], lm->_testChar[i])) {           /* no match */
        int lenTok = strlen( lm->_testChar[i]);
  
          /* test partial field if last character of expected token = newline */

          if ( lm->_testChar[i][lenTok-1] == '\n') {
            if ( strncmp( tokPtr[iFld], lm->_testChar[i], lenTok-1)) break;
          } else break;
        }
      }
      if ( iFld == -1) return lm;          /* true if all tokens were matched */
  } }
  return (LINEMATCH *) 0;
} /* matchLine */

/*****************************************************************/
/* read frames' files' info */

typedef struct {
  long _type;
  char *_needle;
} NEEDLE;

/*****************************************************************/
/* find first member of array of NEEDLE structures that has 
 * its _needle string in a string
 * - returns NEEDLE struct that has _needle that matches
 */
NEEDLE *
matchNeedle( char *hayStack, NEEDLE *needles) {
  while ( needles->_needle) {
    if ( strstr( hayStack, needles->_needle)) return needles;
    ++needles;
  }
  return needles;
} /* matchNeedle */

/*****************************************************************/

#define BIGPXLDIM 161.0e-6

#define KM2DEGFMT " %8.2lf %7.3lf %6.2lf %5.2lf %5.2lf"

#define HMSLEN 9
typedef struct {
  int _doy;
  char _hms[HMSLEN*10];  /* HH:MM:SS */
  double _et, _met;
  double _rts, _res, _inc, _emi, _pha;
} FRAMESINFO;

enum { 
  FI_ALTET=0L   /* :alt et    alternate time */
, FI_SURFPHOT   /* :surf      surface normal & photometric angles */
, FI_BOREJ2K    /* :boreJ2k   boresight for range */
, FI_COUNT
};

#define FI_BITALL ((1L<<FI_COUNT)-1L)                /* assumes FI_COUNT < 31 */

static NEEDLE framesNeedles[] = {
  FI_ALTET,    ":alt et, sclk; UTC, MET-> "
, FI_SURFPHOT, ":surf norm"
, FI_BOREJ2K,  ":boreJ2k"
, -1,          (char *) 0
};

/******************************************************************************/
/* get next frame's info from frames file */

void
getNextFramesInfo( FILEFILE *ff, FRAMESINFO *fI) {
char *cPtr;
char framesFn[MAXSTRSIZE];
char inpLine[MAXSTRSIZE];
long bitsSoFar;
static FILE *lastBadFramesFile;

  /* macro to set bad values on initialization */

# define INITFI \
  fI->_doy = fI->_met = fI->_rts = fI->_res =  \
    fI->_inc = fI->_emi = fI->_pha = -1; \
  fI->_et = -1e300; \
  strncpy( fI->_hms, "????????", HMSLEN); fI->_hms[HMSLEN-1] = '\0'; \
  bitsSoFar = 0

# define BADFRAMESFILEMSG \
  if ( lastBadFramesFile != ff->_dataFile) { \
    fprintf( stderr, "*** Bad frames file\n"); \
    lastBadFramesFile = ff->_dataFile; \
  }

  INITFI;         /* assume failure */

  /* get _et, _met, _doy & _hms from ":alt et ..." line
   * get _inc, _emi & _pha from ":surf norm" line
   * get _rts from ":boreJ2k" line 
   */
  while ( bitsSoFar != FI_BITALL) {
  NEEDLE *needle;

    if ( !(cPtr=FFgets(inpLine,MAXSTRSIZE,ff,0))) {
      INITFI;
      break;
    }

    needle = matchNeedle( inpLine, framesNeedles);
    switch ( needle->_type) {
    double d;
    case FI_ALTET:
      if ( bitsSoFar) {
        BADFRAMESFILEMSG
      }
      INITFI;
      if ( 2 != sscanf( inpLine, "%lf %lf", &fI->_et, &fI->_met)) {
        BADFRAMESFILEMSG
        break;
      }
      /* find start of UTC string, 
       * read doy & hms assuming " YYYY-DOY[ T or // ]HH:MM:SS.ss"
       */
      cPtr = strstr( inpLine, needle->_needle) + strlen( needle->_needle);
      if ( 2 != sscanf( cPtr, "%*[ 0-9]%*[-]%ld%*[ /T]%[0-9:]"
                            , &fI->_doy, fI->_hms)) {
        BADFRAMESFILEMSG
        break;
      }
      bitsSoFar |= (1L << needle->_type);
      break;

    case FI_SURFPHOT:
      if ( 6 != sscanf( inpLine, "%lf %lf %lf %lf %lf %lf"
                               , &d, &d, &d, &fI->_inc, &fI->_emi, &fI->_pha)) {
        BADFRAMESFILEMSG
        break;
      }
      bitsSoFar |= (1 << needle->_type);
      break;

    case FI_BOREJ2K:
      if ( 4 != sscanf( inpLine, "%lf %lf %lf %lf"
                               , &d, &d, &d
                               , &fI->_rts)) {
        BADFRAMESFILEMSG
        break;
      }
      fI->_res = fI->_rts * BIGPXLDIM;
      bitsSoFar |= (1 << needle->_type);
      break;

    default:    /* a line we don't care about */
      break;
    }
    
  } /* while 1 */

  return;
} /* getNextFramesInfo */

/**********************************************************************/
/* find matching or greater met in frames file
 * - assumes getNextFramesInfo() will return increasing met
 * - returns 1 if getNextFramesInfo() returns matching met 
 *   - matching => within 1050 ticks of kmet*1000
 * - returns 0 if getNextFramesInfo() returns met > (kmet*1000) + 1050
 * - returns -N if kmetTok does not pass validity tests
 * - if listFile==NULL, will always return 0
 */
int 
matchFramesInfo( char *kmetTok, FILEFILE *ff, FRAMESINFO *fI) {
double kmet;

  if ( !ff) return 0;

  /* validate kmetTok input */

  if ( !kmetTok) return -1;
  if ( 1 != sscanf( kmetTok, "%lf", &kmet)) return -2;
  if ( kmet <= 0.0) return -3;

  /* valid kmet, look for matching frame */

  while ( (ff->_newData >= 0)  && (fI->_met < ((1000.0*kmet) - 500.0)) ) {
    getNextFramesInfo( ff, fI);
  }
  {
  double x;
    x = fabs(fI->_met - (1000.0*kmet));
  }
  kmet = fI->_met - (1000.0*kmet);
  /* 30.June, 2000 if ( (kmet<0?-kmet:kmet) < 1000.0 ) return 1; */
  if ( (kmet<0?-kmet:kmet) <= 1050.0 ) return 1;
  
  return 0;
} /* matchFramesInfo */

/**********************************************************************/
/* find matching or greater doy & hh:mm:ss 
 * - assumes getNextFramesInfo() will return increasing doy & hh:mm:ss
 * - returns 1 if getNextFramesInfo() returns matching doy & hh:mm:ss
 * - returns 0 if getNextFramesInfo() returns doy:hms > arguments' doy:hms
 * - returns -N if doy or hms do not pass validity tests
 * - if listFile==NULL, will always return 0
 */
int 
matchXXXOLDXXXFramesInfo( char *doy, char *hms, FILEFILE *ff, FRAMESINFO *fI) {
long lDoy, lH, lM, lS;

  if ( !ff) return 0;

  /* validate inputs */

  if ( 1 != sscanf( doy, "%ld", &lDoy)) return -1;

  if ( strlen( hms) != 8) return -2;
  if ( hms[2] != ':' || hms[5] != ':') return -3;
# define ID(I) if ( !isdigit( hms[I])) return -4
  ID(0); ID(1); ID(3); ID(4); ID(6); ID(7);

  sscanf( hms, "%ld%*[:]%ld%*[:]%ld", &lH, &lM, &lS);
  if ( lH > 23 || lM > 59 || lS > 59) return -5;

  /* valid time, look for matching frame */

  while ( (ff->_newData >= 0)  && ( (lDoy > fI->_doy) 
                      || ( (lDoy==fI->_doy) && (strcmp(hms,fI->_hms)>0) ) ) ) {
    getNextFramesInfo( ff, fI);
  }
  if ( lDoy == fI->_doy && !strcmp( hms, fI->_hms)) return 1;
  return 0;
} /* matchXXXOLDXXXFramesInfo */

/********************************************************************/

#define IFOPENFILE( F, PFX, SFX, FN, HDR) \
  if ( !F) { \
    strcpy( FN, PFX); \
    strcat( FN, SFX); \
    F = fopen( FN, "w"); \
    if ( !F) { \
      fprintf( stderr, "Problem opening file %s\n", FN); \
    } else { \
      if ( (HDR)) fprintf( F, "%s\n", HDR); \
    } \
  }

static char *nisNarrowPtr = { "nis" };
static char *nisWidePtr = { "niswide" };

#define USAGE \
fprintf(stderr\
,"Usage:\n %s [-f <frames filenames' list>] [-noam] <prefix>\n", argv[0])

/********************************************************/

int
main( int argc, char **argv) {
char *tokPtr[100];
char rawLine[MAXSTRSIZE];
char tokenLine[MAXSTRSIZE];
int nTok;
long scInstr;
char *sclkch;
double quatarr[4];
LINEMATCH *lm;
char *nisInstr = nisNarrowPtr;
char fn[MAXSTRSIZE];

char listFn[MAXSTRSIZE], framesFn[MAXSTRSIZE];
FILEFILE *ff;

long numImgLeftInSeq, totImgInSeq, seqNum;
long totImg;

FILE *msiMetFile=(FILE *) 0;
FILE *nisMetFile=(FILE *) 0;
FILE *nisNoamFile=(FILE *) 0;
FILE *kmetQuatFile=(FILE *) 0;

FRAMESINFO nextFramesInfo, lclFramesInfo;

long mFI;
char doy2filt[80];
char savKMet[20];
char km2deg[80];

int iargc;
int noam = 0;
char *pfx = (char *) NULL;

#define SETKM2DEG(FI) \
sprintf( km2deg, KM2DEGFMT \
       , (FI)->_rts, (FI)->_res, (FI)->_inc, (FI)->_emi, (FI)->_pha)

#define RESETKM2DEG(FI) \
  (FI)->_doy = -1; \
  (FI)->_rts = (FI)->_res = (FI)->_inc = (FI)->_emi = (FI)->_pha = -1.0; \
  SETKM2DEG(FI)

#define BADDOY2FILT \
  strcpy( doy2filt, "   ?  ??   ????????   ?????????    ?   ?   ????  ?"); \
  strcpy( savKMet, "?????????");

char msiMetHdr[] = { 
"Total DOY     UTC	 MET     SEQ# IMG#  EXP FILT               QUATERNION                     RTS      RES    INC   EMI  PHASE\n\
                                                                                                 (km)  (km/161ur)(deg) (deg) (deg)" 
};

  for ( iargc=1; iargc<argc; ++iargc) {

    if ( !strncmp( argv[iargc], "-f", 2)) {             /* -f<fn>  &  -f <fn> */
      ff = argv[1][2] ? FFinit( argv[iargc]+2)
                      : (++iargc) < argc ? FFinit( argv[iargc])
                                         : (FILEFILE *) NULL;
      if ( !ff) { USAGE; return -1; }
      continue;
    }

    if ( !strcmp( argv[iargc], "-noam")) {                           /* -noam */
      noam = 1;
      continue;
    }

    if ( !pfx) {                                                  /* <prefix> */
      pfx = argv[iargc];
      continue;
    } else {
      USAGE; return -1;
    }
  }

  if ( !pfx) { USAGE; return -1; }

  seqNum = -1;
  totImgInSeq = numImgLeftInSeq = 0;
  totImg = 0;

  RESETKM2DEG( &nextFramesInfo);
  RESETKM2DEG( &lclFramesInfo);

  /* for each line */
  while ( fgets( rawLine, MAXSTRSIZE, stdin)) {
    strcpy( tokenLine, rawLine);
    lm = matchLine( tokenLine, tokPtr, lineMatch);
    if ( !lm) {
      fprintf( stderr, "Could not parse this line:\n%s", rawLine);
    } else {
      switch ( lm->_type) {

      case LINE_MSIACQ:                                    /* MSI acquisition */
        IFOPENFILE( msiMetFile, pfx, ".imagelist", fn, msiMetHdr)
        seqNum = -1;
        totImgInSeq = numImgLeftInSeq = 0;
        sscanf( tokPtr[9], "%ld", &seqNum);
        if ( 1 == sscanf( tokPtr[13], "%ld", &totImgInSeq)) {
          numImgLeftInSeq = totImgInSeq;
        }
        BADDOY2FILT;
        break;

      case LINE_MSIEXP:                                       /* MSI exposure */
        if ( totImgInSeq > 0) numImgLeftInSeq--;
        else numImgLeftInSeq++;
        sprintf( doy2filt, "%4.4ld%4s%11s%12s%5ld%4ld%7s%3s"
               , ++totImg, tokPtr[2], tokPtr[3], tokPtr[4], seqNum
               , totImgInSeq - numImgLeftInSeq
               , (!strcmp(tokPtr[7],"Automatic")) ? "AUTO" 
                 :  ( (!strcmp(tokPtr[7],"Manual")) ? "MAN " : "????" )
               , tokPtr[14]
               );
        strcpy( savKMet, tokPtr[4]);
        mFI = matchFramesInfo( tokPtr[4], ff, &nextFramesInfo);
        if ( mFI == 1) {
          lclFramesInfo = nextFramesInfo;
        } else { 
          RESETKM2DEG( &lclFramesInfo);
          lclFramesInfo._rts = mFI;
          SETKM2DEG( &lclFramesInfo);
        }
        break;

      /* MSI Quater[n]ion line - the trigger to print an output line
       * that combines the MSI exposure line, the MSI Quat line, & 
       * corresponding info from the frames file(s)
       */
      case LINE_MSIQUAT:                                    /* MSI quaternion */
        IFOPENFILE( msiMetFile, pfx, ".imagelist", fn, msiMetHdr)

        /* good if msi exposure's & quat's kmets match */

        if ( strcmp( savKMet, tokPtr[4])) {   /* MET's do not match */

          /* print out non-existent quat for good msi exposure 
           * - use km2deg from that msi exposure & matchFramesInfo()
           */
          if ( strcmp( savKMet, "????????")) {
            fprintf( msiMetFile, " %s %10s %10s %10s %10s%s\n"
                   , doy2filt
                   , "????????", "????????", "????????", "????????"
                   , km2deg
                   );
          }

          BADDOY2FILT;          /* set doy2filt to ?'s */
        }

        /* get matching frame from doy & hms (i.e. tokens 2 & 3) */

        mFI = matchFramesInfo( tokPtr[4], ff, &nextFramesInfo);
        if ( mFI == 1) {
          lclFramesInfo = nextFramesInfo;
        } else { 
          RESETKM2DEG( &lclFramesInfo);
          lclFramesInfo._rts = mFI;
        }
        SETKM2DEG( &lclFramesInfo);

        /* output line */

        fprintf( msiMetFile, " %s %10s %10s %10s %10s%s\n"
               , doy2filt
               , tokPtr[8], tokPtr[9], tokPtr[10], tokPtr[11]
               , km2deg
               );

        BADDOY2FILT;        /* reset doy2filt */

        IFOPENFILE( kmetQuatFile, pfx, ".kmetquat", fn, (char *) NULL)
        fprintf( kmetQuatFile, "%s %s %s %s %s\n", tokPtr[4]
              , tokPtr[8], tokPtr[9], tokPtr[10], tokPtr[11]);
        break;

      case LINE_SETNISSHUT:                                /* Set NIS Shutter */
        if ( strcmp( tokPtr[10], "BOTH_OUT")) nisInstr = nisNarrowPtr;
        else nisInstr = nisWidePtr;
        break;

      case LINE_NISOBSQUAT:                   /* NIS observation + quaternion */
        IFOPENFILE( nisMetFile, pfx, ".nis.met", fn, (char *) NULL)
        /* kMET nis[wide] mirror-position */
        fprintf( nisMetFile, "%s %s %s\n", tokPtr[4], nisInstr, tokPtr[9]);
        if ( noam ) {
          IFOPENFILE( nisNoamFile, pfx, ".nis.noam", fn, (char *) NULL)
          /* doy  UTC  MET    NIS_SEQ_ID   MirrorPosition */
          fprintf( nisNoamFile, "%s %s %s %s %s\n"
                              , tokPtr[2], tokPtr[3], tokPtr[4]
                              , tokPtr[7], tokPtr[9]);
        }
        IFOPENFILE( kmetQuatFile, pfx, ".kmetquat", fn, (char *) NULL)
        fprintf( kmetQuatFile, "%s %s %s %s %s\n", tokPtr[4]
              , tokPtr[11], tokPtr[12], tokPtr[13], tokPtr[14]);
        break;
      default:
        fprintf(stderr, "Program error, contact programmer, code WSNBATGH-0\n");
        fprintf( stderr, "Problem cause:\n%s\n", rawLine);
        break;
      }
    } /* if !lm else ... */
  } /* while fgets */

  FFfree( &ff);
  if ( nisMetFile) fclose( nisMetFile);
  if ( nisNoamFile) fclose( nisNoamFile);
  if ( msiMetFile) fclose( msiMetFile);
  if ( kmetQuatFile) fclose( kmetQuatFile);
  return 0;
}
