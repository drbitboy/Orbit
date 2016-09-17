/* plt2metHtml - given plate number & orbdb database name prefix, 
 *               output HTML table of MET's that contain that plate
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "orbit_spice_names.h"
#include "orbdb.h"
#include "spudshap.h"

#include "debug.h"

#define COMMENTTAG "comment="
#define MODETAG "modetype="
#define LATTAG "lat="
#define WLONTAG "wlon="
#define LATWLONSUBMITTAG "latlonsubmit="
#define CONTROLXTAG "control.x="
#define CONTROLYTAG "control.y="
#define MAPSIZETAG "mapsize="
#define PLATENUMTAG "platenum="
#define BASETAG "base="

#define SORTONTAG "sorton="
#define SORTONDFLTTAG "sortondflt="
typedef enum {
  SORTON_MET=0       /* ascending tags */
, SORTON_INSTR
, SORTON_INSTRPAR
, SORTON_RES
, SORTON_INC
, SORTON_EMI
, SORTON_PHA
, SORTON_RESORT
, SORTON_COUNT
} SORTON;

static SORTON sortOn, sortOnDflt;         /* save which field to sort on here */

typedef struct { SORTON _item; char *_string; } SORTONTYP;

/* these tags are the action buttons' labels & also the table columns' headers*/

static SORTONTYP sortOns[] = {
  SORTON_MET, "MET"
, SORTON_INSTR, "Instrument"
, SORTON_INSTRPAR, "Instr Param"
, SORTON_RES, "AvgRes m/pxl"
, SORTON_INC, "Avg Inc, deg"
, SORTON_EMI, "Avg Emi, deg"
, SORTON_PHA, "Avg Pha, deg"
, SORTON_RESORT, "Re-sort"
, SORTON_COUNT, (char *) NULL
};

#define SORTDIRTAG "sortdir="
#define SORTDIRDFLTTAG "sortdirdflt="

typedef enum {
  SORTDIR_A=0
, SORTDIR_D
, SORTDIR_COUNT
} SORTDIR;
static SORTDIR sortDir, sortDirDflt;
typedef struct { SORTDIR _item; char *_string; } SORTDIRTYP;
static SORTDIRTYP sortDirs[] = {
  SORTDIR_A, "a"
, SORTDIR_D, "d"
, SORTDIR_COUNT, (char *) NULL
};

/* ->_item, ->_string structure lookups - ->_string is NULL on failure */

#define P2MH_TAGMATCHITEM( SORT, SORTS, ITEM) \
  for ( SORT=SORTS; SORT->_string; ++SORT) { \
    if ( SORT->_item == ITEM) break; \
  }

#define P2MH_TAGMATCHSTRING( SORT, SORTS, STRING) \
  for ( SORT=SORTS; SORT->_string; ++SORT) { \
    if ( !strncmp(SORT->_string, STRING, strlen(SORT->_string)) ) break; \
  }

/********************************
 * limit number of rows displayed 
 */

static unsigned long startAtDflt;                    /* first item # in table */
#define MAXROWS 100                      /* and limit number of rows in table */

#define STARTATTAG "startat="
#define STARTATDFLTTAG "startatdflt="

typedef enum {
  STARTAT_PREV=0
, STARTAT_NEXT
, STARTAT_COUNT
} STARTAT;
static STARTAT startAt;
typedef struct { STARTAT _item; char *_string; } STARTATTYP;
static STARTATTYP startAts[] = {
  STARTAT_PREV, "Prev"
, STARTAT_NEXT, "Next"
, STARTAT_COUNT, (char *) NULL
};

/***********************************************************
 * duplicate TAG, terminate at first "="
 */
char *p2mH_termAtEq( char *tag) {
char *c, *cRtn;
  cRtn = strdup(tag);
  if ( (c=strchr(cRtn,'=')) ) *c = '\0';
  return cRtn;
}

static int instrParMiscIdx; /* 0-4 => use mC->_miscVals[N] instead of instrPar*/
  
/***********************************************************
 * qsort comparison function - sorts pointers to metCONTENTs
 */

int p2mH_qsortMC( const void *v1, const void *v2) {
metCONTENT *mC1 = *((metCONTENT **) v1);
metCONTENT *mC2 = *((metCONTENT **) v2);
int retVal=0;

  if ( !mC1 || !mC2) {            /* ensure null pointers go last */
    if ( mC1 == mC2) return 0;
    if ( mC2) return -1;
    return 1;
  }

#define QSORTEST(MBR) \
  if ( retVal == 0) { \
    retVal = ( (mC1->MBR<mC2->MBR) ? -1 : ((mC1->MBR>mC2->MBR) ? 1 : 0) ); \
  }

  switch ( sortOn) {
  case SORTON_INSTR:
    QSORTEST(_instr);
    QSORTEST(_met);
    break;
  case SORTON_INSTRPAR:
    if ( instrParMiscIdx == -1) {
      QSORTEST(_instrPar);
    } else { 
      QSORTEST(_miscVals[instrParMiscIdx]);
    }
    QSORTEST(_met);
    break;
  case SORTON_RES:
    QSORTEST(_resolution); 
    break;
  case SORTON_INC:
    QSORTEST(_mu0);
    break;
  case SORTON_EMI:
    QSORTEST(_mu);
    break;
  case SORTON_PHA:
    QSORTEST(_alpha);
    break;
  /* default:  sort on MET */
  case SORTON_MET:
  default:
    QSORTEST(_met);
    break;
  }
  /* - invert retVal for descending sort */
  return !retVal ? 0 : (sortDir==SORTDIR_D) ? (-retVal) : retVal;
}

#define deg * (M_PI / 180.0)
#define rad * (180.0 / M_PI)

/********************************/
/* return pointer to malloc'ed & converted version of string starting at 
 * position after string tok in environment variable QUERY_STRING
 * - tok must start at beginning of QUERY_STRING or immediately after 
 *   an ampersand
 * - string terminated by ampersand or null
 * - convert %xx from hex to char; convert '+' to space
 */
char *p2mH_getQS( const char *tok) {
char *qs, *lqs;
char *tokLoc = (char *) NULL;
char *cRtn, *cSrc, *cDest;

  if ( !(lqs=qs=getenv("QUERY_STRING")) ) return tokLoc;

  while ( !tokLoc && *lqs) {
    tokLoc = strstr( lqs, tok);
    if ( !tokLoc) return tokLoc;
    if ( tokLoc > qs) {            /* if tokLoc is not first character in qs, */
      if ( tokLoc[-1] != '&') {      /* ensure it is preceded by an ampersand */
        lqs += strlen(tok);       /* if not (e.g. looking for "bc=" but found */
        tokLoc = (char *) NULL;           /*  it inside "abc="), look further */
      }
    }
    if ( tokLoc) {
      tokLoc += strlen( tok);
      break;
    }
  }

  if ( (cRtn=tokLoc) ) {                   /* copy/convert to malloced string */
  int i;
    while (  *cRtn && (*cRtn != '&')) ++cRtn;
    i = cRtn - tokLoc;
    if ( (cRtn = malloc( i+10)) ) {
    char *cSrc = tokLoc;
    char *cDest = cRtn;

      while ( cSrc < (tokLoc+i)) { /*loop through source, copy/convert to dest*/
      long hex;
      char cDestTmp[3];
        switch( *cSrc) {
        case '%':          /* source '%':  convert hex representation of byte */
          cSrc++;
          if ( 1 == sscanf( cSrc, "%2[0-9A-Fa-f]", cDestTmp)) {
            cDestTmp[2] = '\0';
            sscanf( cDestTmp, "%lx", &hex);
            cSrc += strlen(cDestTmp);
            *cDest = hex;
          } else *cDest = '%';
          break;
        case '+': *cDest = ' '; cSrc++; break;        /* convert '+' to space */
        default: *cDest = *(cSrc++); break;                   /* default copy */
        }
        cDest++;
      }
      *cDest = '\0';
    }
  }
  return cRtn;

} /* char *p2mH_getQS( const char *tok) { */

/****************************************/

typedef enum {
  METLO=0
, METHI
, PARLO
, PARHI
, RESLO
, RESHI
, INCLO
, INCHI               /* incidence, emission & phase must be last & incidence */
, EMILO               /*   must be first among them - comparing cos()         */
, EMIHI
, PHALO
, PHAHI
, HILO_COUNT
} HILOWHICHLIM;
typedef struct { HILOWHICHLIM _item; char *_string; } HILOLIMTYP;
static HILOLIMTYP hiloLimTyps[] = {
  METLO, "metlo"
, METHI, "methi"
, PARLO, "parlo"
, PARHI, "parhi"
, RESLO, "reslo"
, RESHI, "reshi"
, INCLO, "inclo"
, INCHI, "inchi"
, EMILO, "emilo"
, EMIHI, "emihi"
, PHALO, "phalo"
, PHAHI, "phahi"
, HILO_COUNT, (char *) NULL
};

typedef enum {
  MODE_HTML=0
, MODE_PLAIN
, MODE_COUNT 
} MODES;
typedef struct { MODES _item; char *_string; } MODETYP;
static MODETYP modeTyps[] = {
  MODE_HTML, "html\0Table"
, MODE_PLAIN, "plain\0Plain text"
, MODE_COUNT, (char *) NULL
};

static double hiloLims[HILO_COUNT]; /*non-0 lims saved here as 1. numLims vals*/
static int numLims;
static int hiloWhichLim[HILO_COUNT];         /* TYPE of non-0 lims saved here */

static double hiloVals[HILO_COUNT]; /*input values saved here, angles, not cos*/

/*************************************************************************
 * get limits from QUERY_STRING for filtering metContents, 0.0 => no limit
 */
int p2mH_getLims(void) {
HILOWHICHLIM hlwl;
char tag[20];
char *cBase, *c;
int i;

  numLims = 0;
  for ( hlwl=0; hlwl<HILO_COUNT; ++hlwl) {

    strcpy( tag, hiloLimTyps[hlwl]._string); strcat( tag, "=");  /* "xxxhl=" */

    hiloVals[hlwl] = hiloLims[numLims] = 0.0;

    if ( (cBase=c=p2mH_getQS( tag))) {
      while ( *c && (*c != '&')) ++c;
      i = c - cBase;
      if ( !(c=(char *) malloc( i+4))) continue;
      strncpy( c, cBase, i); 
      c[i] = '\0';
      sscanf( c, "%lf", hiloLims+numLims);
      if ( hiloLims[numLims] != 0.0) {
      double t = hiloLims[numLims];

        hiloVals[hlwl] = t;

        /*********************************
         * convert limit to database units
         */
        switch ( hlwl ) {
        case METLO:                                    /* convert kmet to met */
          hiloLims[numLims] = t * 1000.0;
          break;
        case METHI:                                    /* convert kmet to met */
          hiloLims[numLims] = (t+1) * 1000.0;              /* +1 for roundoff */
          break;
        case INCLO:                     /* convert angle in degrees to cosine */
        case INCHI:
        case EMILO:
        case EMIHI:
        case PHALO:
        case PHAHI:
          hiloLims[numLims] = (t>180.0) ? -1.0 : ((t<0.0) ? 1.0 : cos( t deg));
          break;
        case PARLO:
        case PARHI:
        default:                                   /* default - no conversion */
          break;
        }
        hiloWhichLim[numLims++] = hlwl;
      }
      free(c);
    }
  } /* for hlwl */
  return 0;
}

/****** test met content against ranges, return 1 if past any limit ******/

int 
p2mH_outOfRange( metCONTENT *mC) {
HILOWHICHLIM hlwl;
int i;

  if ( !mC) return 1;      /* also return 1 for no mC */

  for ( i=0; i<numLims; ++i) {

    hlwl = hiloWhichLim[i];

#   define OORCASE(WLO,WHI,MCMBR) \
    case WLO: if ( (MCMBR) < hiloLims[i]) { \
                DPR((stderr \
                    ,"below lo limit:  hlwl=%ld; lim=%lf; val=%lf met=%lf\n" \
                    , hlwl, hiloLims[i], (double)(MCMBR), mC->_met)); \
                return 1; \
              } \
              break; \
    case WHI: if ( (MCMBR) > hiloLims[i]) { \
                DPR((stderr \
                    ,"above hi limit:  hlwl=%ld; lim=%lf; val=%lf met=%lf\n" \
                    , hlwl, hiloLims[i], (double)(MCMBR), mC->_met)); \
                return 1; \
              } \
              break

    switch ( hlwl) {
    OORCASE(METLO,METHI,mC->_met);
    OORCASE(PARLO,PARHI
           ,(instrParMiscIdx==-1)?mC->_instrPar:mC->_miscVals[instrParMiscIdx]);
    OORCASE(RESLO,RESHI,mC->_resolution);
    OORCASE(INCHI,INCLO,mC->_mu0);        /* ***N.B.  Switch HI/LO for angles */
    OORCASE(EMIHI,EMILO,mC->_mu);
    OORCASE(PHAHI,PHALO,mC->_alpha);
    } /* switch hlwl */

  } /* for i */
  return 0;
}

/********************************/

void p2mH_errex( char *errMsg, ORBDB_FILES *ofs) {
  orbdb_close( ofs);
  printf( "ERROR:  %s\n </td></tr></table> </form> </body> </html> \n", errMsg);
  exit(0);
}

/********************************/

int
main(int argc, char **argv) {

ORBDB_FILES *ofs = (ORBDB_FILES *) NULL;
ORBDB_STATE mState, pState;

metCONTENT *mC;
metCONTENT **mCArray, **mCArrayPtr, **endMCA;
metKEY *mK, *endMK;

pltCONTENT *pC;
pltKEY *pK;

char *c, *qEnv, *cPltNum, *cX, *cY, *cBase;
char  *c1, *c2;

long x, y;
unsigned long plateNum;
double lat, elon;
long nPltKeys, nMetKeys;
long iUP, notEqual;
long hlwl, iOffset;

long argsRead;
typedef enum { CALL_LATLON=0, CALL_PLATENUM, CALL_BAD } CALLTYPE;
CALLTYPE callType;
MODETYP *mT, *mTtmp;
#define orbdbPfx argv[1]
#define pltFn argv[2]

SORTONTYP *sO;
SORTDIRTYP *sD;
STARTATTYP *sA;

unsigned long iCount;

#define ERREX(A) p2mH_errex( A, ofs)
#define P2MHCFREE(A) if (A) free(A); A=(char *)0


  /* Usage:
   *   plt2metHtml <orbdb prefix> <plate filename> [instrParMiscIdx]
   *   - get platenum=NNN from QUERY_STRING 
   *   OR
   *   - get control.x=XXX & control.y=YYY from QUERY_STRING
   */

  instrParMiscIdx = -1;

  switch ( argc) {
  case 4:
    if ( 1==sscanf(argv[3],"%d", &instrParMiscIdx) ) {
      if ( instrParMiscIdx > 0 && instrParMiscIdx < 6 ) --instrParMiscIdx;
      else instrParMiscIdx = -1;
    }
  case 3:
    break;
  default:
    ERREX( "CGI-BIN ERROR - bad arguments");
    break;
  }
  if (!(qEnv=getenv("QUERY_STRING"))) ERREX("CGI-BIN ERROR:  no QUERY_STRING");

  /* get mode type */

  P2MH_TAGMATCHITEM( mT, modeTyps, MODE_HTML);             /* default to HTML */
  if ( (c=p2mH_getQS( MODETAG)) ) {
    P2MH_TAGMATCHSTRING( mT, modeTyps, c)
  }
  P2MHCFREE( c);

  /***************************
   * macro to print HTML only
   */
# define HTMLP if ( mT->_item == MODE_HTML) printf

  /*************************************************************
   * this should be the first output line other than errors above
   * - default to html
   */
  printf( "Content-type: text/%s\n\n", mT->_string ? mT->_string : "html");

  if ( !mT->_string) ERREX("CGI-BIN ERROR:  can't set MODE");


  if ( !(cBase=p2mH_getQS( BASETAG)) ) ERREX( "CGI-BIN ERROR - no base");
  /********************************
   * sort spec - get defaults first
   */

  sortOnDflt = SORTON_MET;
  if ( (c=p2mH_getQS( SORTONDFLTTAG)) ) {
    P2MH_TAGMATCHSTRING( sO, sortOns, c)
    if ( sO->_string) sortOnDflt = sO->_item;
  }
  P2MHCFREE( c);

  sortDirDflt = SORTDIR_D;                          /* will toggle first time */
  if ( (c=p2mH_getQS( SORTDIRDFLTTAG)) ) {
    P2MH_TAGMATCHSTRING( sD, sortDirs, c)
    if ( sD->_string) sortDirDflt = sD->_item;
  }
  P2MHCFREE( c);

  /* sort spec - init to default, get any specified values */

  sortOn = sortOnDflt;
  if ( (c=p2mH_getQS( SORTONTAG)) ) {
    P2MH_TAGMATCHSTRING( sO, sortOns, c)
    if ( sO->_string) sortOn = sO->_item;
  }
  P2MHCFREE( c);

  if ( sortOnDflt == SORTON_RESORT) sortOnDflt = SORTON_MET;  /* just in case */

  if ( sortOn == SORTON_RESORT) {
    sortOn = sortOnDflt;
    sortDir = sortDirDflt;                             /* keep sort direction */
  } else {
    if ( sortOn == sortOnDflt)              /* if same column is to be sorted */
      sortDir = (SORTDIR_A+SORTDIR_D) - sortDirDflt;   /* flip sort direction */
    else
      sortDir = SORTDIR_A;                             /* else sort ascending */
  }

  /* start at default */

  startAtDflt = 1;
  if ( (c=p2mH_getQS( STARTATDFLTTAG)) ) {
    sscanf( c, "%lu", &startAtDflt);
  }
  P2MHCFREE( c);

  /* if Prev/Next button pressed - adjust startAtDflt */

  if ( (c=p2mH_getQS( STARTATTAG)) ) {
    P2MH_TAGMATCHSTRING( sA, startAts, c)
    if ( sA->_item == STARTAT_PREV) {
      sortDir = sortDirDflt;                           /* keep sort direction */
      if ( startAtDflt > MAXROWS) startAtDflt -= MAXROWS;
      else startAtDflt = 1;
    }
    if ( sA->_item == STARTAT_NEXT) {
      sortDir = sortDirDflt;                           /* keep sort direction */
      startAtDflt += MAXROWS;
    }
  }
  P2MHCFREE( c);

  p2mH_getLims();         /* get limits for filtering MET's */

  callType = CALL_BAD;
  if ( (cPltNum=p2mH_getQS( PLATENUMTAG)) ) {
    if ( 1==sscanf( cPltNum, "%lu", &plateNum)) {
    SPUDF *spudf = getplatByname( (SPUDR *) NULL, pltFn, (char **) NULL);
    double *Rxyz;
    unsigned long ip;
    VEC pltCtr = { 0.0, 0.0, 0.0 };
    unsigned long *pltNumPtr, *endPltNumPtr;
      if ( !spudf) ERREX( "CGI-BIN ERROR - Can't find plate model");
      /***************************************
       * find plate with selected plate number
       */
      pltNumPtr = spudf->platenum;
      endPltNumPtr = pltNumPtr + spudf->nface;
      while ( pltNumPtr < endPltNumPtr && *pltNumPtr != plateNum) {
	pltNumPtr++;
      }
      if ( pltNumPtr == endPltNumPtr) {
	ERREX( "CGI-BIN ERROR - Can't find plate in shape model" );
      }
      /*************************************************************
       * add the three vertices of the plate to get a vector through
       * the plate center
       */
      ip = pltNumPtr - spudf->platenum;

#     define ICOPTR(S) (spudf->Rxyz+(3*(S)))
      Rxyz = ICOPTR(spudf->faceapices[ip]);
      VADD2( Rxyz, pltCtr, pltCtr);
      Rxyz = ICOPTR(spudf->oe[spudf->faceoeidx[ip]]);
      VADD2( Rxyz, pltCtr, pltCtr);
      Rxyz = ICOPTR(spudf->oe[spudf->faceoeidx[ip] + 1]);
      VADD2( Rxyz, pltCtr, pltCtr);
      /*******************************************
       * convert to latitude, longitude in degrees
       */
      vhat( pltCtr, pltCtr);
      lat = (asin( pltCtr[2]) rad);
      elon = ((pltCtr[0] != 0.0 || pltCtr[1] != 0.0)
	     ? (atan2( pltCtr[1], pltCtr[0]) rad) : 0.0);
      callType = CALL_PLATENUM;
    }
    free( cPltNum);
  } else {
  char *cSiz;
  long xSiz, ySiz;

    /*************************
     * test if map was clicked
     */
    cX = p2mH_getQS( CONTROLXTAG);
    cY = p2mH_getQS( CONTROLYTAG);
    cSiz = p2mH_getQS( MAPSIZETAG);

    if ( cX && cY && cSiz) {
      if ( 1==sscanf( cX, "%ld", &x) 
	&& 1==sscanf(cY, "%ld", &y)
	&& 2==sscanf(cSiz, "%ldx%ld", &xSiz, &ySiz)
	 ) {
	if ( x > 0 && x <= xSiz && y > 0 && y <= ySiz) {
	  /************************************************
	   * convert clicked position to lat, elon, then XYZ of point 
	   * from which to intersect shape model
	   */
#         define OFFSET 0.5 /* pixel offset for rounding */

	  /* north => -y */

	  lat = (double) (((ySiz*0.5)+OFFSET)-y);
	  lat *= (180.0 / ySiz);

	  /* east longitude => +x */

	  /* elon = (double) (x-((xSiz*0.5)+OFFSET));         /* pm at center */
	  elon = (double) (x+OFFSET);                          /* pm at edges */
	  elon *= (360.0 / xSiz);

	  callType = CALL_LATLON;

	} /* if 0 < x <= xSiz && 0 < y <= ySiz */
      } /* if ( 1==sscanf( cX, "%ld", &x) && 1==sscanf(cY, "%ld", &y) ) { */
      free( cX);
      free( cY);
      free( cSiz);

    } else { /* if ( cX && cY) ... */
    /************************
     * map not clicked, test lat, lon text inputs + submit button
     */
    char *cLat = p2mH_getQS( LATTAG);
    char *cWLon = p2mH_getQS( WLONTAG);
    char *cLatLonSubmit = p2mH_getQS( LATWLONSUBMITTAG);
    char sn[2];
    int latTokCount;
      if ( cLat && cWLon && cLatLonSubmit ) {
	if ( 1 <= (latTokCount=sscanf( cLat, "%lf%1[SsNn]", &lat, sn))
	  && 1 == sscanf( cWLon, "%lf", &elon) ) {

	  /**********************************
	   * adjust lat & lon:  
	   * - lon West -> East
	   * - lat South/North suffix
	   * - range check
	   */

	  elon = -elon;

	  if ( latTokCount == 2) switch (*sn) {
	  case 'S': case 's':
	    lat = -lat;
	    break;
	  case 'N': case 'n':
	  default:
	    break;
	  }
	  /*********************
	   * validate lat & lon inputs
	   */
	  if ( fabs(elon) <= 360.0 && fabs(lat) <= 90.0 ) {
	    callType = CALL_LATLON;
	  }
	}
	free( cLat);
	free( cWLon);
	free( cLatLonSubmit);
      } /* if ( cLat && cWLon && cLatLonSubmit ) ... */
    } /* if ( cX && cY) ... else ... */

    /******************
     * convert from lat,lon to plate number
     */
    if ( callType == CALL_LATLON ) {
    SPUDF *spudf = getplatByname( (SPUDR *) NULL, pltFn, (char **) NULL);
    double maxL, l;
    double *Rxyz, *endRxyz;
    double coselon, sinelon, coslat, sinlat, dist;
    VEC sc, bore;
      if ( !spudf) ERREX( "CGI-BIN ERROR - Can't find plate model");
      Rxyz = spudf->Rxyz;
      endRxyz = Rxyz + (spudf->nv * 3);
      /***************************************************
      * find shape's max XYZ value, triple it for scaling
      */
      maxL = fabs( *(Rxyz++));
      while ( Rxyz < endRxyz) {
      l = fabs( *(Rxyz++));
      if ( maxL < l) maxL = l;
      }
      maxL *= 3.0;
      
      coselon = cos( (double) elon deg);
      sinelon = sin( (double) elon deg);
      coslat = cos( (double) lat deg);
      sinlat = sin( (double) lat deg);
      sc[0] = coselon * coslat;
      sc[1] = sinelon * coslat;
      sc[2] = sinlat;
      if ( fabs(sc[0]) < 1e-14) sc[0] = 0.0;
      if ( fabs(sc[1]) < 1e-14) sc[1] = 0.0;
      if ( fabs(sc[2]) < 1e-14) sc[2] = 0.0;
      VNEG2( sc, bore);                /* boresight points back at origin */
      VSCAL( maxL, sc);                       /* start from outside shape */
      /**********************************************
      * find intersection with shape of bore from sc
      */
      spudf_intersect( spudf, sc, bore, &dist, &plateNum);
      
      if ( plateNum < spudf->nface) {
      
      plateNum = spudf->platenum[plateNum];     /* convert to ordinal plate # */
      callType = CALL_PLATENUM;
      
      /*********************************************************
      * if no intersection, find closest radius (angular sense)
      * ***N.B. This should not happen
      */
      } else {
      unsigned long iP, bestiP;
      double best = 0.0;
      double dot;
      VEC v[3];
      int i;
      VNEG( bore);
      for ( iP=0; iP<spudf->nface; ++iP) {
        CPYVEC( spudf->Rxyz+(3 * spudf->faceapices[iP]), v[0]);
        CPYVEC( spudf->Rxyz+(3 * spudf->oe[spudf->faceoeidx[iP]]),v[1]);
        CPYVEC( spudf->Rxyz+(3 * spudf->oe[spudf->faceoeidx[iP]+1]),v[2]);
        for ( dot=i=0; i<3; ++i) {
          vhat( v[i], v[i]);
          dot += VDOT( bore, v[i]);
        }
        if ( dot > best) { 
          plateNum = spudf->platenum[iP];
          bestiP = iP;
          best = dot;
          callType = CALL_PLATENUM;
        }
      }
      
      } /* if plateNum < nface ... else ... */
      spud_freeSpudf( spudf, (int) 1);

    } /* if ( callType == CALL_LATLON ) ... */

  } /* if cPltNum=p2mH_getQS( PLATENUMTAG) ... else ... */

  if ( callType != CALL_PLATENUM) ERREX( "CGI-BIN ERROR:  no call type");

  if ( !(ofs=orbdb_openForRead( orbdbPfx, (ORBDB_FILES *) NULL)) ) {
    ERREX( "CGI-BIN ERROR OPENING ORBIT DATABASE");
  }

  /********************
   * get plate contents
   */
  pC = orbdb_fetchPltContent( ofs, (long) plateNum);

  if ( !pC) ERREX( "Plate not in database");
  if ( pC->_nMetKeys < 1) ERREX( "No images contain plate");

  /*********************************************
   * allocate array of pointers for metContent's
   */
  if (!(mCArray=(metCONTENT **)malloc( pC->_nMetKeys * sizeof(metCONTENT *)))) {
    free( pC);
    ERREX( "CGI-BIN ALLOCATION ERROR (metCONTENT **)");
  }

  /******************************************************************
   * get met content structures corresponding to this plate's metKEYs
   * - test parameter ranges against non-zero filters
   */
  mK = pC->_metKeys;
  endMCA = mCArray + (nMetKeys = pC->_nMetKeys);

  for ( mCArrayPtr=mCArray; mCArrayPtr < endMCA; ++mK) {

    mC = *mCArrayPtr = orbdb_fetchMetContentByKey( ofs, mK);       /* get MET */

    if ( p2mH_outOfRange(mC)) {          /* if null mC or mC out of range ... */
      endMCA--;                             /* ... bring mountain to mohammed */
      free( mC);
    } else {
      ++mCArrayPtr;                         /* ... else bring mohammed to mtn */
    }
  }

  free( pC);                                   /* AFTER ->_mK reference above */

  /*******************
   * sort metCONTENT's
   */
  qsort( mCArray, nMetKeys=(endMCA-mCArray), sizeof( metCONTENT *)
       , p2mH_qsortMC);

  /*************************
   * start building web page
   */
  HTMLP( "<html>\n\n");

  if ( cBase) HTMLP( "<base href=\"%s\">\n\n", cBase);

  HTMLP( "<head>\n\
	   <title>\n\
	     %ld Images for Plate Number %ld\n\
	   </title>\n</head>\n\n"
	, nMetKeys, plateNum);

  /*******************************************************
   * sort form:  
   * (1) save script name, plate number & base as hidden values
   */
  c = getenv("SCRIPT_NAME");
  c1 = p2mH_termAtEq( PLATENUMTAG);   /* INPUT TYPE="HIDDEN" NAME="this" */
  c2 = p2mH_termAtEq( BASETAG);

  /**************************
   * HTMLP statement start
   */
  HTMLP( "<body>\n\
\n\
<FORM target=\"list\" method=GET action=\"%s\">\n\
\n\
<INPUT TYPE=\"HIDDEN\" NAME=\"%s\" VALUE=\"%ld\">\n\
<INPUT TYPE=\"HIDDEN\" NAME=\"%s\" VALUE=\"%s\">\n"
  , c ? c : "/cgi-bin/plt2met"
  , c1 ? c1 : "platenum" , plateNum
  , c2 ? c2 : "base", cBase);
  /*
   * HTMLP statement end
   *********************/

  c = (char *) 0;                  /* c point into environment; do not free c */
  P2MHCFREE( c1);
  P2MHCFREE( c2);

  /* keep startAtDflt in range */

  /* HTMLP( "startAtDflt is now %lu", startAtDflt); */

  if ( startAtDflt > nMetKeys) {
    if ( nMetKeys > MAXROWS) {
      startAtDflt = ( ((nMetKeys-1)/MAXROWS) * MAXROWS) + 1;
    } else startAtDflt = 1;
  }
  if ( startAtDflt < 1) startAtDflt = 1;

  /* HTMLP( "range-checked startAtDflt is now %lu", startAtDflt); */

  /****************************
   * start at for current form:  
   * save start at item as hidden value
   *     - it will be the default next time
   */
  HTMLP( "<INPUT TYPE=\"HIDDEN\" NAME=\"%s\" VALUE=\"%u\">\n"
	, (c=p2mH_termAtEq( STARTATDFLTTAG)) ? c : "startatdflt"
	, startAtDflt);
  P2MHCFREE( c);

  /************
   * sort form:  
   * (2) save sort item & direction as hidden values
   *     - they will be the defaults next time
   */
  P2MH_TAGMATCHITEM( sO, sortOns, sortOn);
  c1 = sO->_string;
  HTMLP( "<INPUT TYPE=\"HIDDEN\" NAME=\"%s\" VALUE=\"%s\">\n"
	, (c=p2mH_termAtEq( SORTONDFLTTAG)) ? c : "sortondflt"
	, c1 ? c1 : "MET");
  P2MHCFREE( c);

  P2MH_TAGMATCHITEM( sD, sortDirs, sortDir);
  c1 = sD->_string;
  HTMLP( "<INPUT TYPE=\"HIDDEN\" NAME=\"%s\" VALUE=\"%s\">\n"
	, (c=p2mH_termAtEq( SORTDIRDFLTTAG)) ? c : "sortdirdflt"
	, c1 ? c1 : "d");
  P2MHCFREE( c);
  c1 = c2 = (char *) 0;

  /********************************************************************
   * plate number header, lat, lon(***N.B. WEST), re-sort submit button
   */
  switch (mT->_item) {
  case MODE_PLAIN:
    printf( "%ld Images for Plate Number %ld\n", nMetKeys, plateNum);
    printf( "%.0lf %.0lf = Latitude, West Longitude\n"
          , lat, (elon<0) ? -elon : (360.0-elon) );
    break;
  case MODE_HTML:
  default:
    printf( "<center>\n");
    printf( "<h1>Images %ld - %ld out of "
          , startAtDflt
          , (startAtDflt+MAXROWS)>nMetKeys ? nMetKeys : (startAtDflt+MAXROWS-1)
          );
    printf( "%ld for Plate Number %ld</h1>\n", nMetKeys, plateNum);
    printf( "<br><h3>(%.0lf%s, %.0lfW)</h3>\n"
          , fabs(lat), lat<0.0 ? "S" : "N", (elon<0) ? -elon : (360.0-elon) );
    break;
  }

  /********************************
   * output type selection
   * - label is after mode value, see modeTyps[] above
   * - no '\n' so Re-sort button is on same line
   */
  c1 = c2 = (char *) 0;
  HTMLP( "Output Type:  <SELECT NAME=\"%s\">\n"
        , (c1=p2mH_termAtEq(MODETAG)) ? c1 : "modetype");
  P2MHCFREE( c1);

  for ( mTtmp=modeTyps; (c=mTtmp->_string); ++mTtmp) {
    HTMLP( "  <OPTION VALUE=\"%s\"%s>%s\n"
          , c
          , mTtmp->_item==MODE_HTML ? " SELECTED" : "" /* dflt selection HTML */
          , c+strlen(c)+1);                       /* label is after mode type */
  }
  c = (char *) 0;
  HTMLP( "</SELECT>\n");

  /*******************
   * Re-sort button
   */
  P2MH_TAGMATCHITEM( sO, sortOns, SORTON_RESORT);
  HTMLP( "<INPUT TYPE=\"submit\" VALUE=\"%s\" NAME=\"%s\">\n"
        , sO->_string ? sO->_string : "Re-sort"
        , (c=p2mH_termAtEq( SORTONTAG)) ? c : "sorton");
  P2MHCFREE(c);

  /*************************
   * Previous & Next buttons
   */
  if ( startAtDflt > 1 || (startAtDflt+MAXROWS) <= nMetKeys ) {
    HTMLP( "<br>");
  }
  if ( startAtDflt > 1) {
    P2MH_TAGMATCHITEM( sA, startAts, STARTAT_PREV);
    HTMLP( "<INPUT TYPE=\"submit\" VALUE=\"%s\" NAME=\"%s\">\n"
        , sA->_string ? sA->_string : "Prev"
        , (c=p2mH_termAtEq( STARTATTAG)) ? c : "startat");
    P2MHCFREE(c);
  }
  if ( (startAtDflt+MAXROWS) <= nMetKeys ) {
    P2MH_TAGMATCHITEM( sA, startAts, STARTAT_NEXT);
    HTMLP( "<INPUT TYPE=\"submit\" VALUE=\"%s\" NAME=\"%s\">\n"
        , sA->_string ? sA->_string : "Next"
        , (c=p2mH_termAtEq( STARTATTAG)) ? c : "startat");
    P2MHCFREE(c);
  }

  /*********************************
   * user text for plain text output
   */
  c1 = p2mH_termAtEq( COMMENTTAG);
  c2 = p2mH_getQS( COMMENTTAG);
  switch(mT->_item) {
  case MODE_PLAIN:
    printf( "%s\n", c2 ? c2 : "");
    break;
  case MODE_HTML:
  default:
    printf( "<br> Comment for Plain text output:" );
    printf( " <INPUT type=text name=\"%s\" value=\"%s\">\n"
         , c1 ? c1 : "comment"
         , c2 ? c2 : "");
    break;
  }
  P2MHCFREE( c1);
  P2MHCFREE( c2);

  HTMLP( "</center>\n\n");

  /**************************
   * macros for rows of table
   */
# define TRON  HTMLP( "  <tr>\n")
# define TROFF HTMLP( "  </tr>\n")
# define TDON  printf( "    <td valign=bottom align=right>\n")
# define TDOFF printf( "    </td>\n")

# define TDONOUTOFF(A) if ( mT->_item == MODE_HTML) { TDON; printf A; TDOFF; }

# define TDONSTROFF(V)  TDONOUTOFF( STROUT(V))
# define TDONDBLOFF(V)  TDONOUTOFF( DBLOUT(V))
# define TDONDBL0OFF(V) TDONOUTOFF( DBL0OUT(V))
# define TDONDBL2OFF(V) TDONOUTOFF( DBL2OUT(V))
# define TDONLNGOFF(V)  TDONOUTOFF( LNGOUT(V))

# define STROUT(V)  ( "      %s\n", (V))
# define DBLOUT(V)  ( ((double)(V))==0.0 \
                      ? "      %.0lf\n" \
                      : "      %lf\n", (double)(V))
# define DBL0OUT(V) ( "      %.0lf\n", (double)(V))
# define DBL2OUT(V) ( "      %.2lf\n", (double)(V))
# define LNGOUT(V)  ( "      %ld\n", (long)(V))

# define TDONBUTTONOFF( ON) \
  P2MH_TAGMATCHITEM( sO, sortOns, ON); \
  switch ( mT->_item) { \
  case MODE_PLAIN: \
    printf( " %12s", sO->_string ? sO->_string : "....."); \
    break; \
  case MODE_HTML: \
  default: \
    TDONOUTOFF( \
      ( "<INPUT TYPE=\"submit\" VALUE=\"%s\" NAME=\"%s\">\n" \
      , sO->_string ? sO->_string : "Re-sort" \
      , c ? c : "sorton") \
    ) \
    break; \
  }

  /*************
   * start table
   * 1st row:  buttons to select column on which to sort
   */
  HTMLP( "<table border=\"1\">\n");

  c = p2mH_termAtEq( SORTONTAG);
  TRON;
  TDONBUTTONOFF( SORTON_MET)
  TDONBUTTONOFF( SORTON_INSTR)
  TDONBUTTONOFF( SORTON_INSTRPAR)
  TDONBUTTONOFF( SORTON_RES)
  TDONBUTTONOFF( SORTON_INC)
  TDONBUTTONOFF( SORTON_EMI)
  TDONBUTTONOFF( SORTON_PHA)
  TROFF;
  P2MHCFREE(c);

  if ( mT->_item == MODE_PLAIN) printf( "\n");

# define TDONINPOFFWID(NAM,VAL,INPWIDTH) \
  switch ( mT->_item) { \
  case MODE_PLAIN: \
    printf( ((VAL)==0.0 || (fabs((double)(VAL)) <= 1e7)) \
          ? " %12.0lf" : " %12lg" \
          , (double)(VAL)); \
    break; \
  case MODE_HTML: \
  default: \
    TDONOUTOFF( \
      ( "      <INPUT TYPE=\"TEXT\" NAME=\"%s\" VALUE=\"", NAM); \
      printf( (VAL) == 0.0 ? "%.0lf" : "%.2lf", (double)(VAL)); \
      printf( "\" SIZE=\"%s\">\n",INPWIDTH); \
    ) \
    break; \
  }

  /***************************************************************************
   * 2nd & third rows:  text fields for filtering which observation to include
   */
  for ( iOffset=0; iOffset<2; ++iOffset) {
    TRON;
    for( hlwl=iOffset; hlwl<HILO_COUNT; hlwl+=2) {
      TDONINPOFFWID( hiloLimTyps[hlwl]._string, hiloVals[hlwl],hlwl<2?"10":"6")
      /**************************************
       * second column used to label rows (i.e. no text field filter)
       */
      if (hlwl<2) switch (mT->_item) {
      case MODE_PLAIN:
        printf( " %12s", "");
        break;
      case MODE_HTML:
      default:
        TDONSTROFF( iOffset ? "<-maxima->" : "<-minima->")
        break;
      }
    }
    TROFF;
    if (mT->_item == MODE_PLAIN) 
      printf( " %s\n", iOffset ? "<-maxima" : "<-minima");
  }

  HTMLP( "</form>\n");

  for ( mCArrayPtr=mCArray, iCount=0; mCArrayPtr < endMCA; ++mCArrayPtr) {
    mC = *mCArrayPtr;
    if ( mC) {
      iCount++;
      if ( mT->_item == MODE_PLAIN ||                /* plain text: print all */
           ( (iCount >= startAtDflt)              /* OR html: print MAXROWS   */
             && (iCount < (startAtDflt+MAXROWS)) )   /* from startAtDflt      */
         ) {
      double kmet = mC->_met / 1000.;
      long dirPart, namPart;

        TRON;
        dirPart = (long) (kmet / 100000.);
        namPart = (long) (kmet - (dirPart * 100000.));

        /****************
         * put MET & Image as one-row table in single cell so they 
         * are not over each other
         */
#       define ACOS(A) ( ((A)<=1.0 && (A)>=-1.0) ? (acos(A) rad) : -999.0 ) 

        switch ( mT->_item ) {
        case MODE_PLAIN:
          /* printf( " %12.0lf", kmet); */
          /* printf( " %7ld%05ld", dirPart, namPart); */
          printf( " %12.0lf", kmet);
          printf( " %12ld", mC->_instr);
          if ( instrParMiscIdx == -1) {
            printf( " %12ld", mC->_instrPar);
          } else {
            printf( " %12lf", mC->_miscVals[instrParMiscIdx]);
          }
          printf( " %12lf", mC->_resolution);
          printf( " %12lf", ACOS(mC->_mu0));
          printf( " %12lf", ACOS(mC->_mu));
          printf( " %12lf", ACOS(mC->_alpha));
          printf( "\n");
          break;

        case MODE_HTML:
        default:
          TDON;
          HTMLP( "<table border=\"0\">\n"); TRON;
          TDON;
          HTMLP( "      <a href=\"%ld/%05ld.jpg\" target=\"image\">\n"
                , dirPart, namPart);
          HTMLP LNGOUT( (mC->_met-fmod(mC->_met,1000.)) / 1000.);
          HTMLP( "      </a>\n");
          TDOFF;
          TDON;
          HTMLP( "      <a href=\"%ld/%05ld.jpg\" target=\"image\">\n"
                , dirPart, namPart);
          HTMLP( "      <img src=\"%ld/thumb_%05ld.jpg\" border=0>\n"
                , dirPart, namPart);
          HTMLP( "      </a>\n");
          TDOFF;

          TROFF; HTMLP( "</table>\n");

          TDOFF;

          TDONLNGOFF( mC->_instr);

          if ( instrParMiscIdx == -1) {
            TDONLNGOFF( mC->_instrPar)
          } else {
            TDONDBLOFF( mC->_miscVals[instrParMiscIdx])
          }

          TDONDBL2OFF( mC->_resolution)

          TDONDBL0OFF( ACOS(mC->_mu0))

          TDONDBL0OFF( ACOS(mC->_mu))

          TDONDBL0OFF( ACOS(mC->_alpha))
          break;
        }
        TROFF;
      } /* if iCount > 0 ... */
      free( mC);
    } else {
      TRON; TDONSTROFF( "MISSING MET CONTENT")
    }
  }

  /**********************************
   * finish table & html, close ORBDB
   */
  HTMLP( "</table> </body> </html>\n" );
  orbdb_close(ofs);

  return 0;

} /* main */

#include "orbit3d_dummy_funcs.h"
