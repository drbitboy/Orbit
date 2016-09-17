#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef vms
char *malloc();
char *realloc();
#else
#include <malloc.h>
#endif
#include "debug.h"

#include "orbit_spice_names.h"
#include "orbit_cas.h"
#include "orbit_util.h"

static char casNames[CASTYPE_COUNT][30];  /* to save CAS type names */

/**********************************************************************/
/* initialize static info */

void
orbit_CAS_staticInit() {
static notfirst;
int i;

  if ( notfirst) return;  /* only run initialization once */
  notfirst = 1;

  /* load static array with CAS type names */

  for ( i=0; i<CASTYPE_COUNT; ++i) {
    strcpy( casNames[i], WHOAMI( i));
    if ( !strcmp( WHOAMI(-1), casNames[i])) {
      fprintf( stderr, "orbit_CAS_NameToType:  %s code WSNBATGH-0\n"
             , "Program error:  contact programmer,");
      exit(1);
    }
  }

  return;
}

/**********************************************************************/
/* convert from a CAS enumerated type to CAS type name */

char *
orbit_CAS_TypeToName( int typeCas) {
int type;
int i;
static char badType[] = "UNKNOWN_CAS_TYPE";

  orbit_CAS_staticInit();

  if ( typeCas > -1 && typeCas < CASTYPE_COUNT) return casNames[typeCas];
  typeCas -= CASTYPE_COUNT;
  if ( typeCas > -1 && typeCas < OPNAVTYPE_COUNT) return opnavNames[typeCas];

  return badType;
}

/***********************************/
/* convert from CAS to CAS type name */
char *
orbit_CAS_FragToName( CAS *cas) {
static char badType[] = { "NULL_CAS" };
  if ( !cas) return badType;
  return orbit_CAS_TypeToName( CASTYPE(cas));
}

/**********************************************************************/
/* convert from a CAS type name to enumerated type */

int
orbit_CAS_NameToType( char *typeName) {
int type;
int i;

  orbit_CAS_staticInit();

  if ( !typeName) return -1;

  for ( i=0; i<CASTYPE_COUNT; ++i) 
    if ( !strcmp( casNames[i], typeName)) return i;

  for ( i=0; i<OPNAVTYPE_COUNT; ++i)
    if ( !strcmp( opnavNames[i], typeName)) return (i+CASTYPE_COUNT);

  return -1;
}

/**********************************************************************/

CAS *
orbit_CAS_new( int casType) {
CAS *newCas;
static VEC nulvec = { 0.0, 0.0, 0.0 };
static long casNum = 0;
int i;

/* how to make a new CAS
 * - malloc CAS struct + sub-struct
 * - load CAS struct 
 */
#define MKCAS(T,C,U)          MKCASNOBREAK(T,C,U) break
#define MKCASNOBREAK(T,C,U) \
  case T: \
    newCas = (CAS *) malloc( sizeof(CAS) + sizeof(C)); \
    DPR(( stderr, "newCas = 0x%08x - 0x%08x\n", newCas \
                , ((long)newCas) + sizeof(CAS) + sizeof(C))); \
    if ( !newCas) { \
      fprintf( stderr \
             , "***orbit_CAS_new:  can't malloc CAS %s %s\n" \
             , "for CAS type " \
             , WHOAMI(T) \
             ); \
      fflush( stderr); \
    } else { \
      newCas->_type = T; \
      sprintf( newCas->_name, "%s %ld" /* use casType not T - CASOPNAV_* */ \
             , orbit_CAS_TypeToName(casType), casNum++); \
      newCas->_duration = 0L; \
      newCas->_delayStart = 0.0; \
      newCas->_malloced = CAS_MALLOC_NEW; \
      newCas->top = newCas->topNext = newCas->prev = newCas->next = (CAS *) 0; \
      newCas->_enabled = 1; \
      newCas->_data.U = (C *) (newCas + 1); \
      *(newCas->_ptrEt = &newCas->_et) = 0.0; \
      newCas->_noParams = 0; /* assume this frag has some params */ \
      newCas->_ctlVar = (long *) NULL; \
      newCas->_ctlLims[0] = 1; \
      newCas->_ctlLims[1] = 9999; \
    }

  /* create CAS struct using MKCAS macro above */
  switch ( casType) {
  MKCAS( CASTYPE_SHOOT, SHOOT, _msiShoot);
  MKCAS( CASTYPE_MSISEQDEF, MSISEQDEF, _msiSeqDef);
  MKCAS( CASTYPE_NISSEQDEF, NISSEQDEF, _nisSeqDef);
  MKCAS( CASTYPE_NISSR, NISREP, _nisRep);
  MKCAS( CASTYPE_NISDR, NISREP, _nisRep);
  MKCAS( CASTYPE_NISEX, NISREP, _nisRep);
  MKCAS( CASTYPE_NISSU2, NISSU, _nisSu);
  MKCAS( CASTYPE_NISSU1, NISSU, _nisSu);
  MKCAS( CASTYPE_MSISR, MSIREP, _msiRep);
  MKCAS( CASTYPE_MSIDR, MSIREP, _msiRep);
  MKCAS( CASTYPE_MSIDSR, MSIREP, _msiRep);
  MKCAS( CASTYPE_MSITR, MSIREP, _msiRep);
  MKCAS( CASTYPE_DS56, DS56, _ds56);
  MKCAS( CASTYPE_DS40, DS40, _ds40);
  MKCAS( CASTYPE_DS40FULL, DS40, _ds40);
  MKCAS( CASTYPE_CAS, CASCAS, _casCAS);
  MKCAS( CASTYPE_CASDEF, CASDEFCAS, _casDefCAS);
  MKCAS( CASTYPE_REQ, REQCAS, _reqCAS);
  MKCAS( CASTYPE_AUTOEXPOSE, AUTOEXPOSEFRAG, _auto);
  MKCAS( CASTYPE_LOADFILT, LOADFILTFRAG, _loadFilt);
  MKCAS( CASTYPE_MSICONFIG, MSICONFIGFRAG, _msiConf);
  MKCAS( CASTYPE_MSIPARK, PARKFRAG, _park);
  MKCAS( CASTYPE_NISPARK, PARKFRAG, _park);
  MKCAS( CASTYPE_MSIRELATT, RELFRAG, _rel);
  MKCAS( CASTYPE_NAVRELATT, RELFRAG, _rel);
  MKCAS( CASTYPE_NISCAL, NISCAL, _nisCal);
  MKCAS( CASTYPE_NISBUFFLUSH, NISBUFFLUSH, _nisBufFlush);
  MKCAS( CASTYPE_NISCONFIG, NISCONFIG, _nisConf);
  MKCAS( CASTYPE_NISRELATT, RELFRAG, _rel);

  /* xgrs 30.jul'1999 */

  MKCAS( CASTYPE_DS40XGRS, DS40, _ds40);
  MKCAS( CASTYPE_XGRSCONFIG, XGRSCONFIG, _xgrsConf);
  MKCAS( CASTYPE_XGRSRELATT, RELFRAG, _rel);
  MKCAS( CASTYPE_XGRSPARK, PARKFRAG, _park); /**/

  default:

    /* special case for CASTYPE_CASOPNAV:  offset from CASTYPE_COUNT
     * i.e. casType = CASTYPE_COUNT + OPNAV_subtype
     */

    newCas = (CAS *) 0;     /* assume failure */

    if ( casType>=CASTYPE_COUNT && casType < (CASTYPE_COUNT+OPNAVTYPE_COUNT)) {

      MKCASNOBREAK( CASTYPE_CASOPNAV, CASOPNAV, _opnavCAS) /* no ";" required */

      /* extract subtype, set casType to CASTYPE_CASOPNAV */

      if ( newCas) {
        newCas->opnavSubType = casType - CASTYPE_COUNT;
        casType = CASTYPE_CASOPNAV;
      }
      break;
      
    }

    fprintf( stderr, "***orbit_CAS_new:  bad casType\n");
    fflush( stderr);

  } /* switch casType */

  if ( !newCas) return newCas;

  /***************************************************/
  /* ... and load default values into the sub-struct */
  switch ( casType) {

  case CASTYPE_MSIPARK:           /* all of these have no params */
  case CASTYPE_NISPARK:
  case CASTYPE_XGRSPARK: /**/
  case CASTYPE_MSIRELATT:
  case CASTYPE_NISRELATT:
  case CASTYPE_XGRSRELATT:
  case CASTYPE_NAVRELATT:
  case CASTYPE_NISCAL:
  case CASTYPE_NISBUFFLUSH:
    newCas->_noParams = 1;
    break;

  case CASTYPE_AUTOEXPOSE:
    i = 0;
    newCas->msiAutoTImDelta = i++;
    newCas->msiAutoFile4TIm = i++;
    newCas->msiAutoTImExpTime = i++;
    newCas->msiAutoAllowSat = i++;
    newCas->msiAutoTargSatDN = i++;
    newCas->msiAutoOvrExpFlbkTim = i++;
    newCas->msiAutoNoiseOffset = i++;
    break;

  case CASTYPE_LOADFILT:
    for ( i=0; i<8; ++i) newCas->msiLoadFiltSensArray[i] = i;
    break;

  case CASTYPE_XGRSCONFIG:
    newCas->xgrsConfPosn = XGRSCONFImgPosn;                           /* "MG" */
    newCas->xgrsConfDir = XGRSCONFIforDir;                       /* "FORWARD" */
    newCas->xgrsConfBits = 0;
    break;

  case CASTYPE_MSICONFIG:
    newCas->msiConfFullIm = MSICONFIunchFullIm;
    newCas->msiConfSumIm = MSICONFIunchSumIm;
    newCas->msiConfBits = 0;
    break;

  case CASTYPE_NISCONFIG:
    newCas->nisConfSlitDriveSel = NISCONFInochSlitDriveSel;
    newCas->nisConfShutDriveSel = NISCONFInochShutDriveSel;
    newCas->nisConfVoltSet = NISCONFInochVoltSet;
    newCas->nisConfScanDriveSel = NISCONFInochScanDriveSel;
    newCas->nisConfBits = 0;
    break;

#define SEQ2START 100

  case CASTYPE_MSITR:
    newCas->msiRepDelay[0] = SEQ2START;
    for ( i=0; i<3; ++i) {
      newCas->msiRepSeq[i] = 1;
      newCas->msiRepDel[i] = 1;
      newCas->msiRepImageTypeDblTR[i] = 1.3;
    }
    newCas->msiRepIter[0] = 2;
    break;

  case CASTYPE_MSIDSR:
    newCas->msiRepSeq2Start = SEQ2START;

  case CASTYPE_MSIDR:
    newCas->msiRepSeq[1] = 1;
    newCas->msiRepDel[1] = 1;
    newCas->msiRepIter[1] = 2;
    newCas->msiRepDelay[1] = SEQ2START;

  case CASTYPE_MSISR:
    newCas->msiRepSeq[0] = 1;
    newCas->msiRepDel[0] = 1;
    newCas->msiRepIter[0] = 2;
    newCas->msiRepDelay[0] = 0;
    newCas->msiRepImageTypeDbl = 1.3;
    break;

  case CASTYPE_NISSU2:
  case CASTYPE_NISSU1:
    newCas->nisSuRepDelay = 0;
    newCas->nisSuSeq = 1;
    newCas->nisSuMirrorPosn = 72;
    newCas->nisSuAperture = NISSUIslitAper;
    newCas->nisSuGain = NISSUI000Gain;
    newCas->nisSuPpss = 1;
    newCas->nisSuStepDir = NISSUI000Dir;      /* only applies to SU2 */
    newCas->nisSuNoss = 1;                     /* only applies to SU */
    newCas->nisSuBits = 0;

    break;

  case CASTYPE_NISDR:
    newCas->nisRepSeq[1] = 0;

    /* set mirror position, aperture & gain for both Seqs to NOCHANGE */

    newCas->nisRepMirrorPosn[0] = 
      newCas->nisRepMirrorPosn[1] = ORBIT_NOCHANGEVAL;
    newCas->nisRepAperture[0] = newCas->nisRepAperture[1] = NISREPInochangeAper;
    newCas->nisRepGain[0] = newCas->nisRepGain[1] = NISREPInochangeGain;

    newCas->nisRepDel[1] = 1000;
    newCas->nisRepBits[1] = 0;

  case CASTYPE_NISEX:
  case CASTYPE_NISSR:
    newCas->nisRepSetup = 0;
    newCas->nisRepMirrorPosnSetup = 72;
    newCas->nisRepApertureSetup = NISREPIslitAper;
    newCas->nisRepGainSetup = NISREPI000Gain;

    /* [0] & [1] set above for NISDR */
    if ( !isNISDR(*newCas)) {
      newCas->nisRepMirrorPosn[0] = 72;
      newCas->nisRepAperture[0] = NISREPIslitAper;
      newCas->nisRepGain[0] = NISREPI000Gain;
    }

    newCas->nisRepBitsSetup = 0;
    newCas->nisRepSeq[0] = 0;
    newCas->nisRepDel[0] = 1000;
    newCas->nisRepDelay = 0;
    newCas->nisRepIter = (casType==CASTYPE_NISEX) ? 1 : 2;
    newCas->nisRepBits[0] = 0;
    break;

  case CASTYPE_NISSEQDEF:
    newCas->nisIdNum = 0;
    newCas->nisNumScans = 2;
    newCas->nisStepMirror = 1;
    newCas->nisSecPerObs = 16;
    newCas->nisSecBtwObs = 1;
    newCas->nisSecBtwScan = 1;
    newCas->nisCalInterval = 1;
    newCas->nisNumObs = 8;          /* observations/scan */
    newCas->nisNumRests = 0;
    newCas->nisNumDarks = 0;
    newCas->nisTopIndex = -1;
    break;

  case CASTYPE_SHOOT:
    newCas->msiShootCount = 12;
    for ( i=0; i<12; ++i) {
      newCas->msiShootSeq[i] = 1;
      newCas->msiShootDel[i] = i * 2;
      newCas->msiShootImageTypeDbl[i] = 1.3;
    }
    break;

  case CASTYPE_MSISEQDEF:
    /* default values, 1 image, filter 0, 999 msec exposure */
    newCas->msiSeqDefIdNum = 0;
    newCas->msiSeqDefNumImages = 1;
    newCas->msiSeqDefInterval = 2;
    newCas->msiSeqDefBits = 0;                                /* repeat = off */
    /* Comp Table = NONE, Comp Alg = NONE, bits set in orbitgui_cas.c */
    newCas->msiSeqDefCmpTbl = 0;
    newCas->msiSeqDefCmpAlg = MSISEQDEF_CMPALG_NONE;
    newCas->msiSeqDefDpcm = MSISEQDEFIonDpcm-MSISEQDEFI000Dpcm;  /* dpcm = on */
    newCas->msiSeqDefMode = MSISEQDEFIautMode-MSISEQDEFI000Mode; /* mode=auto */
    *newCas->msiSeqDefMsecExp = 999;
    *newCas->msiSeqDefFilt = 0;
    for ( i=1; i<8; ++i) {
      newCas->msiSeqDefMsecExp[i] = newCas->msiSeqDefFilt[i] = 0;
    }
    newCas->msiSeqDefPixels = 16;
    newCas->msiSeqDefTopIndex = -1;

    break;

  case CASTYPE_DS40XGRS: /* Aimpt Sel=aim; Bore=X; Roll Ref/Vec/Coord=Z/Z/ECI */
  case CASTYPE_DS40:              /* Roll Ref/Vec=Z/Z; Roll Ref Coord sys=ECI */
  case CASTYPE_DS40FULL:

    newCas->ds40Bits = 0;

    if isDS40XGRS(*newCas) {
      newCas->ds40SlewDuration =  500;
    } else {
      newCas->ds40SlewDuration =  900;        /* changed from 600 30.jul'1999 */
    }

    newCas->ds40AimptFrmType = Inadir;                      /* ds40_coord_sys */

    CPYVEC( nulvec, newCas->ds40AimptVec);

    /* end of CASTYPE_DS40XGRS user parameters */

    newCas->ds40AimptSelect = Iaim; /* aimpt shortcut=use aim coord sys above */

    CPYVEC( nulvec, newCas->ds40VbVec);
    newCas->ds40VbVec[0] = 1.0;                         /* virt bore = X axis */

    /* end of CASTYPE_DS40 user parameters */

    /* default roll selections that follow are from 
     * RESTORE_DS40_ROLL() in near.satf
     */
    CPYVEC( nulvec, newCas->ds40ScRollVec);
    newCas->ds40ScRollVec[2] = 1.0;                       /* sc roll = Z axis */

    CPYVEC( nulvec, newCas->ds40RollRefVec);  /* irrelevent as Roll Algor=ECI */
    newCas->ds40RollRefVec[2] = 1.0;            /* roll ref = 1.0 out Z axis, */

    newCas->ds40RollFrmType =  Ieci;               /* roll ref is earth nadir */

    break;

  case CASTYPE_DS56:
    newCas->ds56FrmType = Isbf;  /* SBF dflt at ANNBO request - bit set later */
    newCas->ds56ReUse = DS56IreUSE_not;
    newCas->ds56Bits = BITxCHG | BITyCHG | BITzCHG;
    CPYVEC( nulvec, newCas->ds56RatePauseDur[0]);
    CPYVEC( nulvec, newCas->ds56RatePauseDur[1]);
    CPYVEC( nulvec, newCas->ds56RateVec);
    break;

  case CASTYPE_CAS:
    /* set pointers to null & act as if empty strings are in fields */
    for ( i=0; i<CASTYPE_COUNT; ++i) {
      newCas->casCasList[i] = (CAS *) 0;
      newCas->casArgListPtrs[i] = (char *) 0;
      newCas->casArgListSize[i] = 0;
    }
    break;

  case CASTYPE_CASDEF:
    /* set pointers to null & act as if empty strings are in fields */
    for ( i=0; i<MAXNUMSEQDEF; ++i) {
      newCas->casDefList[i] = (CAS *) 0;
      newCas->casDefListPtrs[i] = (char *) 0;
      newCas->casDefListSize[i] = 0;
    }
    newCas->casDefAddMsi = 0;
    newCas->casDefAddNis = 0;
    break;

  case CASTYPE_CASOPNAV:
    orbit_CAS_opnavNew( newCas);
    break;

  case CASTYPE_REQ:
    strcpy( newCas->reqAddCasTemplate, "*.CAS");
    strcpy( newCas->reqAddCasDefTemplate, "*.CASDEF");
    strcpy( newCas->reqAddCasOpnavTemplate, "*.CASOPNAV");
    newCas->reqAddTop = 0;
    newCas->reqAddCasDef = 0;
    newCas->reqAddCasOpnavBits = 0;
    break;

  default:
    break;
  } /* switch casType */

  return newCas;
}

/***********************************************************/
/* free malloced cas and any children */

void
orbit_CAS_freeCAS( CAS *cas) {
SUBFRAGS *lclSubFrags;
CAS **casPtr;
  if ( !cas) return;
  if isCASOPNAV(*cas) {
    FOROPNAVSUBFRAGS( cas, lclSubFrags, casPtr)
      if ( *casPtr) free( *casPtr);
    }}
  }
  free( cas);
  return;
}

/**********************************************************************/
/* convert argList to a CAS - argList argument is not changed
 * if input (CAS *) cas is null, argList *MUST* start with CAS type string
 *   (e.g. DS40, MSISR, &c) 
 * if input (CAS *) cas is non-null, argList *MAY* start with CAS type string
 * - if it does, CAS type will be checked
 */
CAS *
orbit_CAS_ArgsToCAS( char *argList, CAS *cas) {
char *tokPtr[100];
int ntok;
int casType;
int i, cou;
CAS *lclcas = cas;
long ilong, i2;
char *nlPtr;
char *tPtr;
int tlen, j;
char *dupStr = strdup( argList);
int offset = lclcas ? 1 : 0;        /* offset into tokPtr of start of argList */
long rwargFailed;

#define FREE(A) if ( A) free(A)
#define RTNERR { FREE(dupStr); return (CAS *) 0; }

  /* read tokens into tokPtr+1 in case we need to get CAS type from lclcas */

  ntok = orbit_commaParse( dupStr, tokPtr+offset);
  if ( !ntok) RTNERR
  casType = orbit_CAS_NameToType( tokPtr[offset]); /* get type from 1st token */
  if ( casType == -1 && !lclcas) RTNERR  /* error if no type in 1st token AND */
                                                              /* no input CAS */

  /* TO THIS POINT, casType is OK (not -1) &/OR lclcas is not a null pointer */

  if ( casType != -1 && offset) {    /* if CAS type is in shifted argList ... */
    for (i=0; i<ntok; ++i) tokPtr[i] = tokPtr[i+1];       /* ... shift tokens */
    offset = 0;                                       /* ... and reset offset */
  } else if ( casType == -1 && lclcas) {
    casType = CASTYPE(lclcas);                  /* ... else get full CAS type */
    tokPtr[0] = orbit_CAS_TypeToName( casType);             /* from input CAS */
  }

  if ( !lclcas) lclcas = orbit_CAS_new( casType);
  else if ( CASTYPE(lclcas) != casType) RTNERR

  if ( !lclcas) RTNERR

# undef RTNERR
# define RTNERR { \
    FREE(dupStr); \
    if ( !cas) orbit_CAS_freeCAS(lclcas); \
    return (CAS *)NULL; \
  }

# define PRTWARN( WARNING) \
  fprintf( stderr, "%s parsing token %d (%s) of argument list:\n%s\n" \
                 , WARNING, i-offset, tokPtr[i-1], argList)

#define ISREAD 1
#include "orbit_cas_rwarg.h"

  FREE( dupStr);
  return lclcas;
}

/*************************************************************************/
/* write CAS contents to a strdup'ed string of comma-separated arguments */

char *
orbit_CAS_CASToArgs( CAS *cas) {
long ilong, i2;
int j;
CAS *lclcas = cas;
char workingString[1024];
char *workptr = workingString;
char tmpStr[100];
long rwargFailed;

#define ISREAD 0
#include "orbit_cas_rwarg.h"

  return strdup( workingString);
}

/**********************************************************************/
/* add/replace a comma-separated string of arguments 
 * to/in a TOP, CASDEF or REQ CAS
 */
void
orbit_CAS_addTopArgs( CAS *topCas, int topidx, char *argList) {
long argsLen = strlen( argList) + 1;  /* include null terminator */
long fat = argsLen + ((argsLen<100)?10:(argsLen/10)); /* alloc 10c or % extra */
long *listsize;
char **listptr;
long maxtopidx;

  switch ( topCas->_type) {
  case CASTYPE_CAS:
    maxtopidx = CASTYPE_COUNT;
    listsize = topCas->casArgListSize + topidx;
    listptr = topCas->casArgListPtrs + topidx;
    break;
  case CASTYPE_CASDEF:
    maxtopidx = MAXNUMSEQDEF;
    listsize = topCas->casDefListSize + topidx;
    listptr = topCas->casDefListPtrs + topidx;
    break;
  case CASTYPE_CASOPNAV:          /* nothing to do for CASOPNAV, children are */
    return;                                   /* handled with CASOPNAV itself */

  default:
    return;
  }

  /* ensure valid topidx */
  if ( topidx < 0 || topidx >= maxtopidx) return;

  /* ensure storage is large enough */

  if ( *listsize < argsLen) {
    if ( *listptr) 
      *listptr = realloc(*listptr,fat);
    else *listptr = malloc( fat);

    if ( !(*listptr)) {
      fprintf( stderr
             , "orbit_CAS_addTopArgs:  failed allocating string space\n");
      *listsize = 0;
      return;
    } else *listsize = fat;
  }

  strcpy( *listptr, argList);  /* here's the beef */

  return;
}

/**************************************************************/
/* delete lowest level CAS */

#define FREE0(A) { FREE(A); A = (void *) 0; }

void
orbit_CAS_deleteCas3( CAS *cas, void (*orbitRemoveCasFromListFunc)(CAS *)) {
                                /* startCAS is 1st CAS in linked list */
CAS *topCas = cas->top;
int topidx;

  switch ( topCas ? topCas->_type : -1) {

  /* CAS children - clean up pointers, reset arg list size
   */
  case CASTYPE_CAS:  /* ds40*, ds56, msi & nis repeats, shoot, nisex */

    FREE0( topCas->casArgListPtrs[cas->_type]);
    topCas->casArgListSize[cas->_type] = 0;
    topCas->casCasList[cas->_type] = (CAS *) 0;
    break;

  /* CASDEF children - clean up pointers, reset arg list size
   */
  case CASTYPE_CASDEF:

    topidx = isMSISEQDEF( *cas) ? cas->msiSeqDefTopIndex : cas->nisTopIndex;
    FREE0( topCas->casDefListPtrs[topidx]);
    topCas->casDefListSize[topidx] = 0;
    topCas->casDefList[topidx] = (CAS *) 0;
    break;

  /* CASOPNAV children - don't delete until allowed to 
   *                     - i.e. when ->opnavLetMyPeopleGo is set nonZero 
   *                       in orbit_CAS_deleteTopCas
   *                       - i.e. when CASOPNAV itself is being deleted
   */
  case CASTYPE_CASOPNAV:

    if (!topCas->opnavLetMyPeopleGo) return;
    break;

  case -1:     /* no parent CAS, continue */
    break;
  }

  /* cleanup complete, remove from (display &/or linked) list 
   * & free memory allocation
   */
  if ( orbitRemoveCasFromListFunc) orbitRemoveCasFromListFunc( cas);
  FREE( cas);

  return;
} /* orbit_CAS_deleteCas3( CAS *cas, void (*orbitRemoveCasFromListFunc)(... */

/*********************************/
void
orbit_CAS_deleteTopCas( CAS *cas, void (*orbitRemoveCasFromListFunc)(CAS *)) {
CAS *nxt1;
CAS *req = cas->top;
int i;
SUBFRAGS *lclSubFrags;
CAS **casPtr;

  /* for each type of parent CAS, delete children */

  switch( cas->_type) {
  case CASTYPE_CAS:
    for ( i=0; i<CASTYPE_COUNT; ++i) {
      if ( cas->casArgListPtrs[i]) 
        orbit_CAS_deleteCas3( cas->casCasList[i], orbitRemoveCasFromListFunc); 
    }
    break;

  case CASTYPE_CASDEF:
    for ( i=0; i<MAXNUMSEQDEF; ++i) {
      if ( cas->casDefListPtrs[i]) 
        orbit_CAS_deleteCas3( cas->casDefList[i], orbitRemoveCasFromListFunc);
    }
    break;

  case CASTYPE_CASOPNAV:
    cas->opnavLetMyPeopleGo = 1;
    FOROPNAVSUBFRAGS( cas, lclSubFrags, casPtr)
      orbit_CAS_deleteCas3( *casPtr, orbitRemoveCasFromListFunc);
    }}
    break;
  }

  /* reconnect ->topNext linked list around cas */

  if ( req) {
    for ( nxt1=req; nxt1->topNext && (nxt1->topNext != cas)
        ; nxt1=nxt1->topNext) ;
    if ( nxt1->topNext == cas) nxt1->topNext = cas->topNext;
  }

  /* delete the CAS* itself */

  if ( orbitRemoveCasFromListFunc) orbitRemoveCasFromListFunc( cas);
  FREE( cas);

  return;
}

/*********************************/
void
orbit_CAS_deleteReqCas(CAS *cas, void (*orbitRemoveCasFromListFunc)(CAS *)) {
CAS *casCAS;

  /* delete children */

  for ( casCAS=cas->topNext; casCAS; casCAS=casCAS->topNext) {
    orbit_CAS_deleteTopCas( casCAS, orbitRemoveCasFromListFunc);
  }

  /* delete the REQ itself */

  if ( orbitRemoveCasFromListFunc) orbitRemoveCasFromListFunc( cas);
  FREE( cas);

  return;
}

/*********************************/
void
orbit_CAS_deleteAnyCas( CAS *cas, void (*orbitRemoveCasFromListFunc)(CAS *)) {

  switch( cas->_type) {
  case CASTYPE_REQ:
    orbit_CAS_deleteReqCas( cas, orbitRemoveCasFromListFunc);
    break;
  case CASTYPE_CAS:
  case CASTYPE_CASDEF:
  case CASTYPE_CASOPNAV:
    orbit_CAS_deleteTopCas( cas, orbitRemoveCasFromListFunc);
    break;
  default:
    orbit_CAS_deleteCas3( cas, orbitRemoveCasFromListFunc);
    break;
  }
  return;
}

/************************************************************************/
/* update top SeqDef CAS for a child cas
 * - if cas->top is non-null
 *   - if existing index into cas->top->topdeflist* arrays is -1
 *     - find open slot in cas->top->topdeflist* (i.e. 0-length argument list)
 *   - put argument list into cas->top at existing or new index
 *   - make cas->_ptrEt point to same place as cas->top->_ptrEt
 */
void
orbit_CAS_updateCasDef( CAS *cas) {
CAS *casDefCAS = cas->top;
char *argList;
int *topidx = (int *) 0;
long i;

  if ( !casDefCAS) return;

  topidx = (cas->_type == CASTYPE_MSISEQDEF)
         ? &cas->msiSeqDefTopIndex : &cas->nisTopIndex;

  /* if topidx is -1, find an unused slot in a CASDEF CAS */

  if ( *topidx == -1) {
    for ( i=0; i<MAXNUMSEQDEF; ++i) {
      if ( casDefCAS->casDefListSize[i] == 0) break;
    }
    if ( i < MAXNUMSEQDEF) *topidx = i;
    else {
      fprintf( stderr, "Program Error:  ");
      fprintf( stderr, "Can't add another SEQDEF to a CASDEF CAS\n");
      fflush( stderr);
      return;
    }
  }

  if ( argList = orbit_CAS_CASToArgs( cas)) {
    casDefCAS->casDefList[*topidx] = cas;
    orbit_CAS_addTopArgs( casDefCAS, *topidx, argList);
    free( argList);
  }

  cas->_ptrEt = casDefCAS->_ptrEt;    /* make CAS sched. pt be same as CASDEF */

  return;
}

/************************************************************************/
/* update REQ CAS for a child cas (which is itself a TOP or TOP DEF CAS)
 * - if cas->top (a REQ CAS) is non-null
 *   follow linked list of cas->topNext pointers of TOP * children of cas->top
 *   - if existing cas is not found in linked list, add it to list
 *   - make cas->_ptrEt point to same place as cas->top->_ptrEt
 */
void
orbit_CAS_updateReq( CAS *cas) {
CAS *reqCAS = cas->top;
CAS *linkedListCAS = reqCAS;
char *argList;
int *topidx = (int *) 0;
long i;

  if ( !reqCAS) return;

  /* follow linked list to end or to current CAS */

  while ( linkedListCAS->topNext && linkedListCAS->topNext != cas) 
    linkedListCAS = linkedListCAS->topNext;

  linkedListCAS->topNext = cas;             /* add current cas to linked list */

  cas->_ptrEt = reqCAS->_ptrEt;    /* make CAS sched. pt be same as REQ */

  return;
}

/************************************************************************/
/* update top CAS for a child cas */
void
orbit_CAS_updateTop( CAS *cas) {
CAS *casCAS = cas->top;
char *argList;

  if ( !casCAS) return;

  /* divert *SEQDEF & TOP & CAS updates */

  switch (cas->_type) {

  case CASTYPE_CAS:
  case CASTYPE_CASDEF:
  case CASTYPE_CASOPNAV:
    orbit_CAS_updateReq( cas);

    if isCASOPNAV(*cas) {                  /* treat CASOPNAV as a single unit */
    SUBFRAGS *lclSubFrags;
    CAS **casPtr;
      FOROPNAVSUBFRAGS( cas, lclSubFrags, casPtr)
        (*casPtr)->top = cas;                                 /* just in case */
        orbit_CAS_updateTop( *casPtr);
      }}
    }

    return;

  case CASTYPE_REQ:
    fprintf( stderr, "Program Error:  orbit_CAS_updateTop, code %s\n"
                   , "WSNBATGH-REQ");
    fflush( stderr);
    return;

  default:
    break;
  }

  /* act according to parent type */

  switch ( casCAS->_type) {

  case CASTYPE_CAS:
    if ( argList = orbit_CAS_CASToArgs( cas)) {
      orbit_CAS_addTopArgs( casCAS, cas->_type, argList);
      free( argList);
    }
    break;

  case CASTYPE_CASDEF:
    orbit_CAS_updateCasDef( cas);
    return;

  case CASTYPE_CASOPNAV:     /* nothing to do, CASOPNAV children's parameters */
    break;                   /* are read-only                                 */

  default:
    break;
  }

  cas->_ptrEt = casCAS->_ptrEt;  /* make CAS scheduling pt be same as TOP CAS */

  return;
} /* orbit_CAS_updateTop( CAS *cas) { */

#define NEXTFRAGNAMEPFX "NEXTFRAGNAME"

/**********************************************************************/
/* write a file for all CAS below the highest CAS above the target CAS */

void
orbit_CAS_saveFile( char *filnam, CAS *targetCAS) {
#define INLINELEN 1024
char inLine[INLINELEN];
char *tokPtr[100];
FILE *f;
CAS *cas;                /* current cas */
CAS *cas0;               /* first cas, exit when we get back to it */
CAS *nextCAS;
char *dupStr;
char utc[UTCLEN];
long maxutclen = UTCLEN;
int topidx;
char *typeName;

  if ( !targetCAS) return;

  /* climb the ladder */
  for ( cas = targetCAS; cas->top; cas = cas->top) ;

  if ( !isANYTOP( *cas)) {
    fprintf( stderr, "Non-TOP-level CAS chosen:  no file written; try again\n");
    fflush( stderr);
    return;
  }

  cas0 = cas;

  if ( !(f = fopen( filnam, "w"))) {
    fprintf( stderr, "orbit_CAS_saveFile:  error opening file %s\n", filnam);
    fflush( stderr);
    return;
  }

  /* write out TOP* CAS info:
   *
   *   [REQ|CAS[DEF|OPNAV]],<name string>,<start delay>
   *   [REQ|CAS[DEF|OPNAV]]_SCHED_POINT
   *   <utc>
   *
   */
  fprintf( f, "%s,%s\n", typeName=orbit_CAS_FragToName( cas)
                       , dupStr=orbit_CAS_CASToArgs(cas));
  free( dupStr);
  orbit_et2doy( *cas->_ptrEt, utc, maxutclen);
  fprintf( f, "%s\n%s\n", isREQ( *cas) ? REQ_SCHED_POINT :
                         (isCAS( *cas) ? CAS_SCHED_POINT :
                         (isCASDEF( *cas) ? CASDEF_SCHED_POINT :
                         (isCASOPNAV( *cas) ? CASOPNAV_SCHED_POINT : 
                          "WSNBATGH-@")))
                        , utc);

  /* step to child & grandchild CASes, write them out */

  while (1) {
  int lastWasCAS=0;

    if isREQ(*cas) {                                    /* level 1 CAS ... */
      cas = cas->topNext;                          /* advance via topNext ... */
      if ( !cas) break;                                      /* break if null */

    } else {                                          /* level 2 or 3 CAS ... */
    long maxidx;
    CAS **baseCASptr;

      switch( cas->_type) {
      case CASTYPE_CAS:
        topidx = -1;                                    /* get current index, */
        maxidx = CASTYPE_COUNT;                        /* get index limit ... */
        baseCASptr = cas->casCasList;              /* & get CAS pointer array */
        break;
      case CASTYPE_CASDEF:
        topidx = -1;                                    /* get current index, */
        maxidx = MAXNUMSEQDEF;                         /* get index limit ... */
        baseCASptr = cas->casDefList;              /* & get CAS pointer array */
        break;
      case CASTYPE_CASOPNAV:
        topidx = 1;                                /* don't look for children */
        maxidx = 0;           
        break;
      case CASTYPE_NISSEQDEF:
        topidx = cas->nisTopIndex;
        maxidx = MAXNUMSEQDEF;
        baseCASptr = cas->top->casDefList;
        break;
      case CASTYPE_MSISEQDEF:
        topidx = cas->msiSeqDefTopIndex;
        maxidx = MAXNUMSEQDEF;
        baseCASptr = cas->top->casDefList;
        break;
      default:
        topidx = cas->_type;
        maxidx = CASTYPE_COUNT;
        baseCASptr = cas->top->casCasList;
        break;
      }

      for ( ++topidx; topidx<maxidx; ++topidx) {           /* search for next */
        if ( baseCASptr[topidx]) break;               /* non-null CAS pointer */
      }

      if ( topidx < maxidx) {       /* found a level 3 child of CAS or CASDEF */
        cas = baseCASptr[topidx];

      } else {                     /* no level 3 child of CAS or CASDEF found */
        if ( !isANYTOP(*cas)) {
          cas = cas->top;                            /* go to TOP CAS if done */
          if ( cas == cas0) break;   /* break out if we are at starting point */
        }
        cas = cas->topNext;                      /* go to topNext if not done */
        if ( !cas) break;                                    /* break if null */
      }

    } /* if isREQ ... else ... */

    if ( !isANYTOP(*cas)) 
      fprintf( f, "%s,%s\n", NEXTFRAGNAMEPFX, cas->_name);
    else
      fprintf( f, "\n\n");              /* extra lines before CAS, CASDEF, &c */

    /* write CAS type */
    fprintf( f, "%s,", orbit_CAS_FragToName( cas));

    if isANYTOP( *cas) {                           /* level 1 or 2 output ... */
      fprintf( f,"%s\n", dupStr=orbit_CAS_CASToArgs( cas));  /* & append args */
      free( dupStr);
      if ( !isREQ(*cas)) fprintf( f,"\n");     /* extra blank line after CAS* */

    } else {
    char *cptr;
      switch (cas->_type) {       /* level 3 output, get pointer from top CAS */
      case CASTYPE_MSISEQDEF:
        cptr = cas->top->casDefListPtrs[cas->msiSeqDefTopIndex];
        break;
      case CASTYPE_NISSEQDEF:
        cptr = cas->top->casDefListPtrs[cas->nisTopIndex];
        break;
      default:
        cptr = cas->top->casArgListPtrs[cas->_type];
        break;
      }
      fprintf( f, "%s\n", cptr);                      /* ... and print it out */
    }
  } /* while 1 */

  fprintf( f, "\n\n");
  orbit_writeSpiceOpts( f, (char *) 0, 0);  /* EOF => no need to write ENDSO: */

  fflush( f); fclose(f);
  return;
} /* orbit_CAS_saveFile( char *filnam, CAS *targetCAS) { */

/**********************************************************************/
/* write a file for all CAS below the highest CAS above the target CAS */

void
orbit_CAS_autoSaveReq( char *dirnam, CAS **foundCASes) {
char *pathname, *filnam;
char casname[IDLEN];
long lenDn, lenFn, lenRn; /* length dirname, filnam, REQ auto name */
#define lenTot (lenDn+lenFn+4+1)     /* dirnam + filnam + ".REQ" + terminator */
CAS **fCASes;       /* local founcCASes */
CAS *cas;                /* current cas */

  if ( !(fCASes=foundCASes)) return;

# define AUTOSAVEREQRTN free(foundCASes); return;
  if ( !dirnam) { AUTOSAVEREQRTN; }

  lenDn = strlen( dirnam);

  lenFn = 2 * IDLEN;          /* init allocation for filnam, should be enough */

  pathname = (char *) malloc( lenTot);
  if ( !pathname) { AUTOSAVEREQRTN; }

  strncpy( pathname, dirnam, lenDn+1);

  /* loop through REQs in fCASes */

  for ( cas=*(fCASes++); cas; cas=*(fCASes++)) if ( isREQ( *cas)) {

    *casname = '\0';
    sscanf( cas->_name, "%[A-Z_0-9a-z]", casname);           /* read autoname */

    /* check if casname starts with REQ__[A-Z_0-9a-z] */

    if ( (lenRn=strlen(casname)-5) > 0) if ( !strncmp( casname, "REQ__", 5)) {

      if ( lenRn > lenFn) {                               /* check allocation */
      char *oldPathname = pathname;                  /* in case realloc fails */
        lenFn = 2 * lenRn;                        /* leave room for expansion */
        if ( !(pathname=realloc( pathname, lenTot))) {
          free( oldPathname);
          AUTOSAVEREQRTN;
        }
      }
      strcpy( pathname+lenDn, casname+5);           /* append REQ's auto name */
      strcpy( pathname+lenDn+lenRn, ".REQ");                 /* append ".REQ" */
      fprintf( stderr, "%s\n", pathname);
      orbit_CAS_saveFile( pathname, cas);
    }
  } /* for cas=*(fCASes++) */

  if ( pathname) free( pathname);

  AUTOSAVEREQRTN;
}

/**********************************************************************/
/* read a file, translate lines into REQ, CAS, CASDEF & other FRAG's */

void
orbit_CAS_readFile_underTop( char *filnam, void *miscPtr, CAS *argTop
                           , void (*addToMiscPtr)(CAS *)) {
#define INLINELEN 1024
char inLine[INLINELEN];
char *tokPtr[100];
FILE *f;
CAS *casCAS = (CAS *) 0;
CAS *casDefCAS = (CAS *) 0;
CAS *casOpnavCAS = (CAS *) 0;
CAS *reqCAS = (CAS *) 0;
CAS *anycasCASlast = (CAS *) 0;
CAS *newCAS = (CAS *) 0;
void *mallocs[100];
int imalloc, nmalloc;
char *dupStr;
int ntok;
int casType;
char *ptr;
char *curNamePtr;
char curName[IDLEN];
char fragName[IDLEN];

  if ( argTop) {
    switch( argTop->_type) {
    case CASTYPE_REQ: reqCAS = argTop; break;
    case CASTYPE_CAS: casCAS = argTop; break;
    case CASTYPE_CASDEF: casDefCAS = argTop; break;
    case CASTYPE_CASOPNAV: casOpnavCAS = argTop; break;
    default: break;
    }
    miscPtr = argTop->_miscPtr;
  }
#define NONMALLOC( PTREXPR) mallocs[nmalloc++] = (void *) (PTREXPR)
#define MALLOC(PTR,SIZ,CAST) NONMALLOC( (PTR=(CAST *)malloc(SIZ)) )

  nmalloc = 0;

#ifdef RTNERR
#undef RTNERR
#endif
#define RTNERR { \
    for ( imalloc=0; imalloc<nmalloc; ++imalloc) { FREE( mallocs[imalloc]); } \
    return /* (CAS *) 0 */ ; \
  }

  f = fopen( filnam, "r");

  if ( !f) {
    fprintf( stderr, "orbit_CAS_readFile_underTop:  error opening file %s\n"
                   , filnam);
    RTNERR;
  }

  curNamePtr = dupStr = (char *) 0;
  *fragName = '\0';

  /* read a line e.g. "CAS,1997-178 // 07:43:56.132,..." */
  while ( fgets( inLine, INLINELEN, f)) {
  char *nl;
    inLine[INLINELEN-1] = '\0';
    if ( nl = strchr( inLine, '\n')) *nl = '\0';
    dupStr = strdup( inLine);                      /* make a copy of the line */
    ntok = orbit_commaParse( dupStr, tokPtr);               /* parse the line */
    if ( ntok > 0 )
      casType = orbit_CAS_NameToType( tokPtr[0]);   /* determine the CAS type */
    else casType = -1;

    if ( casType == -1 && ntok) {                            /* special tests */

      /* test for name field to be appended to CAS type string in ->_name
       * strings of subsequent CAS's
       * e.g.
       *   "NAME,3x3 Mosaic"     =>   ds40CAS->_name = "DS40 3x3 Mosaic"
       *   "NAME"                =>   use default name from orbit_CASnew()
       */
  
      if ( !strcmp( tokPtr[0], "NAME")) {
        if ( ntok > 1) {
          strncpy( curName, inLine + (tokPtr[1]-dupStr), IDLEN);
          curName[IDLEN-1] = '\0';
          curNamePtr = curName;          /* setup to use "current" name */
  
        } else curNamePtr = (char *) 0;  /* setup to use default name */

      /* read in next fragment's name */

      } else if ( !strcmp( tokPtr[0], NEXTFRAGNAMEPFX)) {
        if ( ntok > 1) {
          strncpy( fragName, inLine + (tokPtr[1]-dupStr), IDLEN);
          fragName[IDLEN-1] = '\0';
  
        } else curNamePtr = (char *) 0;  /* setup to use default name */

      /* read in Start Delay */

      } else if ( anycasCASlast &&
                 (!strcmp( tokPtr[0], TOP_START_DELAY)
                  || !strcmp( tokPtr[0], REQ_START_DELAY)
                  || !strcmp( tokPtr[0], CAS_START_DELAY)
                  || !strcmp( tokPtr[0], CASDEF_START_DELAY)
                  || !strcmp( tokPtr[0], CASOPNAV_START_DELAY)
                 )
                ) {
        if ( !strcmp( tokPtr[0], TOP_START_DELAY)
        || (!strcmp( tokPtr[0], REQ_START_DELAY) && isREQ(*anycasCASlast))
        || (!strcmp( tokPtr[0], CAS_START_DELAY) && isCAS(*anycasCASlast))
        || (!strcmp( tokPtr[0], CASDEF_START_DELAY) && isCASDEF(*anycasCASlast))
        || (!strcmp(tokPtr[0],CASOPNAV_START_DELAY)&&isCASOPNAV(*anycasCASlast))
           ){
        char c[20];
        double delay;
        int i;
          if ( fgets( c, 20, f)) {
            c[19] = '\0';
            if ( nl = strchr( c, '\n')) *nl = '\0';
            if ( sscanf( c, "%lf", &delay) == 1) 
              anycasCASlast->_delayStart = delay;
          }
        }

      /* convert UTC to ET */

      } else if ( anycasCASlast &&
                 (!strcmp( tokPtr[0], TOP_SCHED_POINT)
                  || !strcmp( tokPtr[0], REQ_SCHED_POINT)
                  || !strcmp( tokPtr[0], CAS_SCHED_POINT)
                  || !strcmp( tokPtr[0], CASDEF_SCHED_POINT)
                  || !strcmp( tokPtr[0], CASOPNAV_SCHED_POINT)
                 )
                ) {
        if ( !strcmp( tokPtr[0], TOP_SCHED_POINT)
        || (!strcmp( tokPtr[0], REQ_SCHED_POINT) && isREQ(*anycasCASlast))
        || (!strcmp( tokPtr[0], CAS_SCHED_POINT) && isCAS(*anycasCASlast))
        || (!strcmp( tokPtr[0], CASDEF_SCHED_POINT) && isCASDEF(*anycasCASlast))
        || (!strcmp(tokPtr[0],CASOPNAV_SCHED_POINT)&&isCASOPNAV(*anycasCASlast))
           ){
        char utc[UTCLEN];
          if ( fgets( utc, UTCLEN, f)) {
            if ( !anycasCASlast->top) {   /* update local ->_et, not argTop's */
              utc[UTCLEN-1] = '\0';
              if ( nl = strchr( utc, '\n')) *nl = '\0';
              orbit_utc2et( utc, anycasCASlast->_ptrEt);

              /* updated time - copy _name to _savename & 
               * call addToMiscPtr() again
               */
              strcpy( newCAS->_savename, newCAS->_name);
              addToMiscPtr( 
                !anycasCASlast->top 
                  ? anycasCASlast :
                  ( !anycasCASlast->top->top ? anycasCASlast->top 
                                             : anycasCASlast->top->top));
            } /* if !anycasCASlast->top */
          } /* if fgets */
        } /* if !strcmp ... */
      } /* if ... else if ... else if ... */
    } /* if casType == -1 && ntok */
 
    /**************************************************************************/
    while ( casType != -1) {   /* use while instead of if so we can break out */
    /**************************************************************************/

      if ( ntok == 1 && !feof(f)) { /* single CAS type on a line, append next */
        strcpy( inLine, tokPtr[0]);          /* - copy single token to inLine */
        ptr = inLine + strlen(inLine);            /* - append comma to inLine */
        *(ptr++) = ',';
        if ( fgets( ptr, INLINELEN-(ptr-inLine), f)) {    /* - read next line */
          inLine[INLINELEN-1] = '\0';                     /*   after that     */
          if ( nl = strchr( ptr, '\n')) *nl = '\0';
        }
      }

      /* convert arguments to CAS, 
       * overwriting existing child CAS if necessary, ...
       * *SEQDEF & TOP DEF & TOP TOP CAS'es will be made new because 
       * casCAS->casCasList pointers should always be null
       */

      newCAS = orbit_CAS_ArgsToCAS( inLine
              , ( casCAS 
               && (casType<CASTYPE_COUNT))         /* special test:  _CASOPNAV*/
                ? casCAS->casCasList[casType] : (CAS *)0);

      if ( !newCAS) {
        fprintf( stderr, "%s(): Problem reading %s arguments\n"
                       , "orbit_CAS_readFile_underTop"
                       , orbit_CAS_TypeToName(casType));
        break;
      }

      /* ... use current fragment name if it exists, reset fragment name ... */
      if ( *fragName) {

        strcpy( newCAS->_name, fragName);
        newCAS->_name[IDLEN-1] = '\0';
        *fragName = '\0';

      /* ... use change name to <TYPE> <curName> if name exists, ... */

      } else if ( curNamePtr) {
      char *typeName = orbit_CAS_TypeToName(casType);
        strcpy( newCAS->_name, typeName);
        strcat( newCAS->_name, " ");
        strncat( newCAS->_name, curNamePtr, IDLEN-strlen(newCAS->_name));
        newCAS->_name[IDLEN-1] = '\0';
      }

      /* ... add/update current non-TOP CAS to current casCAS if there is one,*/
      /*     ... or add current TOP* CAS to current reqCAS                 */

      /* switch ( casType) { */
      switch ( newCAS->_type) {               /* test this as casType might = */
                                               /* CASTYPE_COUNT + OPNAVTYPE_* */
      case CASTYPE_CAS:
        anycasCASlast = casCAS = newCAS;
        newCAS->top = reqCAS;
        break;

      case CASTYPE_CASDEF: 
        anycasCASlast = casDefCAS = newCAS;
        newCAS->top = reqCAS;
        break;

      case CASTYPE_CASOPNAV: 
        anycasCASlast = casOpnavCAS = newCAS;
        newCAS->top = reqCAS;
        break;

      case CASTYPE_REQ: 
        anycasCASlast = reqCAS = newCAS; 
        newCAS->top = (CAS *) 0;
        break;

      case CASTYPE_MSISEQDEF:
      case CASTYPE_NISSEQDEF:
        newCAS->top = casDefCAS;
        break;

      default:
        newCAS->top = casCAS;
        if ( casCAS) casCAS->casCasList[casType] = newCAS;
        break;
      }

      orbit_CAS_updateTop( newCAS);

      /* ... copy _name to _savename & add CAS to miscPtr */

      strcpy( newCAS->_savename, newCAS->_name);
      newCAS->_miscPtr = miscPtr;
      addToMiscPtr( newCAS);

      break;  /* out of while loop */

    /**************************************************************************/
    } /* while ( castype != -1) - break above ends here */
    /**************************************************************************/
    if ( dupStr) free( dupStr);
    dupStr = (char *) 0;

  } /* while ( fgets( inLine, INLINELEN, f)) { */

  fclose( f);

  return /* casCAS */;
} /* orbit_CAS_readFile_underTop( char *filnam, void *miscPtr, CAS *argTop, void (*addToMiscPtr)(CAS *)) { */

/*******************************/
/* read file, create new CASes */

void
orbit_CAS_readFile( char *filnam, void *miscPtr, void (*addToMiscPtr)(CAS *)) {
  orbit_CAS_readFile_underTop( filnam, miscPtr, (CAS *) 0, addToMiscPtr);
  return;
}

/*********************************/
/* find CAS that has target name */
CAS *
orbit_CAS_getCASByName( CAS *startCAS, char *target_name)
{
CAS *stepCAS = startCAS;

  for ( stepCAS = startCAS; stepCAS; stepCAS = stepCAS->next) {
    if ( !strcmp( stepCAS->_name, target_name)) break;
  }
  return stepCAS;
}

/****************************************************************/
/* provide automatic method to name REQ
 * REQ:XXX_DOYi
 * XXX = MSI/NIS/XGRS/OPNAV
 * DOY = day of year
 * i = designator a-z, A-Z
 */
void
orbit_CAS_autonameReq( CAS *req, long *oldDoy, long *idsgn
                      , void (*addToMiscPtr)(CAS *)) {
CAS *topNext;
long y, doy;
char lclc[1024], descr[1024];
#define ALPHABETSIZE (1L+'z'-'a')
static char dsgn[ALPHABETSIZE];
long i;
char utc[UTCLEN];
long maxutclen = UTCLEN;

  if ( !*dsgn) for ( i=0; i<ALPHABETSIZE; ++i) {
    dsgn[i] = 'a' + i;
  }

  if ( isREQ(*req)) {

    /* give REQ a unique name w/REQ__ prefix */

    /* - convert et to doy utc string, read DOY */

    orbit_et2doy( *req->_ptrEt, utc, maxutclen);
    if ( 2 == sscanf( utc, "%ld%*[- ]%ld", &y, &doy)) {

      /* - reset designator at start of new day */

      if (doy != *oldDoy) { *idsgn = 0; *oldDoy = doy; }

      /* - get descriptor from one of the child CASes */

      strcpy( descr, "");
      for ( topNext=req->topNext; topNext && !*descr
          ; topNext=topNext->topNext) {
        orbit_CAS_getCASKeyFromCas( topNext, descr);
      }
      if ( !*descr) strcpy( descr, "HUH");     /* default descr if none found */

      /* - combine parts into local string
       *   - append XXX suffix for addMiscPtr to mung,
       * - force termination at length IDLEN-1
       * - copy to ->_name, update miscPtr
       */
      sprintf( lclc, "REQ__%s_%3.3ld%c %4.4ld XXX", descr, doy, dsgn[(*idsgn)++]
                                                  , y);
      lclc[IDLEN-1] = '\0';
      strcpy( req->_name, lclc);
      addToMiscPtr( req);

    } /* if 2 == sscanf ... */
  } /* for ... if isREQ */
  return;
}

/****************************************************************/
/* provide automatic method to name REQs
 * REQ:XXX_DOYi
 * XXX = MSI/NIS/XGRS
 * DOY = day of year
 * i = designator a-z, A-Z
 */
void
orbit_CAS_autonameReqs( CAS **foundCASes, void (*addToMiscPtr)(CAS *)) {
CAS *stepCAS, *topNext;
CAS **fCASes;
long oldDoy;
long idsgn;

  if ( !foundCASes) return;

  oldDoy = -1;
  idsgn = 0;
  fCASes = foundCASes;
  for ( stepCAS=*(fCASes++); stepCAS; stepCAS = *(fCASes++)) {
    orbit_CAS_autonameReq( stepCAS, &oldDoy, &idsgn, addToMiscPtr);
  }
  return;
}

/************************************************************************/
/* find MSI Sequence Definition CAS that matches target sequence number */
/* - returns null (CAS *) for no match */
CAS *
orbit_CAS_getMSISeqDefByNum( CAS *startCAS, double et, long target_num)
{
CAS *stepCAS;
CAS *foundCAS = (CAS *) 0;

  for ( stepCAS = startCAS; stepCAS; stepCAS = stepCAS->next) {
    if ( stepCAS->_type == CASTYPE_MSISEQDEF && orbit_CAS_isEnabled(stepCAS))
    if ( stepCAS->msiSeqDefIdNum == target_num) {
      if ( CASSTART(stepCAS) <= et) {
        if ( foundCAS) {
          if ( CASSTART(stepCAS) > CASSTART(foundCAS)) foundCAS = stepCAS;
        } else foundCAS = stepCAS;
      }
    }
  }
  return foundCAS;
}

/************************************************************************/
/* find NIS Sequence Definition CAS that matches target sequence number */
/* - returns null (CAS *) for no match */
CAS *
orbit_CAS_getNISSeqDefByNum( CAS *startCAS, double et, long target_num)
{
CAS *stepCAS;
CAS *foundCAS = (CAS *) 0;

  for ( stepCAS = startCAS; stepCAS; stepCAS = stepCAS->next) {
    if ( stepCAS->_type == CASTYPE_NISSEQDEF && orbit_CAS_isEnabled(stepCAS))
    if ( stepCAS->nisIdNum == target_num) {
      if ( CASSTART(stepCAS) <= et) {
        if ( foundCAS) {
          if ( CASSTART(stepCAS) > CASSTART(foundCAS)) foundCAS = stepCAS;
        } else foundCAS = stepCAS;
      }
    }
  }
  return foundCAS;
}

/*********************************************************/
/* find latest CAS that covers a time and matches a type */
/* - force DS56 _delayStart to be 0
/* - returns null (CAS *) for no match */
CAS *
orbit_CAS_find( CAS *startCAS, int typeCAS, double et)
{
CAS *stepCAS = startCAS;
CAS *foundCAS = (CAS *) 0;

  while ( stepCAS) {
    if ( (stepCAS->_type == typeCAS) && orbit_CAS_isEnabled(stepCAS)) {
      if ( CASSTART(stepCAS) <= et) {
        if ( foundCAS) {
          if ( CASSTART(stepCAS) > CASSTART(foundCAS)) foundCAS = stepCAS;
        } else foundCAS = stepCAS;
      }
    }
    stepCAS = stepCAS->next;
  }
  return foundCAS;
}

/***************************/
/* peform ds56 on a vector */
void 
orbit_CAS_ds56Vec( VEC baseVec, CAS *startCAS, int aimptType, double et
                 , VEC vecout)
{
static double rpd;
CAS *foundCAS;
VEC delvec = { 0.0, 0.0, 0.0};
double et56, deltaEt, period, radrot;
int i, j;

#define NODS56_RTN { COPYBASE return; }

#define COPYBASE \
  if ( aimptType == Inadir) { \
  double cosel = cos( rpd * baseVec[0]); \
  double sinel = sin( rpd * baseVec[0]); \
  double cosaz = cos( rpd * baseVec[1]); \
  double sinaz = sin( rpd * baseVec[1]); \
    /* baseVec[0] is elevation (altitude) away from nadir, deg */ \
    /* baseVec[1] is azimuth clockwise from sun, deg */ \
    /* - nadir => X; sun az => Y; XxY => Z */ \
    vecout[0] = cosel; vecout[1] = sinel * cosaz; vecout[2] = sinel * sinaz; \
  } else { CPYVEC( baseVec, vecout); }

  if ( rpd == 0.0) rpd = atan(1.0) / 45.0;

  /* if no DS56 CAS' found, copy base vector to output & return */

  if ( !(foundCAS=orbit_CAS_find( startCAS, CASTYPE_DS56, et)) ) NODS56_RTN

  /* ensure et is not past the SCAN STOP command */

  et56 = CASSTART(foundCAS);
  if ( et > (et56 + foundCAS->ds56ScanDur)) NODS56_RTN

  /* ensure ds56 coordinate system matches aimpt */

  if ( foundCAS->ds56FrmType != aimptType) NODS56_RTN 

  /* calculate delta xyz for ds56 at time et */ 

#define BITTEST( BIT0, I) (foundCAS->ds56Bits & (BIT0<<(I))) 
#define REVERSE BITTEST( BITxCHG, i) 
#define PAUSELAST BITTEST( BITxPAUSE, i)
#define RATEDUR foundCAS->ds56RatePauseDur[0][i]
#define PAUSEDUR foundCAS->ds56RatePauseDur[1][i]
#define RATE foundCAS->ds56RateVec[i]

  for ( i=0; i<3; ++i) {
    deltaEt = et - et56;
    period = PAUSEDUR + RATEDUR;

    /* reduce deltaEt by period of scan/pause cycles */

    if ( REVERSE ) period *= 2;   /* double period for reverse case */
    if ( (deltaEt > period) && (period > 0.0)) {
    long nperiods = (long) (deltaEt / period);
      if ( !REVERSE) delvec[i] += (nperiods * RATE * RATEDUR);
      deltaEt -= (nperiods * period);

    } else if ( period <= 0.0) {
      deltaEt = 0.0;
    }

    /* loop through remaining scan-pause or pause-scan cycle(s)
     * - exit loop if remaining time (deltaEt) goes negative
     * - j is even for first part, odd for second
     * - do two cycles if REVERSE
     */
    for ( j=0; (j < (REVERSE?4:2) ) && (deltaEt > 0.0); ++j) {

      /* PAUSE - if j is even & pause is first or j is odd and pause is last
       *       - reduce deltaEt by pause duration
       */
      if ( (((j%2)==0) && !PAUSELAST) || (((j%2)==1) && PAUSELAST) ) {
        deltaEt -= PAUSEDUR;

      /* RATE - calculate time available, add product with rate to delvec 
       *        - subtract product from delvec on second pass (i.e. REVERSE)
       *      - reduce deltaEt by rate duration (ok to set deltaEt < 0) 
       */
      } else {
      double dEt = (deltaEt < RATEDUR) ? deltaEt : RATEDUR;

        if ( j > 1) delvec[i] -= dEt * RATE; /* odd cycle - reverse */
        else        delvec[i] += dEt * RATE;

        deltaEt -= RATEDUR;
      }
    } /* for j=0; j<(R?4:2) && (deltaEt > 0.0)... */

  }

  switch ( aimptType) {
  /* if ref frame origin is not S/C (i.e. not Nadir, SBF or J2K), translate 
   * baseVec i.e. add base + delta to get out
   */
  case Ieci:
  case Isci:
  case Iaci:
  case Iabf:
    VADD2( delvec, baseVec, vecout);
    break;

  /* else ref frame is Nadir, SBF or J2k, rotate baseVec around delvec */

  case Inadir:
    radrot = rpd * VLEN( delvec);
    COPYBASE                           /* convert el, az in baseVec to vecout */
    vrotv( vecout, delvec, &radrot, vecout);        /* rotate aimpt by radrot */
    break;

  case Ij2k:
    /************************
     * 20010115 - reversed sign
     *          - was:  radrot = - rpd * VLEN( delvec);
     */
    radrot = rpd * VLEN( delvec);     /*to rotate S/C by radrot around delvec,*/
    vrotv( baseVec, delvec, &radrot, vecout);      /* rotate aimpt by -radrot */
    break;

  case Isbf:
    radrot = rpd * VLEN( delvec);    /* baseVec is actually virtual boresight */
    vrotv( baseVec, delvec, &radrot, vecout);
    break;

  default:
    fprintf( stderr, "orbit_CAS_ds56Vec:  Program error; contact programmer, %s"
                   , "code WSNBATGH-0\n");
    fflush( stderr);
    NODS56_RTN
    break;
  }

  return;
}

/********************************************/
/* find ds40, return bore & roll structures */
void 
orbit_CAS_ds40Vec( POINTING *bore, POINTING *roll, CAS *startCAS, double et)
{
static double rpd;
CAS *foundCAS, *foundCASFULL, *foundCASXGRS;
static VEC nulvec = { 0.0, 0.0, 0.0};

#define NODS40_RTN { return; } 

  if ( rpd == 0.0) rpd = atan(1.0) / 45.0;

  /* look for DS40 or DS40FUll */

  foundCAS = orbit_CAS_find( startCAS, CASTYPE_DS40, et);
  foundCASFULL = orbit_CAS_find( startCAS, CASTYPE_DS40FULL, et);
  foundCASXGRS = orbit_CAS_find( startCAS, CASTYPE_DS40XGRS, et);

  if ( !foundCAS && !foundCASFULL && !foundCASXGRS) 
    NODS40_RTN                                    /* if no DS40 found, return */

  /* point foundCAS to foundDS40FULL 
   * - if foundCAS is null
   * - if foundCAS is earlier than foundCASFULL
   * then repeat for foundCAS wrt foundDS40XGRS
   */

  if ( !foundCAS) { foundCAS = foundCASFULL; }
  else if ( foundCASFULL) {
    if ( CASSTART(foundCAS) < CASSTART(foundCASFULL)) foundCAS = foundCASFULL;
  }

  if ( !foundCAS) { foundCAS = foundCASXGRS; }
  else if ( foundCASXGRS) {
    if ( CASSTART(foundCAS) < CASSTART(foundCASXGRS)) foundCAS = foundCASXGRS;
  }

  if ( !foundCAS) NODS40_RTN             /* we should not be able to get here */

  /* load the vectors */

  /* - aimpoint */
  if ( foundCAS->ds40AimptSelect == Iaim) {
    bore->aimpt.type = foundCAS->ds40AimptFrmType;
    CPYVEC( foundCAS->ds40AimptVec, bore->aimpt.vec);
  } else {
    bore->aimpt.type = foundCAS->ds40AimptSelect;
    CPYVEC( nulvec, bore->aimpt.vec);
  }

  /* - virtual boresight */
  bore->scvec.type = Iuser;
  CPYVEC( foundCAS->ds40VbVec, bore->scvec.vec);

  /* - s/c roll vector */
  roll->scvec.type = Iuser;
  CPYVEC( foundCAS->ds40ScRollVec, roll->scvec.vec);

  /* - roll reference vector */
  roll->aimpt.type = foundCAS->ds40RollFrmType;
  if ( roll->aimpt.type == Ij2k) {
    CPYVEC( foundCAS->ds40RollRefVec, roll->aimpt.vec);
  } else {
    CPYVEC( nulvec, roll->aimpt.vec);
  }
  
  return;
}

/****************************************/
/* generate a list of frames from a cas */
void 
orbit_gen_runCas( CAS *cas, ORBIT *inpOrbit, void *inpCur_item) {
void orbit_gen_msiShoot( CAS *, void *);
void orbit_gen_msiRep( CAS *, void *);
void orbit_gen_nisSU( CAS *, void *);
void orbit_gen_nisRep( CAS *, void *);
CAS *childCAS;
void *cur_item;

  if ( !cas) return;
  if ( !CASISENABLED( *cas)) return;

  /* get cur_item from inpOrbit if not NULL, else use inpCur_item */
  if ( !(cur_item=(inpOrbit?((SC *)inpOrbit->_sc)->_cur_item:inpCur_item)))
    return;

  switch ( cas->_type) {
  int i;
  SUBFRAGS *lclSubFrags;
  CAS **casPtr;
  case CASTYPE_REQ:
    for ( childCAS=cas->topNext; childCAS; childCAS=childCAS->topNext) {
      orbit_gen_runCas( childCAS, inpOrbit, cur_item);
    }
    break;
  case CASTYPE_CAS:
    for ( i=0; i<CASTYPE_COUNT; ++i) {
      orbit_gen_runCas( cas->casCasList[i], inpOrbit, cur_item);
    }
    break;
  case CASTYPE_CASOPNAV:
    FOROPNAVSUBFRAGS( cas, lclSubFrags, casPtr)
      orbit_gen_runCas( *casPtr, inpOrbit, cur_item);
    }}
    break;
  case CASTYPE_SHOOT:
    orbit_gen_msiShoot( cas, cur_item);
    break;
  case CASTYPE_MSITR:
  case CASTYPE_MSISR:
  case CASTYPE_MSIDR:
  case CASTYPE_MSIDSR:
    orbit_gen_msiRep( cas, cur_item);
    break;
  case CASTYPE_NISSU2:
  case CASTYPE_NISSU1:
    orbit_gen_nisSU( cas, cur_item);
    break;
  case CASTYPE_NISEX:
  case CASTYPE_NISSR:
  case CASTYPE_NISDR:
    orbit_gen_nisRep( cas, cur_item);
    break;
  default:                                        /* do nothing e.g. CASDEF */
    break;
  } /* switch cas->_type */
  return;
} /* orbit_gen_runCas( CAS *cas, ORBIT *inpOrbit, void *inpCur_item) { */
