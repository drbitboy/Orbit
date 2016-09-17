/* orbit_cas_sasf.c - convert ORBIT REQs and CASes to seqgen SASF files */
#include <string.h>
#include <stdio.h>
#include <malloc.h>
#include "orbit_cas.h"
#include "orbit_util.h"
#include "debug.h"

/***********************************************************************/
/* return cas bits that correspond to which fragments the cas contains */

long orbit_CAS_getCASBits( CAS *cas) {
long bits = 0;
long i;

  switch ( cas->_type) {
  case CASTYPE_CASOPNAV:
  case CASTYPE_MSISEQDEF:
  case CASTYPE_NISSEQDEF:
    bits = (1L<<cas->_type);
    break;

  case CASTYPE_CAS:                         /* for CAS, OR bits of child cas' */
    for ( i=0; i<CASTYPE_COUNT; ++i) {
      if ( cas->casArgListPtrs[i]) bits |= (1L<<i);
    }
    break;

  case CASTYPE_CASDEF:    /* for CASDEF, return bits of first *SEQDEF in list */
    for ( i=0; !bits && (i<MAXNUMSEQDEF); ++i) {
      if ( cas->casDefList[i] && cas->casDefListPtrs[i]) {
        bits = orbit_CAS_getCASBits( cas->casDefList[i]);
      }
    }
    break;

  default:
    break;
  }
  return bits;
}

/***************************************************************************/
/* return ptr to element of static CASDESCR array that matches cas members */
/* returns NULL for no match */

CASDESCR *orbit_CAS_getCASDescr( CAS *cas) {
long bits;
CASDESCR *lclcasDescr;
long subType;

static CASDESCR opnavDescr[OPNAVTYPE_COUNT];

  switch (cas->_type) {

  case CASTYPE_CASOPNAV:

    subType=cas->opnavSubType;
    if ( subType<0 || subType >= OPNAVTYPE_COUNT) break;

    lclcasDescr = opnavDescr + subType;

    if ( !lclcasDescr->_casBits || !lclcasDescr->_shortName 
         || !lclcasDescr->_sasfName) {
      lclcasDescr->_shortName = opnavNames[subType] + 4;
      lclcasDescr->_sasfName = opnavNames[subType];

      /* combination of CASBITS_OPNAV & subType bit will be unique 
       * as long as CASTYPE_CASOPNAV (30) >= OPNAVTYPE_COUNT (18)
       * even if we blend casDescr with opnavDescr later
       */
      lclcasDescr->_casBits = CASBITS_CASOPNAV | (1L<<subType);
    }
    return lclcasDescr;
    break;

  default:                                                     /* CAS, CASDEF */

    bits = orbit_CAS_getCASBits( cas);
    for ( lclcasDescr = casDescr; lclcasDescr->_casBits; ++lclcasDescr) {
      if ( bits == lclcasDescr->_casBits) return lclcasDescr;
    }
    break;
  }
  return (CASDESCR *) 0;
}

/**********************************************/
/* convert from CAS descriptor to requestor/key
 * - copy from 5th position (after CAS_) from the CASDESCR member _seqgenName
 *   and continuing until the end of string, an underscore (_), or
 *   MAXCASKEYLEN characters
 */
void orbit_CAS_getCASKeyFromDescr( CASDESCR *lclcasDescr, CASKEY returnKey) {
char *cptr;
  if ( !lclcasDescr) {
    *returnKey = '\0';
  } else {
    strncpy( returnKey, lclcasDescr->_sasfName+4, MAXCASKEYLEN); /* skip CAS_ */
    returnKey[MAXCASKEYLEN] = '\0';                /* ensure null termination */
    if ( (cptr = strchr( returnKey,  '_'))) *cptr = '\0'; /* terminate at "_" */
    for ( cptr=returnKey; *cptr; ++cptr) {           /* convert to upper case */
      *cptr = toupper( *cptr);
    }
  }
  return;
}

/**************************************************************************/
/* return requestor/key of a cas - see orbit_CAS_getCASKeyFromDescr above */
void orbit_CAS_getCASKeyFromCas( CAS *cas, char *returnKey) {
/* CASDESCR *lclcasDescr; */
char *cptr;
  orbit_CAS_getCASKeyFromDescr( orbit_CAS_getCASDescr( cas), returnKey);
  return;
}

/*****************************************************/
/* utility routine for orbit_CAS_skipDelayArgOffset) 
 * - see comments in that routine (below)
 */
static char *
orbit_CAS_skip1Arg( char *offstr) {
  while ( (*offstr != ',') && *offstr) {  /* look for comma or terminator */
    ++offstr;
  }
  if ( *offstr) ++offstr;                         /* increment past comma */
  return offstr;
}

/*****************************************************************************/
/* return a pointer offset to the location after the _delayStart arg in a
 * string containing CASToArgs for one fragment (offset = 0 * for MSI_SHOOT,
 * after first comma for all others)
 */
char *
orbit_CAS_skipDelayArgOffset( CAS *cas, int casType) {
char *offstr;

  switch ( cas->_type) {
  case CASTYPE_CAS:
    offstr = cas->casArgListPtrs[casType];
    break;
  case CASTYPE_CASDEF:
    offstr = cas->casDefListPtrs[casType]; /* casType is index, not type */
    break;
  default:
    fprintf( stderr
           , "orbit_CAS_skipDelayArgOffset warning:  this routine was called");
    fprintf( stderr, " for a REQ or FRAGMENT (%s)\n", cas->_name);
    fflush( stderr);
    return (char *) 0;
    break;
  }

  if ( offstr && (!isCAS( *cas) || casType != CASTYPE_SHOOT)) {
    offstr = orbit_CAS_skip1Arg( offstr);
  }
  return offstr;
}

/***************************************/
/* convert REQ or CAS into SASF format
 * - copy result to strdup'ed string
 * - controlled by actNumNext (next activity number):
 *   actNumNext = 0  cas is REQ, init REQ i.e. start new activity, return 1
 *   actNumNext > 0  cas is CAS, output CAS_* arguments, return actNumNext+1
 *   actNumNext < 0  cas is null, end activity, return 0
 * - return -1 (for REQ) or -actNumNext on error
 */
long orbit_CAS_sasfString( char **outPtr, CAS *cas, long actNumNext, int idx) {
char key[MAXCASKEYLEN+1];
char outStr[1024];
CAS *topNext;
long bits, i, ds, dsNeg, delayH, delayM, delayS;
char reqName[IDLEN];
char *cptr, *casArgs;
char startTime[UTCLEN];
void orbit_et2doy( double, char *, long);
CASDESCR *lclcasDescr;

  *outPtr = (char *) 0;                                     /* assume failure */

  if ( actNumNext == 0) {                             /* initialize REQUEST */
    if ( !isREQ( *cas)) {
      fprintf( stderr
             , "***orbit_CAS_sasfString:  Program error 0; contact programmer\n");
      fflush( stderr);
      return -1;
    }

    /* find CAS under REQ that can be used to get requestor & key (e.g. MSI) */
    *key = '\0';
    topNext = cas->topNext;

    /* new code, go through all CAS under REQ, OPNAV overrides anything */

    while ( topNext) {
    char tmpKey[MAXCASKEYLEN+1];
      *tmpKey = '\0';
      orbit_CAS_getCASKeyFromCas( topNext, tmpKey);
      if ( *tmpKey) 
        if ( !*key || !strcmp( tmpKey, "OPNAV")) 
          strcpy( key, tmpKey);
      topNext = topNext->topNext;
    }

    /* old code
    /* while ( !*key && topNext) {
    /*   orbit_CAS_getCASKeyFromCas( topNext, key);
    /*   if ( !*key) topNext = topNext->topNext;
    /* }
    /**/

    if ( !*key) strcpy( key, "???");                  /* something is needed */

    /* look for special name e.g. REQ__MSI_143a ... */

    *reqName = '\0';
    sscanf( cas->_name, "%[A-Z_0-9a-z]", reqName);

    if ( !strncmp( "REQ__", cas->_name, 5) && (strlen(reqName) > 5)) {
        sscanf( cas->_name+5, "%[A-Z_0-9a-z]", reqName);
    } else {
      /* replace spaces et al with "_" in name; convert et to utc (doy) */
      strcpy( reqName, cas->_name);
      for ( cptr=reqName; *cptr; ++cptr) if ( *cptr <= ' ') *cptr = '_';
    }
    orbit_et2doy( *cas->_ptrEt, startTime, UTCLEN-1);

    sprintf( outStr
           , "request(%s,\n\t\tSTART_TIME, %s,\n\t\tREQUESTOR, \"%s\",\n\t\tPROCESSOR, CTP1,\n\t\tKEY, \"%s\")\n\n"
           , reqName, startTime
           , key, key
           /*strcmp(key,"OPNAV") ? key : "NAV"         /* change OPNAV to NAV */
           /*strcmp(key,"OPNAV") ? key : "NAV"         /*  "                  */
           );

    actNumNext = 1;
  } /* if actNumNext == 0 */
  /****************************************************************************/
  else if ( actNumNext > 0) {                            /* activity i.e. CAS */

    switch (cas->_type) {
    case CASTYPE_CAS:             /* OLD:  if ( isCAS( *cas)) { */

      lclcasDescr = orbit_CAS_getCASDescr( cas);
      break;

    case CASTYPE_CASDEF:          /* OLD:  } else if ( isCASDEF( *cas)) { */

      if ( cas->casDefList[idx] && cas->casDefListPtrs[idx]) {
        lclcasDescr = orbit_CAS_getCASDescr( cas->casDefList[idx]);
      } else {
        return -actNumNext;
      }
      break;

    case CASTYPE_CASOPNAV:

      lclcasDescr = orbit_CAS_getCASDescr( cas);
      break;

    default:                      /* OLD:  } else { */

      fprintf( stderr
             , "***orbit_CAS_sasfString:  Program error 1; contact programmer\n"
             );
      fflush( stderr);
      return -actNumNext;    /* return activity number unchanged in abs value */

      break;
    } /* switch (cas->_type) */

    if ( !lclcasDescr) {
      fprintf( stderr
             , "***orbit_CAS_sasfString:  Non-standard CAS (%s) %s\n"
             , cas->_name, "will not be saved");
      fflush( stderr);
      return actNumNext;                  /* return activity number unchanged */
    }

    orbit_CAS_getCASKeyFromDescr( lclcasDescr, key);
    if ( !*key) strcpy(key, "???");

    ds = (long) cas->_delayStart;         /* set CAS delay wrt prev activity  */

    if ( isCASDEF(*cas) && ds) {      /* in CASDEF, only 1st SEQDEF has delay */
    int i;
      for ( i=idx-1; i>=0; --i) {
        if ( cas->casDefList[i]) {     /* there is another SEQDEF before idx, */
          ds = 0;                                       /* set delay to 0 ... */
          break;                                     /* and break out of loop */
        }
      }
    }
    if ( ds < 0 ) { ds = -ds; dsNeg = 1; } else dsNeg = 0;
    delayS = ds % 60L;
    delayM = (ds % 3600L) / 60L;
    delayH = ds / 3600;

    sprintf( outStr
           , "\tactivity(%ld,\n\t\tSCHEDULED_TIME,\\%s%ld:%ld:%ld\\,FROM_PREVIOUS_START,\n\t\t%s(%s"
           , actNumNext, dsNeg ? "-" : "", delayH, delayM, delayS
           , strcmp(key, "OPNAV") ? key : "MSI"     /* change function to MSI */
           , lclcasDescr->_sasfName
           );

#   define APPENDOFFPTR \
    if ( *offptr) { \
      strcat( outStr, ","); \
      strcat( outStr, offptr); \
    }

    switch( cas->_type) {
    char *offptr;
    char *opnavArgStr;

    case CASTYPE_CAS:                                                  /* CAS */

      for ( i=0; i< CASTYPE_COUNT; ++i) {
        if ( (offptr=orbit_CAS_skipDelayArgOffset(cas,(int) i)) ) {
          APPENDOFFPTR
        }
      }
      break;

    case CASTYPE_CASDEF:                          /* CASDEF - do SEQDEF # idx */

      if ( (offptr=orbit_CAS_skipDelayArgOffset(cas, idx)) ) {
          APPENDOFFPTR
      } else return actNumNext;

      break;

    case CASTYPE_CASOPNAV:                /* CASOPNAV - lots of special stuff */
      if ( (opnavArgStr=orbit_CAS_CASToArgs( cas))) {     /* strdup'ed string */
        offptr = orbit_CAS_skip1Arg( opnavArgStr);               /* skip name */
        offptr = orbit_CAS_skip1Arg( offptr);             /* skip _delayStart */
        APPENDOFFPTR
        free( opnavArgStr);                          /* free strdup'ed string */
      }
      break;

    } /* switch cas->_type */

    strcat( outStr, ")\n\t),\n");
    actNumNext++;                                /* increment activity number */
  /****************************************************************************/
  } else {                                     /* actNumNext<0 => REQUEST end */
    sprintf( outStr, "end;\n\n");
    actNumNext = 0;
  }
  *outPtr = strdup( outStr);                     /* duplicate string & return */
  return actNumNext;
} /* orbit_CAS_sasfString( char **outPtr, CAS *cas, long actNumNext, int idx) */

/****************************************/

void 
orbit_CAS_saveReqToSasfFile( FILE *f, CAS *req) {
CAS *cas;
char *dupStr;
long actNumNext;

  if ( !isREQ( *req)) {
    fprintf( stderr, "Non-REQ passed to orbit_CAS_saveReqToSasfFile\n");
    fflush( stderr);
    return;
  }

  actNumNext = 0;

# define PRINTSTR(S) fprintf( f, "%s", S)
# define PRINTDUP if ( dupStr) { PRINTSTR(dupStr); free( dupStr); }

  for ( cas=req; cas; cas = cas->topNext) {
    /* actNumNext will increment on success, writing CASes */
    if ( isCASDEF( *cas)) {
    int i;
      for ( i=0; i<MAXNUMSEQDEF; ++i) {
        if ( cas->casDefList[i] && cas->casDefListPtrs[i]) {
          if ( (actNumNext=orbit_CAS_sasfString( &dupStr, cas, actNumNext, i))
               > 0 ) {
            PRINTDUP
          } else {
            actNumNext = - actNumNext;
            fprintf( f, "\n* ORBIT:  ERROR WRITING SEQDEF %s\n"
                           , cas->casDefList[i]->_name);
          }
        }
      }
    } else 
    if ( (actNumNext=orbit_CAS_sasfString( &dupStr, cas, actNumNext,0)) > 0) {
      PRINTDUP
    } else if ( cas != req && actNumNext < 0) {                /* if < 0 ... */
      actNumNext = - actNumNext;                      /* ... bypass CAS error */
      fprintf( f, "\n* ORBIT:  ERROR WRITING CAS %s\n"         /* and flag it */
             , cas->_name);                                   /* in SASF file */
    }
    if ( actNumNext<1) {                                /* break on REQ error */
      fprintf( stderr, "orbit_CAS_saveReqToSasfFile failure\n");
      fflush( stderr);
      break;
    }
  }
  orbit_CAS_sasfString( &dupStr, (CAS *) 0, -1, 0);           /* activity end */
  PRINTDUP
  return;
}

/*********************************************************************/

FILE *
orbit_CAS_sasfOpenFile( char *filnam, int openForRead) {
FILE *f = (FILE *) NULL;

  if ( !strcmp( "-", filnam ? filnam : "-")) {
    f = openForRead ? stdin : stdout;
  } else if ( !(f=fopen(filnam,openForRead?"r":"w")) ) {
    fprintf( stderr, "orbit_CAS_sasfOpenFile:  error opening file %s for %s\n"
                   , filnam, openForRead ? "reading" : "writing");
    fflush( stderr);
    return f;
  }

  if ( !openForRead) {                           /* write header to sasf file */
  char *host, *ct, *gecos;

    orbit_utilGetHostCtimeGecos( &host, &ct, &gecos);
    fprintf( f,
"CCSD3ZF0000100000001NJPL3KS0L015$$MARK$$;\n\
MISSION_NAME = NEAR;\n\
SPACECRAFT_NAME = NEAR;\n\
DATA_SET_ID = SPACECRAFT_ACTIVITY_SEQUENCE;\n\
FILE_NAME = %s;\n\
PRODUCT_CREATION_TIME = %s;\n\
PRODUCER_ID =  ORBIT;\n\
SEQ_ID = orbitseq;\n\
HOST_ID = %s;\n\
CCSD3RE00000$$MARK$$NJPL3IF0M01300000001;\n\
$$NER       SPACECRAFT ACTIVITY SEQUENCE FILE\n\
************************************************************\n\
*PROJECT    NER\n\
*SPACECRAFT 93\n\
*OPERATOR   %s\n\
*FILE_CMPLT TRUE\n\
*DATE       %s\n\
*ORBIT      V0\n\
*TITLE      orbitseq\n\
"
# define STR(A) ((A) ? (A) : "<allocation error>")
    , filnam, STR(ct), STR(host), STR(gecos), STR(ct));
    if ( host) free( host);
    if ( ct) free( ct);
    if ( gecos) free( gecos);

    orbit_writeSpiceOpts( f, "*", 1);   /* write SPICESPEC stuff, incl ENDSO: */

    fprintf( f, "$$EOH\n$$EOD\n");

  } /* if !openForRead */

  return f;
}

/*********************************************************************/

void
orbit_CAS_sasfCloseFile( FILE *f, int openForRead) {
  if ( !openForRead) {
    fprintf( f, "$$EOF\n");
    if ( f != stdout) fclose(f);
  } else {
    if ( f != stdin) fclose(f);
  }
  return;
}

/*********************************************************************/

void 
orbit_CAS_saveSasfFileOneOrAll( char *filnam, CAS *targetCAS, int saveAll) {
FILE *f;
CAS *req;
int openForRead;

  if ( !targetCAS) return;

  for ( req=targetCAS; req->top; req = req->top) ;         /* find REQ parent */
  if ( !isREQ( *req)) {
    fprintf( stderr, "Non-REQ chosen:  no file written; try again\n");
    fflush( stderr);
    return;
  }

  if ( !(f = orbit_CAS_sasfOpenFile( filnam, openForRead=0))) {
    return;
  }

  while ( req) {
    orbit_CAS_saveReqToSasfFile( f, req);

    if ( saveAll) {              /* if we're saving everything, find next REQ */
      for ( req=req->next; req; req=req->next) {
        if ( isREQ( *req)) break;
      }
    } else {
      req = (CAS *) 0;     /* else set req to NULL so while(req) loop ends */
    } /* if saveAll */
  } /* while req */

  orbit_CAS_sasfCloseFile( f, openForRead);
  return;
}

orbit_CAS_saveSasfFileOneReq( char *filnam, CAS *targetCAS) {
  orbit_CAS_saveSasfFileOneOrAll( filnam, targetCAS, 0);
}

orbit_CAS_saveSasfFileAllReq( char *filnam, CAS *targetCAS) {
  orbit_CAS_saveSasfFileOneOrAll( filnam, targetCAS, 1);
}
