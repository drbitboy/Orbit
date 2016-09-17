#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "orbit_cas.h"
#include "orbit_util.h"

enum {
  SASF_INIT=0L     /* beginning of file */
, SASF_BASE        /* after $$EO[HD] */
, SASF_REQNAME     /* after request( */
, SASF_REQPARMS    /* between request(REQUESTNAME, & ) */
, SASF_REQTIME     /* after START_TIME, */
, SASF_REQ         /* after request(...), revert to SASF_BASE after ) */
, SASF_ACTPARMS    /* between activity( & ) - revert to _REQ */
, SASF_ACTSCHPT    /* after SCHEDULED_TIME */
, SASF_ACTDELAYST  /* after SCHEDULED_TIME,\H:M:S\, */
, SASF_ACTCAS      /* between XYZ( & ) - revert to _ACTPARMS */
, SASF_MAKEIT      /* between XYZ( & ) - revert to _ACTPARMS */
, SASF_OPENQUOTE   /* after odd number of double quotes */
                   /* - revert to prev state after next double quote */
, SASF_COUNT };

/******************************/

void
orbit_CAS_sasf2CAS( char *filnam, void *miscPtr, void (*addToMiscPtr)(CAS *)) {
FILE *f;

char *cPtr;
#define MAXSTRLEN 1024
char inpStr[2*MAXSTRLEN];
char *inpStr2 = inpStr + MAXSTRLEN;

long iScanf, inpLen, isCtl;
long sepTotals[SEP_COUNT];
long sepBits, uniquePos;
long mainTotals[SEP_COUNT];
long mainBits;
int i;

char argList[MAXSTRLEN];

char *realTokPtr[100];
long realNTok;

CAS *req, *lclcas;
char reqName[MAXSTRLEN];
char reqUtc[MAXSTRLEN];

char quotedString[MAXSTRLEN];
char *qsPtr;

long reqAbsTime, activitySchedTime;
int reqRelTiming;

long state, prevState, tmpState;

  f = strcmp("-",filnam) ? fopen( filnam, "r") : stdin;

  if ( !f) {
    fprintf(stderr, "***Could not open file (%s)\n", filnam);
    fflush(stderr);
    return;
  }

/* use tmpState so CHGST(prevState) will work */

#define CHGST(NEWSTATE) tmpState=NEWSTATE; prevState=state; state=tmpState

  state = SASF_INIT;
  CHGST(SASF_INIT);

  while ( fgets( inpStr, MAXSTRLEN, f)) {
    inpStr[MAXSTRLEN-1] = '\0';
    if ( (cPtr=strchr( inpStr, '\n'))) *cPtr = '\0';
    *inpStr2 = '\0';
    if ( 1==sscanf( inpStr, "%s", inpStr2)) {
      if (!strcmp(inpStr2,"$$EOH")) break;
      if (!strcmp(inpStr2,"$$EOD")) break;
      if (!strcmp(inpStr2,"$$EOF")) { 
        if ( f != stdin) fclose(f);
        fprintf(stderr, "***Premature $$EOF in file (%s)\n", filnam);
        fflush(stderr);
        return;
      }
    }
  }

  fscanf( f, "%[ \n\t\r]", inpStr);   /* read whitespace */

  CHGST( SASF_BASE);

  *inpStr = *inpStr2 = '\0';
  while ( EOF != (iScanf=fscanf( f, "%[^,() \n\t\r]%[,() \n\t\r]"
                             , inpStr, inpStr2))) {

    if ( !iScanf) {
      fprintf(stderr, "Separators [,()] in whitespace in SASF file; exiting\n");
      fflush(stderr);
      break;
    }

    inpStr[MAXSTRLEN-1] = '\0';
    inpLen = strlen( inpStr);

    i = orbit_analyzeSeparator( inpStr, mainTotals, &mainBits);

    if ( iScanf == 2) {
      uniquePos = orbit_analyzeSeparator( inpStr2, sepTotals, &sepBits);
    } else {
      uniquePos = -1;
    }

    /* special step for odd number of quotes in inpStr */

    if ( (1L & mainTotals[SEP_QUOTES])) {
      if (state != SASF_OPENQUOTE) {                    /* init quoted string */
        qsPtr=quotedString;
        CHGST( SASF_OPENQUOTE);
      } else {
        /* next line may overflow inpStr */
        strcpy( qsPtr, inpStr);       /* copy last section of inpStr to qsPtr */
        strcpy( inpStr, quotedString); /* & copy quoted string back to inpStr */
        CHGST( prevState);
      }
    }

#if 0
    /*********************DEBUG******************/

    fprintf( stdout, "\n************\n%ld=fscanf, inpStr='%s' "
                   , iScanf, inpStr);

    fprintf( stdout, "trailer = '");

    if ( iScanf == 2) {

      for ( cPtr=inpStr2; *cPtr; ++cPtr) {
      char *cPtr2;
        cPtr2 = (*cPtr>=' ') ? cPtr : (*cPtr=='\n') ? "\\n"
                                    : (*cPtr=='\t') ? "\\t"
                                    : (*cPtr=='\r') ? "\\r" : "<?>" ;
        fprintf( stdout, (cPtr==cPtr2) ? "%.1s" : " " /* "%s" */, cPtr2);
      }
      fprintf( stdout, "'");

      fprintf( stdout, " 0x%04x", sepBits);
      for ( i=0; i<SEP_COUNT; ++i) {
        if ( i!=SEP_WS) if ( SEPBIT(i) & sepBits) {
          fprintf( stdout, uniquePos == i ? " unique count '%c' = %ld"
                                          : " '%c':%ld"
                         , sepChars[i], sepTotals[i]);
          if ( uniquePos != -1) break;
        }
      }
    } else {
      fprintf( stdout, "'");
    }
    fprintf( stdout, "\n");
    /****************DONEDEBUG**************/
#endif


#   define IFISSTART(STR) if ( (!*(STR) || (!strcmp(inpStr,STR)))
#   define IFIS(STR) IFISSTART( STR) )
#   define IFISUNQ(STR,UNIQUEPOS) \
    IFISSTART( STR) && iScanf==2 && uniquePos == UNIQUEPOS)
#   define IFCHGST(STR,UNIQUEPOS,NEWSTATE,SFX) \
    IFISUNQ(STR,UNIQUEPOS) { CHGST(NEWSTATE) SFX

    switch (state) {

    case SASF_OPENQUOTE:
      if ( (1L & mainTotals[SEP_QUOTES]) && (qsPtr!=quotedString)) {
        fprintf( stderr, "Error, code WSNBATGH-PARSESASF-QuotedStuff-0\n");
        fflush( stderr);
        CHGST( prevState);
      }
      sprintf( qsPtr, "%s", inpStr);
      qsPtr += strlen( qsPtr);
      if ( iScanf == 2) {
        sprintf( qsPtr, "%s", inpStr2);
        qsPtr += strlen( qsPtr);
      }

      if ( state != SASF_OPENQUOTE) {
        fprintf( stderr, "Ignoring quoted string (%s)\n", quotedString);
        fflush( stderr);
      }

      break; /* case SASF_OPENQUOTE */

    case SASF_BASE:                                   /* looking for request, */
      IFCHGST("request",SEP_OPENS, SASF_REQNAME, ; )
        req = NULL;
        strcpy( reqName, "");
        strcpy( reqUtc, "");
        reqAbsTime = 0;
        break;
      }
      break; /* case SASF_BASE */

    case SASF_REQNAME:                          /* looking for <REQUESTNAME>, */
      if ( iScanf==2 && uniquePos==SEP_COMMAS) {
        strcpy( reqName, inpStr);
        CHGST(SASF_REQPARMS);
        break;
      }
      break;

    case SASF_REQPARMS:                  /* looking for START_TIME, or <XYZ>) */
      IFCHGST("START_TIME",SEP_COMMAS, SASF_REQTIME, ; break; } )
      IFCHGST("",SEP_CLOSES, SASF_REQ, ; break; } )
      break;

    case SASF_REQTIME:                       /* looking for yyyy-doyThh:mm:ss */
      IFISUNQ( "", SEP_COMMAS) {
      long y,d,h,m,s;
        if ( sscanf( inpStr, "%ld - %ld T %ld : %ld : %ld", &y, &d, &h, &m, &s)
             == 5) {
          strcpy( reqUtc, inpStr);
          CHGST(SASF_REQPARMS);
          break;
        }
      }
      break;

    case SASF_REQ:                                   /* looking for activity( */
      IFCHGST( "activity", SEP_OPENS, SASF_ACTPARMS, ; )
        lclcas = NULL;
        break;
      }
                                                               /* or for end; */
      IFISSTART( "end;") && sepBits == SEPBIT(SEP_WS)) {
        CHGST(SASF_BASE);
        break;
      }
      break;

    case SASF_ACTPARMS:                         /* looking for SCHEDULED_TIME */

      IFCHGST( "SCHEDULED_TIME", SEP_COMMAS, SASF_ACTSCHPT, ; break; })

                                        /* or for FROM_PREVIOUS/REQUEST_START */
      IFISUNQ( "FROM_PREVIOUS_START", SEP_COMMAS) {
        reqRelTiming = 1;
        break;
      }
      IFISUNQ( "FROM_REQUEST_START", SEP_COMMAS) {
        reqRelTiming = 0;
        break;
      }
                                                               /* or for XYZ( */
      IFCHGST( "", SEP_OPENS, SASF_ACTCAS, ; )
        realTokPtr[0] = argList;
        realNTok = 0;
        break;
      }
                                                                 /* or for ), */
      if ( iScanf == 2
        && (sepBits & (SEPBIT(SEP_COMMAS)|SEPBIT(SEP_CLOSES))) ) {
        if ( sepTotals[SEP_CLOSES]==1 && sepTotals[SEP_COMMAS]==1) {
          CHGST( SASF_MAKEIT);
          break;
        }
      }
      break;

    case SASF_ACTSCHPT:                             /* looking for \hh:mm:ss\ */
      IFISUNQ( "", SEP_COMMAS) {
      long h,m,s;
      char *lclInpStr = inpStr;
      char c[20];

        if ( *inpStr=='-' || *inpStr=='+') lclInpStr++;        /* leading +/- */

        if ( 7 == sscanf( lclInpStr, "%[\\]%ld%[:]%ld%[:]%ld%[\\]"
                                   , c,  &h, c, &m,c, &s, c)) {
          CHGST( SASF_ACTPARMS);
          activitySchedTime = ((h *60) + m) * 60 + s;
          if ( *inpStr == '-') activitySchedTime *= -1;
          break;
        }
        if ( 3 == sscanf( lclInpStr, "%[\\]%ld%[\\]", c, &s,c)) {
          CHGST( SASF_ACTPARMS);
          activitySchedTime = s;
          if ( *inpStr == '-') activitySchedTime *= -1;
          break;
        }
      }
      break;

    case SASF_ACTCAS:       /* looking for activity "parameter," */
                                              /* or "parameter)" */
                                              /* or "parameter))" */
                                              /* or "parameter))," */

      if ( iScanf == 2 && (sepBits & (SEPBIT(SEP_COMMAS)|SEPBIT(SEP_CLOSES))) ){
      char *cPtr;

        /* add parameter to realTokPtr */

        strcpy( cPtr=realTokPtr[realNTok++], inpStr);
        realTokPtr[realNTok] = cPtr + inpLen + 1;

        /* if that was the first parameter, make adjustments
         * - increment token count for name (actual pointer will come later)
         * - add the sched time (always read by CAS/CASDEF/CASOPNAV)
         */
        if ( realNTok == 1) {

          /* - increment realNTok to make room for the name
           *   - point realTokPtr[2] at same place as realTokPtr[1] does now
           *     i.e. into argList string
           *     - realTokPtr[1] will point somewhere else entirely later
           */
          cPtr = realTokPtr[realNTok++];
          realTokPtr[realNTok] = cPtr + 1;

          if ( !reqRelTiming) {             /* convert to FROM_PREVIOUS_START */
            activitySchedTime -= reqAbsTime;
            reqAbsTime += activitySchedTime;
          }
          sprintf( cPtr=realTokPtr[realNTok++], "%ld", activitySchedTime);
          realTokPtr[realNTok] = cPtr + strlen(cPtr) + 1;
        }

        /* special case for one or two close parens followed by a comma */

        if ( sepTotals[SEP_CLOSES]==2 && sepTotals[SEP_COMMAS]==1) {
          CHGST( SASF_MAKEIT);                      /* assume order is ") )," */
          break;
        }
        if ( sepTotals[SEP_CLOSES]==1 && sepTotals[SEP_COMMAS]==1) {
          CHGST( SASF_ACTPARMS);                     /* assume order is ") ," */
          break;
        }
        break;
      }
      break;  /* case SASF_ACTCAS: */

    default:
      break;

    } /* switch ( state) { */

    *inpStr = *inpStr2 = '\0';

    /**************************************/
    /* create a CAS with current arg list */
    /**************************************/

    if ( state == SASF_MAKEIT) {
    int casType;
    long ilong;
    int j, i, cou;
    int ntok;
    char **tokPtr;
    char *tPtr, *nlPtr;
    long tlen;
    long rwargFailed = !0;                                  /* assume failure */

      lclcas = (CAS *) NULL;

      if (-1 != (casType=orbit_CAS_NameToType( realTokPtr[0])) ) {      
        lclcas = orbit_CAS_new( casType);

        if ( lclcas) switch (lclcas->_type) {
        /* case CASTYPE_CAS:
        case CASTYPE_CASDEF: */
        case CASTYPE_CASOPNAV:
          realTokPtr[1] = lclcas->_name;                /*put name after type */
          lclcas->_delayStart = activitySchedTime;      /* redundant for CAS* */

          ntok = realNTok;
          tokPtr = realTokPtr;
          break;

        default:       /* short-circuit orbit_cas_rwarg.h for all other types */
          ntok = 0;
          tokPtr = realTokPtr;
          break;
        }

        if ( lclcas && (ntok > 0)) {                          /* read the CAS */
#         define ISREAD 1
#         include "orbit_cas_rwarg.h" /* will clear rwargFailed if successful */
        }
      }

      if ( rwargFailed && lclcas) {        /* cleanup after orbit_cas_rwarg.h */
        orbit_CAS_freeCAS( lclcas);       /* failure or orbit_CAS_new failure */
        lclcas = (CAS *) NULL;
      }

      /* make new REQ only if args were successfully read into lclcas */

      if ( !req && lclcas) {
        fprintf( stderr, "\n***********\nREQ:  '%s'/'%s'\n", reqName, reqUtc);
        fflush( stderr);
        req = orbit_CAS_new( CASTYPE_REQ);

        if ( req) {
          strncpy( req->_name, reqName, IDLEN);
          req->_name[IDLEN-1] - '\0';
          strcpy( req->_savename, req->_name);
          orbit_utc2et( reqUtc, req->_ptrEt);
          req->_miscPtr = miscPtr;
          addToMiscPtr( req);
        } else {
          rwargFailed = !0;
        }
      }

      if ( !req || !lclcas) {                                      /* failure */
        if ( lclcas) {                   /* free lclcas only, leave req as is */
          orbit_CAS_freeCAS( lclcas);
          lclcas = (CAS *) NULL;
        }

      } else {                                                     /* success */

        lclcas->top = req;
        orbit_CAS_updateTop( lclcas);
        strcpy( lclcas->_savename, lclcas->_name);
        lclcas->_miscPtr = miscPtr;
        addToMiscPtr( lclcas);

        while ( realNTok-- > 0) {
          fprintf( stdout, "%c%s", lclcas ? ',' : '\0', realTokPtr[realNTok]);
        }
        fprintf( stdout, "\n");
      }

      state = SASF_REQ;

    } /* if ( state == SASF_MAKEIT) { */

  } /* while EOF != fscanf */

  if ( f != stdin) fclose( f);
  return;
}
