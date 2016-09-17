/* orbdbMrg.c - merge 1 or more ORBit DataBases
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef vms
char *malloc();
char *realloc();
#else
#include <malloc.h>
#endif

#include "orbdb.h"

/********************************/

int
main(int argc, char **argv) {

metCONTENT *mC;
metKEY *mK, *mK2;
long *pltNums = (long *) 0;
long *lPtr;
long numPltKeys;

long outNumPlates, inpNumPlates;
int iArgc;

char inpName[1024];
char inpLine[1024];

#define USAGE "\n\
Usage:  orbdbMrg outPutOrbdb [-f inputOrbdbListFile | inpOrbdb1 [...]]\n\
"

FILE *fInpList = (FILE *) 0;

char *fnOutOrbdb = (char *) 0;
char *fnInpOrbdb = (char *) 0;
ORBDB_FILES *inpOfs = (ORBDB_FILES *) NULL;
ORBDB_FILES *outOfs = (ORBDB_FILES *) NULL;
ORBDB_STATE inpState;

#define CLOSEALLUSGRTN(I) fprintf( stderr, "%s", USAGE);  CLOSEALLRTN(I)
#define CLOSEALLRTN(I)    CLOSEALL;  return(I)
#define CLOSEALL          CLOSEINPOFS  CLOSEINPOFS  CLOSEINPLIST  FREENUMS
#define FREENUMS          if ( pltNums) free(pltNums)
#define CLOSEINPOFS \
  if ( inpOfs) { orbdb_close(inpOfs); inpOfs = (ORBDB_FILES *)0; }
#define CLOSEOUTOFS \
  if ( outOfs) { orbdb_close(outOfs); outOfs = (ORBDB_FILES *)0; }
#define CLOSEINPLIST \
  if ( fInpList) { fclose(fInpList); fInpList = (FILE *)0; }

  if ( (iArgc=2) >= argc) { CLOSEALLUSGRTN(-1); }
  fnOutOrbdb = argv[1];
  if ( !*fnOutOrbdb) { CLOSEALLUSGRTN(-1); }
  if ( !(outOfs=orbdb_openForWrite( fnOutOrbdb, (ORBDB_FILES *) NULL)) ) {
    CLOSEALLUSGRTN(-1);
  }
  outNumPlates = orbdb_getNumPlates( outOfs);
  if ( outNumPlates < 0) {
    fprintf( stderr, "***Problem getting number of plates in ORBDB files '%s'%s"
                   , fnOutOrbdb, "; exiting\n");
    CLOSEALLUSGRTN(-1);
  }

  while ( iArgc < argc || fInpList) {           /* loop through all arguments */

    strcpy( fnInpOrbdb=inpName, "");

    /* loop until fnInpOrbdb points at a non-null string 
     * - hopefully an ORBDB prefix
     */
    while ( !*fnInpOrbdb) {

      strcpy( fnInpOrbdb=inpName, "");                     /* set for failure */

      /* test if we are reading from a list file - one orbdb prefix per line */

      if ( fInpList) {
      char *cPtr;

        /* if eof or other error reading fInpList, close fInpList */

        if ( !fgets( inpLine, 1023, fInpList)) { 
          if ( iArgc >= argc) { CLOSEALLRTN(0); } /* exit now if no more args */
          CLOSEINPLIST
        } else {
          sscanf( inpLine, "%1023s", inpName);        /* else scan for prefix */
          if ( *inpName == '!' ) *inpName = '\0';     /* ignore comment lines */
        }
        continue;  /* for eof OR scan, jump to end of while !*fnInpOrbdb loop */
      }

      /* not reading from a list file, check argument(s)
       * - test for new -f <inputOrbdbListFile>
       */
      if ( !strcmp( "-f", argv[iArgc])) {
        if ( ++iArgc >= argc) { CLOSEALLUSGRTN(-1); }
        if ( !(fInpList=fopen(argv[iArgc],"r"))) {
          fprintf( stderr, "***Problem opening list file '%s'; exiting\n"
                         , argv[iArgc]);
          CLOSEALLUSGRTN(-1);
        }
        iArgc++;
        continue;     /* jump to end (but not out) of while !*fnInpOrbdb loop */
      } /* if !strcmp "-f" */

      fnInpOrbdb = argv[iArgc++];         /* - argument contains ORBDB prefix */

    } /* while !*fnInpOrbdb */

    /* non-null prefix of input ORBDB files
     * - open them
     */
    if ( !(inpOfs=orbdb_openForRead( fnInpOrbdb, (ORBDB_FILES *) NULL)) ) {
      fprintf( stderr, "***Problem opening ORBDB files '%s'; exiting\n"
                    , fnInpOrbdb);
      CLOSEALLUSGRTN(-1);
    }
    fprintf( stderr, "INFO:  opened ORBDB '%s'\n", fnInpOrbdb);
    
    /* - test number of plates */

    inpNumPlates = orbdb_getNumPlates( inpOfs);
    if ( inpNumPlates < 0) {
      fprintf( stderr, "***Problem getting # of plates in ORBDB files '%s'%s\n"
                     , fnInpOrbdb, "; exiting");
      CLOSEALLUSGRTN(-1);
    }
    if ( inpNumPlates == 0 ) {
      fprintf( stderr, "***Plates not set in input ORBDB files '%s'%s\n"
                     , fnInpOrbdb, "; exiting");
      CLOSEALLUSGRTN(-1);
    }
    if ( outNumPlates == 0) {   /* set number of plates in output ORBDB files */
      outNumPlates = inpNumPlates;
      fprintf( stderr, "INFO:  setting # of plates to %ld\n", outNumPlates);
      orbdb_setNumPlates( outOfs, outNumPlates);
    }
    if ( outNumPlates != inpNumPlates) {
      fprintf( stderr, "***Mismatch in number of plates in ORBDB files '%s'%s\n"
                     , fnInpOrbdb, "; exiting");
      CLOSEALLUSGRTN(-1);
    }

    if ( !pltNums ) {
      if ( !(pltNums=(long *) malloc( sizeof(long) * outNumPlates)) ) {
        fprintf( stderr, "***Problem allocating %ld plate numbers; exiting/n"
                       , outNumPlates);
        CLOSEALLUSGRTN(-1);
      }
    }

    /* step through all MET keys in input ORBDB files */

    mK = orbdb_metFirstkey( inpOfs, &inpState);              /* first MET key */
    while ( mK) {                                        /* loop through keys */
    pltKEY *pK;

      mC = orbdb_fetchMetContentByKey( inpOfs, mK);        /* get MET content */

      if ( !mC) {
        fprintf( stderr
               , "***No content returned by orbdb_fetchMetContentByKey()\n");
        fprintf( stderr
               , "   ORBDB name/pfx = '%s'; exiting\n", fnInpOrbdb);
        CLOSEALLRTN(-1);
      }
                                            /* unload plate keys into pltNums */
      for ( lPtr=pltNums, pK=mC->_visPlates
          ; pK<(mC->_visPlates+mC->_nVisPlates)
          ; ++pK, ++lPtr) {
        *lPtr = orbdb_unloadPltKey(pK);
      }
                                        /* MET content + plate keys => output */
      orbdb_addMet( outOfs, mC, pltNums);             /* i.e. here's the beef */

      free( mC);
      mK2 = mK;
      mK = orbdb_metNextkey( inpOfs, &inpState);              /* next MET key */
      free( mK2);
    } /* while mK */

    CLOSEINPOFS

  } /* while iArgc < argc || fInpList */

  CLOSEOUTOFS

  return 0;

} /* main */
