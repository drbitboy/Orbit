#include <string.h>
#include <stdio.h>

#define _ORBITFORT_H_TYPESONLY_
#include "orbitfort.h"
#include "orbit_spice_names.h"
#include "debug.h"

#define NUMSEQDEF 30
enum {
   SEQID=0
 , SCANPERSEQ
 , SECBTWSCAN
 , OBSPERSCAN
 , CALINTERVAL
 , SPECTRAPEROBS
 , NUMRESTS
 , NUMDARKS
 , STEPPEROBS
 , SECBTWOBS
 , COUNTSEQ
 };

#define scanPerSeq seqArr[SCANPERSEQ]
#define secBtwScan seqArr[SECBTWSCAN]
#define obsPerScan seqArr[OBSPERSCAN]
#define calInterval seqArr[CALINTERVAL]
#define spectraPerObs seqArr[SPECTRAPEROBS]
#define numRests seqArr[NUMRESTS]
#define numDarks seqArr[NUMDARKS]
#define stepPerObs seqArr[STEPPEROBS]
#define secBtwObs seqArr[SECBTWOBS]

#define USAGE \
  fprintf( stderr, "Usage:\n  superscan %s %s %s %s %s %s %s %s %s %s\n" \
  , "'<starting time>'" \
  , "<seq_num>" \
  , "<init_mir_pos>" \
  , "<nis | niswide>" \
  , "<positions_per_super_scan>" \
  , "<mirror steps betw SS positions>" \
  , "<# of SS>" \
  , "<LEAPSECOND filename>" \
  , "<SCLK filename>" \
  , "<.REQ file containing NISSEQDEF(s)>" \
  ); \
  fprintf( stderr, "  Starting time = YYYY-DOY:HH:MM:SS or kMET\n"); \
  fprintf( stderr, "    e.g.  '1998-320 // 12:34:56' or 86857008\n"); \
  fflush( stderr)

#define IGETINT( I, IARG, CARG) \
  if ( 1 != sscanf(CARG[IARG], "%ld", &I)) { \
    fprintf( stderr, "Error parsing argument %d '%s'; exiting ...\n" \
                   , IARG, CARG[IARG]); \
    USAGE; return -1; \
  } \
  IARG++

#define IARG( I) IGETINT( I, iarg, argv)
#define ITOK( I) IGETINT( I, iTok, tokPtr)

int
main( int argc, char **argv) {
double startingkMet;
long initPosnPerSS;
long numPosnPerSS;
long mirrorStepSS;
long seqNumSS;
long numSS;
char *aperSS;
long delayPerSS;
long seqArr[COUNTSEQ];
long seqNum;
#define UTCLEN 51
char utc[UTCLEN];
fortint utcLen, utcLenM1 = UTCLEN-1, fnLen, strLen1, strLen2;
double etMet, etMet0, delEtMet;
int isUtc;
int iarg;
char rawLine[1024];
char cpyLine[1024];
char *tokPtr[100];
int nTok, iTok;
int orbit_commaParse( char *, char **);
FILE *fReq;
long i, iNumSeq, iNumSS, iMP, iScan, iObs;

  switch (argc) {
  case 11:
     break;
  default:
    USAGE;
    return -1;
    break;
  }

  iarg=1;
  if ( strchr( argv[iarg], ':')                         /* find : in HH:MM:SS */
    || strchr( argv[iarg], 'T')                      /* or T in YYYY-DOYT... */
    || strchr( argv[iarg], '/')                      /* or / in YYYY-DOY//... */
    || (strchr( argv[iarg], '-') && argv[iarg][0] != '-') /* or non-leading - */
     ) {
    /* must wait for leapsecond kernel
     * utcLen = strlen( argv[iarg]);
     * ospice_utc2et( &etMet0, &utcLen, argv[iarg]);
     */
    isUtc = 1;
  } else {
    sscanf( argv[iarg], "%lf", &etMet0);
    isUtc = 0;
  }

  iarg++;
  IARG( seqNumSS);
  seqArr[SEQID] = seqNumSS - 1; /* so no match if sequence seqNumSS not found */
  IARG( initPosnPerSS);
  aperSS = argv[iarg++]; /* aperture */
  IARG( numPosnPerSS);
  IARG( mirrorStepSS);
  IARG( numSS);

  /* LEAPSECOND & SCLK */
  ospice_clpool();
  fnLen = strlen( argv[iarg]);
  ospice_ldpool( &fnLen, argv[iarg]);
  iarg++;

  fnLen = strlen( argv[iarg]);
  ospice_ldpool( &fnLen, argv[iarg]);
  iarg++;

  /* leapsecond kernel should be available now */

  if ( isUtc) {
    utcLen = strlen( argv[1]);
    ospice_utc2et( &etMet0, &utcLen, argv[1]);
  }

  /* look for NISSEQDEF lines in REQ file */

  fReq = fopen( argv[iarg], "r");
  if ( !fReq) {
    fprintf( stderr, "Error opening REQ file 'argv'; exiting ...\n"
                   , argv[iarg]);
    USAGE;
  }
  while ( fgets( rawLine, 1024, fReq)) {
    strcpy( cpyLine, rawLine);
    nTok = orbit_commaParse( cpyLine, tokPtr);

    /* is token 0 == "NISSEQDEF" */

    if ( nTok == (COUNTSEQ+2)) if ( !strcmp( *tokPtr, "NISSEQDEF") ) {

      iTok=2;                 /* third token is sequence number (after delay) */
      ITOK( seqNum);

      if ( seqNum == seqNumSS) {                         /* if seq # matches, */
        seqArr[SEQID] = seqNum;
        for ( i=SCANPERSEQ; i<COUNTSEQ; ++i) {      /* read in rest of tokens */
          ITOK( seqArr[i]);
  } } } }
  fclose( fReq);

  if ( seqArr[SEQID] != seqNumSS) {
    fprintf( stderr, "Couldn't find NISSEQDEF ID # %ld in file %s\n"
                   , seqNumSS, argv[iarg]);
    USAGE;
    return -1;
  }

  etMet = etMet0;

  for ( iNumSS=0; iNumSS<numSS; ++iNumSS) {            /* for each super scan */

    for ( iNumSeq=0; iNumSeq<numPosnPerSS; ++iNumSeq) {  /* for each start MP */

      for ( iScan=0; iScan<scanPerSeq; ++iScan) {     /* for each scan in seq */

        iMP = initPosnPerSS + (iNumSeq * mirrorStepSS);

        for ( iObs=0; iObs<obsPerScan; ++iObs) {      /* for each Obs in scan */

          if ( !isUtc) fprintf( stdout, "%lf", etMet);      /* print out time */
          else {
            ospice_et2doy( &etMet, &utcLenM1, &utcLen, utc);
            utc[utcLen] = '\0';
            fprintf( stdout, "%s", utc);
          }

          fprintf( stdout, " %s %ld\n", aperSS, iMP);      /* & aperture & MP */

          etMet += (spectraPerObs+secBtwObs);
          iMP += stepPerObs;

        } /* for iObs */

        etMet += (secBtwScan - secBtwObs);

      } /* for iScan */

      etMet += ( (((obsPerScan*stepPerObs)/4) + 1) - secBtwScan );

    } /* for iNumSeq */

    etMet += ( ((numPosnPerSS*abs(mirrorStepSS)))/4 + 1 );

  } /* for iNumSS */

  return 0;
}
