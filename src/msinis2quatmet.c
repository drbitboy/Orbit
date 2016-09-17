/* Read lines from stdin, write files
                                                                           Files
Line (typical, # = numerics)                                   Description Types
============================================================   ==============  =
#,#:#:#,#,#,MSI ,Automatic,exposure,image,taken,with,filter,#  MSI Exposure    1
#,#:#:#,#,#,MSI ,Manual,exposure,image,taken,with,filter,#     MSI Exposure    1
#,#:#:#,#,#,MSI ,Quaterion:,#,#,#,#                            Quaternion      2
#,#:#:#,#,#,NIS ,RT,f#,NI_SHUTR_IN_OUT,BOTH_OUT                Set NIS Shutter -
#,#:#:#,#,#,NIS ,RT,f#,NI_SHUTR_IN_OUT,SLIT                    Set NIS Shutter -
#,#:#:#,#,-,Seq:,#,Pos:,#,Quat:,#,#,#,#                        NIS Obs + Quat  3
0 11111 2 3 4444 5

Fields:
 0  Day of year
 1  HH:MM:SS
 2  kMET
 3  Number or dash

 4  MSI - exposure - 12 fields total
          - Field 6 = "exposure"
          - Field 7 = "image"
          - Field 8 = "taken"

    MSI - quaternion - 10 fields total
          - Field 5 = "Quaterion:"
          - Fields 6-9 = q0-q3

    NIS - Set shutter - 9 fields total
          - Field 7 = "NI_SHUTR_IN_OUT"
          - Field 8 = "SLIT" or "BOTH_OUT" - assume narrow if not BOTH_OUT

    Seq: - NIS observation + Quaternion - 13 fields total
           - Field 6 = "Pos:"
           - Field 7 = mirror position
           - Field 8 = "Quat:";
           - Fields 9-12 = q0-q3

File types:

  1       YYDDD.msi.met
  2       YYDDD.bc
  3       YYDDD.nis.met & YYDDD.bc

 */

typedef struct {
  int _type;
  int _nTok;
  char **_testChar;
  int *_testFld;
} LINEMATCH;

#define NTOK_MSIEXP 12
#define NTOK_MSIQUAT 10
#define NTOK_SETNISSHUT 9
#define NTOK_NISOBSQUAT 13

enum { LINE_MSIEXP=0, LINE_MSIQUAT, LINE_SETNISSHUT, LINE_NISOBSQUAT };

static char *testMSIExpChar[] = { "MSI", "exposure", "image", "taken" };
static int testMSIExpFld[]    = { 4,     6,          7,       8,   -1 };
static char *testMSIQuatChar[] = { "MSI", "Quater\n" };
static int testMSIQuatFld[]    = { 4,     5,      -1 };
static char *testNISShutChar[] = { "NIS", "NI_SHUTR_IN_OUT" };
static int testNISShutFld[]   =  { 4,     7,             -1 };
static char *testNISObsQuatChar[] = { "Seq:", "Pos:", "Quat:" };
static int testNISObsQuatFld[]   =  { 4,      6,      8,   -1 };

static LINEMATCH lineMatch[] = {
    { LINE_MSIEXP, NTOK_MSIEXP, testMSIExpChar, testMSIExpFld }
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
}

#define IFOPENFILE( F, PFX, SFX, FN) \
  if ( !F) { \
    strcpy( FN, PFX); \
    strcat( FN, SFX); \
    F = fopen( FN, "w"); \
    if ( !F) { \
      fprintf( stderr, "Problem opening file %s\n", FN); \
    } \
  }

static char *nisNarrowPtr = { "nis" };
static char *nisWidePtr = { "niswide" };

#include <stdio.h>
#include <string.h>

int
main( int argc, char **argv) {
char *tokPtr[100];
char rawLine[1024];
char tokenLine[1024];
char *cPtr, *lPtr;
int nTok;
long scInstr;
char *sclkch;
double quatarr[4];
LINEMATCH *lm;
char *nisInstr = nisNarrowPtr;
char fn[1024];

FILE *msiMetFile=(FILE *) 0;
FILE *nisMetFile=(FILE *) 0;
FILE *kmetQuatFile=(FILE *) 0;

  /* for each line */
  while ( fgets( rawLine, 1024, stdin)) {
    strcpy( tokenLine, rawLine);
    lm = matchLine( tokenLine, tokPtr, lineMatch);
    if ( !lm) {
      fprintf( stderr, "Could not parse this line:\n%s", rawLine);
    } else {
      switch ( lm->_type) {

      case LINE_MSIEXP:                                       /* MSI exposure */
        IFOPENFILE( msiMetFile, argv[1], ".msi.met", fn)
        fprintf( msiMetFile, "%s %s\n", tokPtr[2], "msi");
        break;

      case LINE_MSIQUAT:                                    /* MSI quaternion */
        IFOPENFILE( kmetQuatFile, argv[1], ".kmetquat", fn)
        fprintf( kmetQuatFile, "%s %s %s %s %s\n", tokPtr[2]
              , tokPtr[6], tokPtr[7], tokPtr[8], tokPtr[9]);
        break;

      case LINE_SETNISSHUT:                                /* Set NIS Shutter */
        if ( strcmp( tokPtr[8], "BOTH_OUT")) nisInstr = nisNarrowPtr;
        else nisInstr = nisWidePtr;
        break;

      case LINE_NISOBSQUAT:                   /* NIS observation + quaternion */
        IFOPENFILE( nisMetFile, argv[1], ".nis.met", fn)
        fprintf( nisMetFile, "%s %s %s\n", tokPtr[2], nisInstr, tokPtr[7]);
        IFOPENFILE( kmetQuatFile, argv[1], ".kmetquat", fn)
        fprintf( kmetQuatFile, "%s %s %s %s %s\n", tokPtr[2]
              , tokPtr[9], tokPtr[10], tokPtr[11], tokPtr[12]);
        break;
      default:
        fprintf(stderr, "Program error, contact programmer, code WSNBATGH-0\n");
        fprintf( stderr, "Problem cause:\n%s\n", rawLine);
        break;
      }
    } /* if !lm else ... */
  } /* while fgets */
  if ( nisMetFile) fclose( nisMetFile);
  if ( msiMetFile) fclose( msiMetFile);
  if ( kmetQuatFile) fclose( kmetQuatFile);
  return 0;
}
