/*
   -outiep <fn>          Output one line for each plate in a FOV to flat file
   -outiep -             named <fn>; <fn> = '-' => standard output (default).
   -nooutiep             - if -nooutiep is specified, do not write flat file
   -spud <viewfn>        Use spud model from file <viewfn>
   -plates <platefn>     Use plate model from file <platefn>
   -instats <instatfn>   input plate model statistics file
   -outstats <outstatfn> output updated plate model statistics
   -iep -vine -xgrs      Output geometry format (default=iep).
   -vmag -lcrv           -iep  => cpLLR i e alpha vertLL time
   -fov <fovType>        How to interpret whether a given plate is in the
   -stat <statistic>     Summarize <statistic> for each plate,
   -max -sum -min  Selection criterion for per-plate statistics.  Has no
   -platesoutstat <statistic>   output this statistic to plate model instead of
   -init           initialize plate model per-plate statistics to -1.0 before
   -rigor          perform more rigorous visibility checks
 */

#include <Xm/MainW.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>

#ifdef vms
char *malloc();
#else
#include <malloc.h>
#endif
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _ORBITFORT_H_TYPESONLY_
#include "orbitgui_fieldmenu.h"
#include "orbit_util.h"
#include "local.h"
#include "debug.h"

static char *fmLbl[] = {
  "Input frames' filename"
, "Output IEP file geometry format\0IEP\0VINE\0XGRS\0VMAG\0LCRV\0NONE\0"
, "Output IEP filename (if not NONE above)"
, "In-FOV algorithm\0Winding\0Cart4\0XGRS\0W2\0"
, "Input Spud Model filename"
, "Input Plate Model filename"
, "Output Plate Model filename"
, "Input statistics filename"
, "Output statistics filename"
, "Orbit database prefix|filename"
, "Statistic to summarize\0Inc\0Mu0\0Emi\0Mu\0Mu0Mu\0Pha\0"
," \" (cont'd; check 1 item only)\0Cos(Pha)\0Res\0Cov\0MRMorph\0MRAlbedo\0obs\0"
, "Stat summarization criterion\0Max\0Min\0Sum\0"
, "Stat to output to Plate\0Inc\0Mu0\0Emi\0Mu\0Mu0Mu\0Pha\0"
," \" (cont'd; check 1 item only)\0Cos(Pha)\0Res\0Cov\0MRMorph\0MRAlbedo\0Obs\0"
, "InitModel'sStats/RigorousVisibilityTests/Verbose\0-init\0-rigor\0-verbose\0"
/* OUT 15.may'2000 , "Initialize plate model's statistics first\0Yes\0No\0" */
, "Observational Parameter Low Limit"
/* OUT 15.may'2000 , "Perform more rigorous visibility tests\0Yes\0No\0" */
, (char *) NULL };

enum
{ F2IEP_IEPFMT=0L, F2IEP_VINEFMT, F2IEP_XGRSFMT, F2IEP_VMAGFMT, F2IEP_LCRVFMT
, F2IEP_NONEFMT
, F2IEP_WINDINGFOV, F2IEP_CART4FOV, F2IEP_XGRSFOV, F2IEP_W2FOV
, F2IEP_INCSTAT, F2IEP_MU0STAT, F2IEP_EMISTAT, F2IEP_MUSTAT, F2IEP_MU0MUSTAT
  , F2IEP_PHASTAT
  , F2IEP_COSPHASTAT, F2IEP_RESSTAT, F2IEP_COVSTAT
  , F2IEP_MRMORPHSTAT, F2IEP_MRALBEDOSTAT
  , F2IEP_OBSPARMSTAT
  , F2IEP_MAXCRIT, F2IEP_MINCRIT, F2IEP_SUMCRIT
, F2IEP_YESINIT /* , F2IEP_NOINIT */
, F2IEP_YESRIGOR /* , F2IEP_NORIGOR */
, F2IEP_YESVERBOSE

# define F2IEP_0FN F2IEP_FRAMESINFN                         /* first filename */

, F2IEP_FRAMESINFN
, F2IEP_IEPOUTFN
, F2IEP_SPUDINFN
, F2IEP_PLATESINFN
, F2IEP_PLATESOUTFN
, F2IEP_STATSINFN
, F2IEP_STATSOUTFN
, F2IEP_ORBDBFN
, F2IEP_OBSLOWLIM
, F2IEP_COUNT };

typedef struct { long _bitpos; int _index; char *_str[2]; } CHARLIST;

static CHARLIST options[] = {
  F2IEP_IEPFMT, 0,  "-iep", (char *) NULL
, F2IEP_VINEFMT, 0, "-vine", (char *) NULL
, F2IEP_XGRSFMT, 0, "-xgrs", (char *) NULL
, F2IEP_VMAGFMT, 0, "-vmag", (char *) NULL
, F2IEP_LCRVFMT, 0, "-lcrv", (char *) NULL
, F2IEP_NONEFMT, 0, "-nooutiep", (char *) NULL
, F2IEP_WINDINGFOV, 0, "-fov", "winding"
, F2IEP_CART4FOV, 0, "-fov", "cart4"
, F2IEP_XGRSFOV, 0, "-fov", "xgrs"
, F2IEP_W2FOV, 0, "-fov", "w2"
, F2IEP_INCSTAT, 0, "-stat", "incid"
, F2IEP_MU0STAT, 0, "-stat", "mu0"
, F2IEP_EMISTAT, 0, "-stat", "emiss"
, F2IEP_MUSTAT, 0, "-stat", "mu"
, F2IEP_MU0MUSTAT, 0, "-stat", "mu0mu"
, F2IEP_PHASTAT, 0, "-stat", "phase"
, F2IEP_COSPHASTAT, 0, "-stat", "cosphase"
, F2IEP_RESSTAT, 0, "-stat", "resolution"
, F2IEP_COVSTAT, 0, "-stat", "coverage"
, F2IEP_MRMORPHSTAT, 0, "-stat", "mrmorph"
, F2IEP_MRALBEDOSTAT, 0, "-stat", "mralbedo"
, F2IEP_OBSPARMSTAT, 0, "-stat", "obs"
, F2IEP_MAXCRIT, 0, "-max", (char *) NULL
, F2IEP_MINCRIT, 0, "-min", (char *) NULL
, F2IEP_SUMCRIT, 0, "-sum", (char *) NULL
, F2IEP_YESINIT, 0, "-init", (char *) NULL
, F2IEP_YESRIGOR, 0, "-rigor", (char *) NULL
, F2IEP_YESVERBOSE, 0, "-verbose", (char *) NULL

  /* output plate model statistic - uses second long bit field */

, F2IEP_INCSTAT, 1, "-platesoutstat", "incid"
, F2IEP_MU0STAT, 1, "-platesoutstat", "mu0"
, F2IEP_EMISTAT, 1, "-platesoutstat", "emiss"
, F2IEP_MUSTAT, 1, "-platesoutstat", "mu"
, F2IEP_MU0MUSTAT, 1, "-platesoutstat", "mu0mu"
, F2IEP_PHASTAT, 1, "-platesoutstat", "phase"
, F2IEP_COSPHASTAT, 1, "-platesoutstat", "cosphase"
, F2IEP_RESSTAT, 1, "-platesoutstat", "resolution"
, F2IEP_COVSTAT, 1, "-platesoutstat", "coverage"
, F2IEP_MRMORPHSTAT, 1, "-platesoutstat", "mrmorph"
, F2IEP_MRALBEDOSTAT, 1, "-platesoutstat", "mralbedo"
, F2IEP_OBSPARMSTAT, 1, "-platesoutstat", "obs"

  /* output file names
   * - Fn _index=-1 => use Fn[] relative to F2IEP_0FN */

, F2IEP_FRAMESINFN, -1, "-framesin", (char *) NULL
, F2IEP_IEPOUTFN, -1, "-outiep", (char *) NULL
, F2IEP_SPUDINFN, -1, "-spud", (char *) NULL
, F2IEP_PLATESINFN, -1, "-plates", (char *) NULL
, F2IEP_PLATESOUTFN, -1, "-platesout", (char *) NULL
, F2IEP_STATSINFN, -1, "-instats", (char *) NULL
, F2IEP_STATSOUTFN, -1, "-outstats", (char *) NULL
, F2IEP_ORBDBFN, -1, "-orbdb", (char *) NULL

  /* observational parameter low limit - text string */

, F2IEP_OBSLOWLIM, -2, "-obsLowLim", (char *) NULL
# define F2IEP_0TXT F2IEP_OBSLOWLIM

,  -1, 0, (char *) NULL, (char *) NULL                         /* end of list */
};

static char *myArgv[80];                     /* 80 arguments should be enough */
static int myArgc;

static long f2iepBitsArr[2];
#define f2iepBits f2iepBitsArr[0]
#define f2iepBits2 f2iepBitsArr[1]

static FLDMENU fldMenu[40];   /* should be plenty */

#define MAXFNLEN 1023
typedef char FN[MAXFNLEN+1];
typedef char TXT[FLD_MAXTXTLENPERDBL+1];

static char frm2iepCmd[] = { "frm2iep" };

static FN Fn[F2IEP_0TXT-F2IEP_0FN], Fn0[F2IEP_0TXT-F2IEP_0FN];
static FN fldMenuWildcard;

static TXT Txt[F2IEP_COUNT-F2IEP_0TXT],  Txt0[F2IEP_COUNT-F2IEP_0TXT];

/**************************/
void frm2iepgui_setWatchInsens( Widget w, Boolean set) {

  if ( XmIsPushButton(w)) XtSetSensitive( w, set ? False : True);

  while ( !XtIsShell( w)) w = XtParent( w);
  orbitgui_watchCursor( w, set);

  while ( !XtIsTopLevelShell( w)) w = XtParent( w);
  orbitgui_watchCursor( w, set);
  return;
}

#define ADDARG( CPTR) if ( CPTR) { *(lclArgv++) = CPTR; myArgc++; }
/**************************/
int
frm2iepgui_ok_CB( Widget mainButton, FLDMENU *fldMenu) {
CHARLIST *cl;
char *cPtr;
char **lclArgv;
int i;
int frm2iep_main( int, char **);

  myArgc = 0; lclArgv = myArgv;

  ADDARG( frm2iepCmd);

  for ( cl=options; cl->_bitpos >= 0; ++cl ) {
    switch ( cl->_index) {
    case -2:                           /* ->_index is -2; special text string */
      cPtr = Txt[cl->_bitpos - F2IEP_0TXT];

      if ( !cPtr) break;
      ADDARG( cl->_str[0])
      ADDARG( cPtr)
      break;

    case -1:                /* ->_index is -1; special text string - filename */
      cPtr = Fn[cl->_bitpos - F2IEP_0FN];

      if ( !cPtr) break;
      if ( !*cPtr) break;
      if ( !strcmp( cPtr, "[ .. ]")) break;
      if ( strchr( cPtr, '*')) break;
      if ( strchr( cPtr, '?')) break;
      ADDARG( cl->_str[0])
      ADDARG( cPtr)
      break;

    default:                      /* ->_index >= 0; is index into bits array; */
      if ( 0L !=                              /*   - use arg(s) if bit is set */
                 (f2iepBitsArr[cl->_index] & (1L<<cl->_bitpos)) ) {
        ADDARG( cl->_str[0])
        ADDARG( cl->_str[1])
      }
      break;
    }
  }
  *lclArgv = (char *) 0;

  for ( lclArgv=myArgv; *lclArgv; ++lclArgv, ++i) {
    fprintf( stderr, "%s%s", i ? " " : "", *lclArgv);
  }
  fprintf( stderr, "\n"); fflush( stderr);

  i = frm2iep_main( myArgc, myArgv);

  frm2iepgui_setWatchInsens( mainButton, False);

  /* return i ? 0 : 1; */
  return 0;
}

/**************************/
int
frm2iepgui_cancel_CB( Widget mainButton, FLDMENU *fldMenu) {

  frm2iepgui_setWatchInsens( mainButton, False);
  return 1;
}

/**************************/
void
frm2iepgui_createFldMenu_CB( Widget w, XtPointer client_data
                                     , XtPointer call_data) {
FLDMENU *fm = fldMenu;
char **lclFmLbl = fmLbl;
int bitDflt;            /* to avoid "left shift is negative" compiler warning */

Widget parentW = (Widget) client_data;

  if ( !parentW) exit(0);

  frm2iepgui_setWatchInsens( w, True);

#define NEXTFM (fm++)->lbl_txt = *(lclFmLbl++)

#define MKFMBIT( BITS, BIT0, BITDFLT) \
  fm->type = FLD_BIT; \
  fm->fld_lng = &BITS; \
  if ( (BITDFLT) >= 0) BITS |= (1L<<(bitDflt=BITDFLT));/* see bitDflt above */ \
  fm->fld_lowbit = BIT0; \
  NEXTFM

#define MKFMBITS( BITS, BIT0, BITDFLT) \
  fm->type = FLD_BITS; \
  fm->fld_lng = &BITS; \
  if ( (BITDFLT) >= 0) BITS |= (1L<<(bitDflt=BITDFLT)); \
  fm->fld_lowbit = BIT0; \
  NEXTFM

#define MKFMFIL( I, WILDCARD, ISWRITE) \
  fm->type = (ISWRITE) ? FLD_TXTFILEWRITE : FLD_TXTFILEREAD; \
  fm->fld_txt = Fn[I-F2IEP_0FN]; \
  fm->fld_txt0 = Fn0[I-F2IEP_0FN]; \
  fm->fld_txt_maxlen = MAXFNLEN; \
  strcpy( fm->fld_txt0, WILDCARD); \
  NEXTFM

  f2iepBits = f2iepBits2 = 0;

  /* "Input frames' filename" */
  MKFMFIL( F2IEP_FRAMESINFN, "*.f", 0);

  /* "Output IEP file geometry format\0IEP\0VINE\0XGRS\0VMAG\0LCRV\0NONE\0" */
  MKFMBIT( f2iepBits, F2IEP_IEPFMT, F2IEP_IEPFMT);

  /* "Output IEP filename (if not NONE above)" */
  MKFMFIL( F2IEP_IEPOUTFN, "*.iep", 1);

  /* "In-FOV algorithm\0Winding\0Cart4\0XGRS\0W2\0" */
  MKFMBIT( f2iepBits, F2IEP_WINDINGFOV, F2IEP_WINDINGFOV);

  /* "Input Spud Model filename" */
  MKFMFIL( F2IEP_SPUDINFN, "*.spud", 0);

  /* "Input Plate Model filename" */
  MKFMFIL( F2IEP_PLATESINFN, "*.pl*", 0);

  /* "Output Plate Model filename" */
  MKFMFIL( F2IEP_PLATESOUTFN, "*.pl*", 1);

  /* "Input statistics filename" */
  MKFMFIL( F2IEP_STATSINFN, "*.iepstat", 0);

  /* "Output statistics filename" */
  MKFMFIL( F2IEP_STATSOUTFN, "*.iepstat", 1);

  /* "Orbit database prefix|filename" */
  MKFMFIL( F2IEP_ORBDBFN, "*t.orbdb", 1);

  /* "Statistic to summarize\0Inc\0Mu0\0Emi\0Mu\0Mu0Mu\0Pha\0" */
  MKFMBITS( f2iepBits, F2IEP_INCSTAT, -1);

  /* " \" (cont'd...)\0Cos(Pha)\0Res\0Cov\0MRMorph\0MRAlbedo\0" */
  MKFMBITS( f2iepBits, F2IEP_COSPHASTAT, -1);

  /* "Stat summarization criterion\0Max\0Min\0Sum\0" */
  MKFMBIT( f2iepBits, F2IEP_MAXCRIT, -1);

  /**** output stats go in second bit field */

  /* "Stat to output to Plate\0Inc\0Mu0\0Emi\0Mu\0Mu0Mu\0Pha\0" */
  MKFMBITS( f2iepBits2, F2IEP_INCSTAT, -1);

  /* " \" (cont'd...)\0Cos(Pha)\0Res\0Cov\0MRMorph\0MRAlbedo\0" */
  MKFMBITS( f2iepBits2, F2IEP_COSPHASTAT, -1);

  /* "Initialize plate model's statistics first\0Yes\0No\0" */
  /* OUT 15.may'2000 MKFMBIT( f2iepBits, F2IEP_YESINIT, F2IEP_NOINIT); */

  /* "Init stats/RigorousVis/Verbose\0-init\0-rigor\0-verbose\0" */
  MKFMBITS( f2iepBits, F2IEP_YESINIT, -1);

  /* "Observational Parameter Low Limit" */
  fm->type = FLD_TXT;
  fm->fld_txt = Txt[F2IEP_OBSLOWLIM-F2IEP_0TXT];
  fm->fld_txt0 = Txt0[F2IEP_OBSLOWLIM-F2IEP_0TXT];
  strcpy( fm->fld_txt0, "0.0");
  fm->fld_txt_maxlen = FLD_MAXTXTLENPERDBL;
  NEXTFM;
 
  /* "Perform more rigorous visibility tests\0Yes\0No\0" */
  /* OUT 15.may'2000 MKFMBIT( f2iepBits, F2IEP_YESRIGOR, F2IEP_NORIGOR); */

  fm->type = FLD_END;
  fm->client_call = frm2iepgui_cancel_CB;
  fm->fld_txt0 = fldMenuWildcard;
  strcpy( fm->fld_txt0, "*.frm2iepMenu");

  fldMenu->client_call = frm2iepgui_ok_CB;
  fldMenu->client_data = (void *) w;   /* save button to reset watch & insens */

  orbitgui_create_fldmenu_dialog( parentW, "frm2iep", fldMenu);

  return;
}

/*****************************/
int
frm2iepgui_main( int argc, char **argv) {
Widget toplevel, pb, rc;
XtAppContext app;
XtActionsRec actions[4];

  XtSetLanguageProc (NULL, NULL, NULL);

  toplevel = XtVaAppInitialize( &app, "Demos", NULL, 0
                              , &argc, argv, NULL, NULL);

  rc = XtVaCreateWidget( "rowcol", xmRowColumnWidgetClass, toplevel,
      NULL);

  pb = XtVaCreateManagedWidget( "FRM2IEP Setup"
                              , xmPushButtonWidgetClass, rc, NULL);
  XtAddCallback( pb, XmNactivateCallback, frm2iepgui_createFldMenu_CB, rc);

  pb = XtVaCreateManagedWidget( "Exit"
                              , xmPushButtonWidgetClass, rc, NULL);
  XtAddCallback( pb, XmNactivateCallback, frm2iepgui_createFldMenu_CB, NULL);

  XtManageChild( rc);
  XtRealizeWidget( toplevel);
  XtAppMainLoop( app);

  return 0;
}

#ifndef _NO_MAIN_

#define DUMYROUTINE( A, B) \
void *A () \
{ if ( *(B)) { fprintf(stderr,"Call to dummy routine:  %s\n",B); exit(1); } \
return 0; }                                                                    


/* DUMYROUTINE( orbitgui_return_spudv_sc, "orbitgui_return_spudv_sc") */
/* DUMYROUTINE( orbitgui_return_boreroll, "orbitgui_return_boreroll") */

DUMYROUTINE( orbitgui_update_BoreRoll, "orbitgui_update_BoreRoll")

DUMYROUTINE( orbitgui_get1stCASFromCurItem, "")
DUMYROUTINE( orbit_CAS_ds40Vec, "")
DUMYROUTINE( orbit_CAS_ds56Vec, "")
DUMYROUTINE( orbit_CAS_TypeToName, "")
DUMYROUTINE( orbitgui_add_comment_sc, "")

/*****************************/
int
main( int argc, char **argv) {
  return frm2iepgui_main( argc, argv);
}

#endif
