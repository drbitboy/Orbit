#include <stdio.h>
#include <string.h>
#include <math.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/ToggleB.h>

#define deg *3.14159265358979323846/180.
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

char **orbit_argv;
int orbit_argc;

enum { XSETSS=0, XFORCESS, XACTIVATESS, XRESETSS, XGETSS, XEXITSS };

/* timeout,interval,prefer_blanking,allow_exposures,mode */
/* MODE must be last */
enum { TIMOUT=0, INTERV, PRFBLK, ALLWXP, MODE
     , LASTONE };
static int tipam[LASTONE];                          /* integer value for each */
static Widget Ws[LASTONE];          /* widget for each value (text or rowcol) */

static char *wLbls[LASTONE] = {
  "Timeout:  "
, "Interval:  "
, "Prefer Blanking:  "
, "Allow Exposures:  "
, "Screen Saver Mode (for FORCE below):  "
};

enum {                          /* indices for radio button widgets & values */
  SUBSSDONTPREFERBLK=0, SUBSSPREFERBLK, SUBSSDEFAULTBLK
, SUBSSDONTALLOWEXP, SUBSSALLOWEXP, SUBSSDEFAULTEXP
, SUBSSACTIVEMODE, SUBSSRESETMODE
, SUBLASTONE
};

static radioVals[SUBLASTONE] = {                              /* valid values */
  DontPreferBlanking, PreferBlanking, DefaultBlanking
, DontAllowExposures, AllowExposures, DefaultExposures
, ScreenSaverActive, ScreenSaverReset
};

static int tipamSubIdx[LASTONE+1] = {     /* starting index of range of valid */
  SUBSSDONTPREFERBLK                      /* values, next index is 1 past end */
, SUBSSDONTPREFERBLK
, SUBSSDONTPREFERBLK
, SUBSSDONTALLOWEXP
, SUBSSACTIVEMODE
, SUBLASTONE
};

static Widget subWs[SUBLASTONE];                     /* radio button widgets  */

static char *radioLbls[SUBLASTONE] = {                 /* radio button labels */
  "Don't Prefer", "Prefer", "Default"
, "Don't Allow", "Allow", "Default"
, "Active", "Reset"
};

/**********************************************************/
void
exit_CB( Widget w, XtPointer client_data, XtPointer call_data) {
  XtCloseDisplay(XtDisplay(w));
  exit(0);
}

/**********************************************************/
void
ss_CB( Widget w, XtPointer client_data, XtPointer call_data) {
Display *display = XtDisplay(w);
int action = (int) client_data;
int lcltimeout;
int lclinterval;
int lclprefer_blanking;
int lclallow_exposures;
int iW, iW0, iWLast;

  /*
  XGetScreenSaver( display, &lcltimeout, &lclinterval
                 , &lclprefer_blanking, &lclallow_exposures);
  fprintf( stdout
    , "%5d=timeout %5d=interval %5d=prefer_blanking %5d=allow_exposures\n"
    , lcltimeout, lclinterval, lclprefer_blanking, lclallow_exposures);
  fflush( stdout);
  /**/

  /* get values from widgets - first determine which values to get */

  switch (action) {
  case XEXITSS:
    exit_CB( w, client_data, call_data);
    break;

  case XGETSS:               /* only get mode from widget if action is XGETSS */
    iW0 = MODE;
    iWLast = iW0 + 1;
    break;

  default:                                        /* get all values otherwise */
    iW0 = 0;
    iWLast = LASTONE;
    break;
  }

  for ( iW=0; iW<LASTONE; ++iW) {
    if ( tipamSubIdx[iW] >= tipamSubIdx[iW+1]) {
    char *c;
      c = XmTextFieldGetString( Ws[iW]);
      if ( c) {
        sscanf( c, "%d", tipam+iW);
        XtFree( c);
      }
    } else {
    int iSubW;
      for ( iSubW=tipamSubIdx[iW]; iSubW<tipamSubIdx[iW+1]; ++iSubW) {
        if ( XmToggleButtonGetState( subWs[iSubW])) {
          tipam[iW] = radioVals[iSubW];
          break;
        }
      }
    }
  }

  /* set screen saver parameters if this is not XGETSS */

  if ( action != XGETSS) {
    XSetScreenSaver( display
                   , tipam[TIMOUT]
                   , tipam[INTERV]
                   , tipam[PRFBLK]
                   , tipam[ALLWXP]);
  }

  switch (action) {

  case XFORCESS:
    XForceScreenSaver( display, tipam[MODE]);
    break;

  case XACTIVATESS:
    XActivateScreenSaver( display);
    break;

  case XRESETSS:
    XResetScreenSaver( display);
    break;

  case XGETSS:
  case XSETSS:
  default:
    break;
  }

  /* update local values from X server */

  XGetScreenSaver( display
                 , tipam+TIMOUT
                 , tipam+INTERV
                 , tipam+PRFBLK
                 , tipam+ALLWXP);

  /* update widgets from local values */

  for ( iW=0; iW<LASTONE; ++iW) {
    if ( tipamSubIdx[iW] >= tipamSubIdx[iW+1]) {
    char c[255];
      sprintf( c, "%d", tipam[iW]);
      XmTextFieldSetString( Ws[iW], c);
    } else {
    int iSubW;
      for ( iSubW=tipamSubIdx[iW]; iSubW<tipamSubIdx[iW+1]; ++iSubW) {
        if ( tipam[iW] == radioVals[iSubW]) {
          XmToggleButtonSetState( subWs[iSubW], True, False);
        } else {
          XmToggleButtonSetState( subWs[iSubW], False, False);
        }
      }
    }
  }
  return;
}

/*************************************************************/
int
main( int argc, char **argv) {
Display *dpy;
XtAppContext app_context;
Widget app_shell, topLevel, mainw, form, rowcol, lbl, rowcol2, sep, button;
XmString xmstr;
char tmpStr[255];
int iW, iSubW;
Arg args[10];
int n;

  orbit_argv = argv;
  orbit_argc = argc;

  XtSetLanguageProc( NULL, NULL, NULL);

  topLevel = XtVaAppInitialize( &app_context, "SSControl", NULL, 0
                              , &argc, argv, NULL, NULL);
  dpy = XtDisplay(topLevel);

/**/
  app_shell= XtVaAppCreateShell( "ScreenSaver Control", "Display3d"
                               , topLevelShellWidgetClass, dpy
                               , XmNdeleteResponse, XmDESTROY
                               , NULL);
  XtAddCallback( app_shell, XmNdestroyCallback, exit_CB, NULL);
/**/

# define XtVCMW XtVaCreateManagedWidget
# define XtVCW XtVaCreateWidget

  rowcol = XtVCW( "rowcol", xmRowColumnWidgetClass, topLevel /* form */
                 , XmNpacking, XmPACK_COLUMN
                 , XmNnumColumns, (short) (LASTONE+3)
                 , XmNorientation, XmHORIZONTAL
                 , NULL);

  /* settings; one of
   * [Label]  [TextField], or
   * [Label]  [RadioButton] [RadioButton] ...
   */

  for ( iW=0; iW<LASTONE; ++iW) {
    lbl = XtVCMW( wLbls[iW], xmLabelGadgetClass, rowcol
                , NULL);
    if ( tipamSubIdx[iW] >= tipamSubIdx[iW+1]) {
      Ws[iW] = XtVCMW( "text"
                     , xmTextFieldWidgetClass, rowcol
                     , XmNtraversalOn, True
                     , NULL);
    } else {
      Ws[iW] = XtVCMW( "rowcol2"
                     , xmRowColumnWidgetClass, rowcol
                     , XmNpacking, XmPACK_COLUMN
                     , XmNnumColumns, (short) 1
                     , XmNorientation, XmHORIZONTAL
                     , XmNradioBehavior, True
                     , XmNradioAlwaysOne, True
                     , NULL);
      for ( iSubW=tipamSubIdx[iW]; iSubW<tipamSubIdx[iW+1]; ++iSubW) {
         subWs[iSubW] = XtVCMW( radioLbls[iSubW]
                              , xmToggleButtonWidgetClass, Ws[iW]
                              , NULL);
      }
    }
  }

  /* Separators, then Control Buttons */

  sep = XtVCMW( "sep1", xmSeparatorWidgetClass, rowcol
              , NULL);
  sep = XtVCMW( "sep2", xmSeparatorWidgetClass, rowcol
               , NULL);

  lbl = XtVCMW( "Screen Saver Controls", xmLabelGadgetClass, rowcol
              , NULL);

  lbl = XtVCMW( "Parameter Controls", xmLabelGadgetClass, rowcol
              , NULL);

  rowcol2 = XtVCMW( "rowcol2"
                   , xmRowColumnWidgetClass, rowcol
                   , XmNpacking, XmPACK_COLUMN
                   , XmNnumColumns, (short) 1
                   , XmNorientation, XmHORIZONTAL
                   , NULL);

  button = XtVCMW( "ACTIVATE", xmPushButtonWidgetClass, rowcol2
                 , XmNtraversalOn, False
                 , NULL);
  XtAddCallback (button, XmNactivateCallback, ss_CB, (XtPointer) XACTIVATESS);

  button = XtVCMW( "FORCE [Mode]", xmPushButtonWidgetClass, rowcol2
                   , XmNtraversalOn, False
                   , NULL);
  XtAddCallback (button, XmNactivateCallback, ss_CB, (XtPointer) XFORCESS);

  button = XtVCMW( "RESET", xmPushButtonWidgetClass, rowcol2
                 , XmNtraversalOn, False
                 , NULL);
  XtAddCallback (button, XmNactivateCallback, ss_CB, (XtPointer) XRESETSS);

  rowcol2 = XtVCMW( "rowcol2"
                   , xmRowColumnWidgetClass, rowcol
                   , XmNpacking, XmPACK_COLUMN
                   , XmNnumColumns, (short) 1
                   , XmNorientation, XmHORIZONTAL
                   , NULL);

  button = XtVCMW( "GET", xmPushButtonWidgetClass, rowcol2
                 , XmNtraversalOn, False
                 , NULL);
  XtAddCallback (button, XmNactivateCallback, ss_CB, (XtPointer) XGETSS);

  button = XtVCMW( "SET", xmPushButtonWidgetClass, rowcol2
                 , XmNtraversalOn, False
                 , NULL);
  XtAddCallback (button, XmNactivateCallback, ss_CB, (XtPointer) XSETSS);

  button = XtVCMW( "EXIT", xmPushButtonWidgetClass, rowcol2
                 , XmNtraversalOn, False
                 , NULL);
  XtAddCallback (button, XmNactivateCallback, ss_CB, (XtPointer) XEXITSS);

  tipam[MODE] = ScreenSaverActive;

  /* initial setup of MODE buttons so mode will be set properly */

  XmToggleButtonSetState( subWs[SUBSSRESETMODE], False, False);
  XmToggleButtonSetState( subWs[SUBSSACTIVEMODE], True, False);

  ss_CB( rowcol, (XtPointer) XGETSS, (XtPointer) 0); /* set widget values */

  XtManageChild( rowcol);
  XtRealizeWidget( topLevel);
  XtAppMainLoop( app_context);
  return 0;
}
