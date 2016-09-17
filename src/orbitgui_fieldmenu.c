/* orbitgui_fieldmenu.c - gui for menu of text fields (incl. floating pt) */

#include <Xm/FileSB.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/ToggleB.h>
#include <Xm/DialogS.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledW.h>

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <sys/stat.h>

#include "orbitgui_fieldmenu.h"
#include "debug.h"
#define CHARSET XmSTRING_DEFAULT_CHARSET
#define XmSF XmStringFree

static Arg args[20];
static Cardinal n;

void do_writeable_search();
void do_readable_search();

static void MyDW(Widget w) {
  DPR((stderr, "in (orbitgui_fieldmenu)MyDW// "));
  XtDestroyWidget(w);
  return;
}

static void MyDW_CB(Widget w, XtPointer client_data, XtPointer call_data) {
  MyDW(w);
  return;
}

void
orbitgui_fldmenu_loadsave_CB( Widget w, XtPointer client_data
                            , XtPointer call_data, int dowrt)
{
char *filnam;
char *check_able();
FILE *f;
FLDMENU *fm, *fldMenu = (FLDMENU *) client_data;
char lblline[255], dataline[255], *cptr;
int badlab, i;
int scaleint;               /* intermediate int for loading to or from Slider */
XmFileSelectionBoxCallbackStruct *cbs = 
  (XmFileSelectionBoxCallbackStruct *) call_data;

  if ( XmStringGetLtoR( cbs->dir, XmFONTLIST_DEFAULT_TAG, &cptr)) {
    /* fprintf( stderr, "cbs->dir='%s'\n", cptr); /**/
    fflush( stderr);
  }

  filnam = check_able(w, client_data, call_data, dowrt);
  if ( !filnam) return;
  if ( !dowrt || strcmp( filnam, "-")) f = fopen( filnam, dowrt?"w":"r");
  else f = stderr;
  XtFree( filnam);
  if ( !f) return;
  MyDW( w);

  for ( fm=fldMenu; fm->type != FLD_END; fm++) {
  /* get text field data if it exists & if we are writing the file */
  char *fld_data = ((fm->type == FLD_BIT)
                     ||(fm->type == FLD_BITS) 
                     ||(fm->type == FLD_SLIDER_UPLEFT)
                     ||(fm->type == FLD_SLIDER_UPRIGHT)
                     || !dowrt) 
                     ? (char *) 0
                     : XmTextFieldGetString (fm->fld_w);

    if ( dowrt) {
      fprintf( f, "%s\n", fm->lbl_txt);
    } else {
      *lblline = *dataline = '\0';
      fgets( lblline, 255, f);
      fgets( dataline, 255, f);
      if ( cptr=strchr( lblline, '\n')) *cptr = '\0';
      if ( cptr=strchr( dataline, '\n')) *cptr = '\0';
      if ( strcmp( fm->lbl_txt, lblline)) {
        strcpy( dataline, "Bad Label");
        badlab = 1;
      } else {
        badlab = 0;
      }
    }

    switch ( fm->type) {
    case FLD_TXT:
    case FLD_TXTFILEREAD:
    case FLD_TXTFILEWRITE:
    case FLD_INT:
    case FLD_LNG:
    case FLD_LNG8:
    case FLD_FORTINT:
    case FLD_DBL:
    case FLD_DBL8:
    case FLD_UTC:
      if ( dowrt) fprintf( f, "%s\n", fld_data);
      else {
        XmTextFieldSetString( fm->fld_w, dataline);
      }
      break;

    /* read slider */
    case FLD_SLIDER_UPLEFT:
    case FLD_SLIDER_UPRIGHT:
      if ( dowrt) {
        XmScaleGetValue( fm->fld_w, &scaleint);
        fprintf( f, "%d\n", scaleint);
      } else {
        if ( !badlab) i = sscanf( dataline, "%d", &scaleint);
        if ( i) XmScaleSetValue( fm->fld_w, scaleint);
      }  
      break;

    /* read toggle buttons, set bits accordingly using mask */
    case FLD_BIT:
    case FLD_BITS:
      for ( i=0; i<fm->fld_numbits; i++) {
        if ( dowrt) {
          fprintf( f, "%c", XmToggleButtonGetState(fm->fld_ws[i]) ? '1' : '0');
        } else {
          if ( dataline[i] == '1') {
            XmToggleButtonSetState( fm->fld_ws[i], True, False);
          } else {
            XmToggleButtonSetState( fm->fld_ws[i], False, False);
          }
        }
      }
      if ( dowrt) fprintf( f, "\n");
      break;
    }
    if ( fld_data) XtFree ( fld_data);
  } /* for fm... */
  if ( f != stderr) fclose( f);
  else fflush( f);
}

void
orbitgui_fldmenu_load_CB( Widget w, XtPointer client_data, XtPointer call_data)
{ 
  orbitgui_fldmenu_loadsave_CB( w, client_data, call_data, 0); 
}

void
orbitgui_fldmenu_save_CB( Widget w, XtPointer client_data, XtPointer call_data)
{ 
  orbitgui_fldmenu_loadsave_CB( w, client_data, call_data, 1); 
}

/**********************************************************/
/* save/load imgfrm structures to/from file - bring up file selection dialog */
void
orbitgui_fldmenu_FSB_CB( Widget w, XtPointer client_data, XtPointer call_data
                        , int dowrt)
{
Widget dialog;
void (*do_search)();
void (*doit_CB)();
char lcls[200];
XmString title_string= NULL;
XmString pattern_string= NULL;
FLDMENU *fm = (FLDMENU *)client_data;
Widget shell = w;

  while ( !XtIsShell(shell)) shell = XtParent(shell);

  sprintf( lcls, "%s File Name Selection", dowrt ? "Save" : "Load");

  title_string=XmStringCreateLtoR( lcls, CHARSET);

  /* last fm structure has pattern string in member ->fld_txt0 */
  for ( ; fm->type != FLD_END; ++fm) ;
  pattern_string=XmStringCreateLtoR(fm->fld_txt0, CHARSET);

  n = 0;
  XtSetArg (args[n], XmNpattern, pattern_string); n++;
  XtSetArg (args[n], XmNdialogTitle, title_string); n++;
  XtSetArg (args[n], XmNfileSearchProc, dowrt ? do_writeable_search 
                                              : do_readable_search); n++;
  dialog = XmCreateFileSelectionDialog (shell, "Files", args, n);

  XtSetSensitive (
      XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);

  XtAddCallback (dialog, XmNokCallback, dowrt ? orbitgui_fldmenu_save_CB
                                              : orbitgui_fldmenu_load_CB
                                      , client_data);
  XtAddCallback (dialog, XmNcancelCallback, MyDW_CB, NULL);

  XtManageChild (dialog);

  XmSF( title_string);
  XmSF( pattern_string);

  return;
}

void
orbitgui_fldmenu_FSB_load_CB( Widget w
                            , XtPointer client_data, XtPointer call_data)
{ orbitgui_fldmenu_FSB_CB( w, client_data, call_data, 0); }

void
orbitgui_fldmenu_FSB_save_CB( Widget w
                            , XtPointer client_data, XtPointer call_data)
{ orbitgui_fldmenu_FSB_CB( w, client_data, call_data, 1); }

/**********************************************************************/
/* orbitgui_fldmenu_cancel_CB() -- callback for when the user hits 
 * the cancel button
 */
void
orbitgui_fldmenu_cancel_CB( Widget button_W, XtPointer client_data
                                           , XtPointer call_data) {
Widget dialog = button_W;
FLDMENU *fldMenu = (FLDMENU *) client_data;
FLDMENU *fm = fldMenu;

  /* DPR1( "in fldmenu_cancel_CB() // "); /**/

  for ( ; dialog && !XmIsDialogShell( dialog); dialog = XtParent( dialog)) ;

  /* fprintf( stderr
         , "in fldm cancel cb, fldmenu = %08xx, (dialog==button) = %d\n"
         , fldMenu, (dialog == button_W) ), fflush( stderr); /**/

  /* clean up the dialog 
   * - if this dialog called here (XmNdestroyCallback), pass through 
   *   here and free fldMenu
   * - if the cancel button (or any other child of the dialog) called here, 
   *   destroy dialog & return
   *   - destroying dialog will call here again via XmNdestroyCallback callback 
   */

  if ( dialog != button_W) { 
    XtDestroyWidget( dialog);
    return;
  }

  /* find the last FLDMENU structure 
   *  - call its application cancel callback if it exists
   *    - the callback is responsible for freeing fldMenu
   */
  for ( ; fm->type != FLD_END; fm++) ;
  if ( fm->client_call) (*fm->client_call)( fldMenu->client_data, fldMenu);

  return;
}

/**********************************************/
/* read a single long or NOCHANGE */

int
orbitgui_fldmenu_sscanf_long( char *inStr, long *longPtr) {
int retVal = 1;
  if ( 1 != sscanf( inStr, "%ld", longPtr)) {
  char *cPtr;
    for( cPtr=inStr; *cPtr; ++cPtr) { *cPtr = toupper( *cPtr); }
    if ( !strcmp( "NOCHANGE", inStr)) {
      *longPtr = ORBIT_NOCHANGEVAL;
    } else {
      retVal = 0;
    }
  }
  return retVal;
}

/*****************************************************************************/
/* orbitgui_fldmenu_readOnlyFlagFunction() - dummy callback that should never 
 * be called, is only used to indicate to orbitgui_create_fldmenu_dialog() that
 * OK button should be disabled (i.e. set insensitive)
 */
int
orbitgui_fldmenu_readOnlyFlagFunction( void *v, FLDMENU *f) { return 0; }

/*****************************************************************************/
/* orbitgui_fldmenu_ok_CB() -- callback for when the user hits the ok button */

void
orbitgui_fldmenu_ok_CB( Widget button_W, XtPointer client_data
                                       , XtPointer call_data) {
FLDMENU *fldMenu = (FLDMENU *) client_data;
FLDMENU *fm;

char fld_sfmt[80];
long mask;
int i;
double lcldbl[8];
long lcllng[8];
char scn[8][FLD_MAXTXTLENPERLNG+1];
int scaleint;

  /* We should not get here with
   * fldMenu->client_call == orbitgui_fldmenu_readOnlyFlag because OK button 
   * should be disabled, if but check it here anyway just in case
   */
  if ( fldMenu->client_call == orbitgui_fldmenu_readOnlyFlagFunction) return;

  /* DPR1("in _fldmenu_ok_CB // "); /**/

  /* go through each field-menu field and get the data */
  for ( fm=fldMenu; fm->type != FLD_END; fm++) {
    char *fld_data = ((fm->type == FLD_BIT)
                     ||(fm->type == FLD_BITS) 
                     ||(fm->type == FLD_SLIDER_UPLEFT) 
                     ||(fm->type == FLD_SLIDER_UPRIGHT)
                     ) ? (char *) 0
                       : XmTextFieldGetString (fm->fld_w);

    /* FLD_TXT[*]/INT/DBL:  test if anything has changed; if yes, then read it
     *                      - copy it back to fld_txt0 in case OK does not
     *                        remove this menu
     * FLD_BIT/BITS:  set bits according to each toggle button 
     * FLD_SLIDER_UP*:  read the scale, put it into the variable 
     *                  via the intermediate int scaleint
     */
    switch ( fm->type) {

    /* text field:  read up to _maxlen-1 characters */
    case FLD_TXT:
    case FLD_TXTFILEREAD:
    case FLD_TXTFILEWRITE:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      strncpy( fm->fld_txt, fld_data, fm->fld_txt_maxlen);
      fm->fld_txt[fm->fld_txt_maxlen-1] = '\0';
      strcpy( fm->fld_txt0, fm->fld_txt);
      break;

    /* read an int */
    case FLD_INT:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      if ( strlen( fld_data) ) {
        sscanf( fld_data, "%d", fm->fld_int);
      } else {
        *fm->fld_int = 0;
        XmTextFieldSetString( fm->fld_w, "0");
      }
      strcpy( fm->fld_txt0, fld_data);
      break;

    /* read a long - allow for NOCHANGE */
    case FLD_LNG:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      if ( !orbitgui_fldmenu_sscanf_long( fld_data, fm->fld_lng)) {
        *fm->fld_lng = 0;
        XmTextFieldSetString( fm->fld_w, "0");
      }
      strcpy( fm->fld_txt0, fld_data);
      break;

    /* read up to 8 longs - allow for NOCHANGE */
    case FLD_LNG8:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      /* build format string:  _maxlen %Ns's */
      for ( fld_sfmt[0] = i = 0; i< fm->fld_txt_maxlen; ++i) {
      char *cPtr;
        cPtr = fld_sfmt + strlen(fld_sfmt);
        sprintf( cPtr, "%%%ds ", FLD_MAXTXTLENPERLNG);
        /* strcat( fld_sfmt,"%ld "); */
      }

      /* parse the input line into individual strings */

      i = sscanf( fld_data, fld_sfmt, scn[0], scn[1], scn[2]
                                , scn[3], scn[4], scn[5]
                                , scn[6], scn[7]);

      /* limit the number of values returned */
      fm->subtype = (i < fm->fld_txt_maxlen) ? ((i<0) ? 0 : i) 
                                             : fm->fld_txt_maxlen;

      for ( i=0; i < fm->subtype; ++i) {
        /* read either a long or NOCHANGE - returns 0 on failure */
        if ( !orbitgui_fldmenu_sscanf_long( scn[i], fm->fld_lng+i)) {
          fm->subtype = i;       /* if nothing read, set the number of values */
        }                               /* returned & force break out of loop */
      }

      if ( !fm->subtype) {  /* if # values read is 0, interpret string as "0" */
        *fm->fld_lng = 0;
        fm->subtype = 1;
        XmTextFieldSetString( fm->fld_w, "0");
      }
      strcpy( fm->fld_txt0, fld_data);
      break;

    /* read a double */
    case FLD_DBL:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      if ( strlen( fld_data) ) {
        sscanf( fld_data, "%lf", fm->fld_dbl);
      } else {
        *fm->fld_dbl = 0.0;
        XmTextFieldSetString( fm->fld_w, "0");
      }
      /* DPR(( stderr, "fld_data/fld_dbl/*fld_dbl = '%s'/%08x/%lf\n"
             , fld_data, (int)fm->fld_dbl, *fm->fld_dbl)); /**/
      strcpy( fm->fld_txt0, fld_data);
      break;

    /* read a UTC */
    case FLD_UTC:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      if ( strlen( fld_data) ) {
        orbit_utc2et( fld_data, fm->fld_dbl);
        strcpy( fm->fld_txt0, fld_data);
      } else {
        strcpy( fld_data, fm->fld_txt0);
      }
      break;

    /* read a couple of doubles */
    case FLD_DBL8:
      if ( !strcmp( fm->fld_txt0, fld_data)) break;
      if ( strlen( fld_data) ) {
        /* build format string : _maxlen %lf's */
        for ( fld_sfmt[0] = i = 0; i< fm->fld_txt_maxlen; ++i) {
          strcat( fld_sfmt,"%lf ");
        }
        /* read the values */
        i = sscanf( fld_data, fld_sfmt, lcldbl+0, lcldbl+1, lcldbl+2
                                  , lcldbl+3, lcldbl+4, lcldbl+5
                                  , lcldbl+6, lcldbl+7);
        /* limit the number of values returned */
        fm->subtype = (i < fm->fld_txt_maxlen) ? i : fm->fld_txt_maxlen;
        for ( i=0; i < fm->subtype; ++i) fm->fld_dbl[i] = lcldbl[i];
      } else {
        *fm->fld_dbl = 0.0;
        fm->subtype = 1;
        XmTextFieldSetString( fm->fld_w, "0");
      }
      strcpy( fm->fld_txt0, fld_data);
      break;

    /* read slider */
    case FLD_SLIDER_UPLEFT:
    case FLD_SLIDER_UPRIGHT:
      XmScaleGetValue( fm->fld_w, &scaleint);
      /* load scale bar from int, long or fortint subtype via scaleint */
      switch (fm->subtype) {
      case FLD_INT:
        *fm->fld_int = scaleint;
        break;
      case FLD_LNG:
        *fm->fld_lng = scaleint;
        break;
      case FLD_FORTINT:
        *fm->fld_fortint = scaleint;
        break;
      }
      break;

    /* read toggle buttons, set bits accordingly using mask */
    case FLD_BIT:
    case FLD_BITS:
      mask = 1L << fm->fld_lowbit;
      for ( i=0; i<fm->fld_numbits; i++, mask = mask << 1) {
        if ( XmToggleButtonGetState(fm->fld_ws[i]) ) {
          *fm->fld_lng |= mask; /* set a bit */
        } else {
          *fm->fld_lng &= (~mask); /* clear a bit */
        }
      }
      break;
    }
    if ( fld_data) XtFree ( fld_data);
  }


  /* call the application OK callback if it exists */
  if ( fldMenu->client_call) {
  int nonzerotodestroy;
  Widget dlgshell = button_W;
  Widget topshell;

    for ( ; dlgshell && !XmIsDialogShell( dlgshell)
          ; dlgshell = XtParent( dlgshell)) ;
    for ( topshell = dlgshell; topshell && !XtIsTopLevelShell(topshell)
        ; topshell = XtParent(topshell)) ;

    orbitgui_watchCursor( dlgshell, True);
    orbitgui_watchCursor( topshell, True);

    nonzerotodestroy = (*fldMenu->client_call)( fldMenu->client_data, fldMenu);
    orbitgui_watchCursor( dlgshell, False);
    orbitgui_watchCursor( topshell, False);
    if ( nonzerotodestroy) {
      /* ok callback is responsible for freeing fldMenu
       * , so remove dialog destroy callback to fldmenu_cancel_CB
       */
      XtRemoveCallback( dlgshell, XmNdestroyCallback, orbitgui_fldmenu_cancel_CB
                      , fldMenu);
      XtDestroyWidget( dlgshell);
    }

  } else {
  Widget dlgshell = button_W;

    for ( ; dlgshell && !XmIsDialogShell( dlgshell)
          ; dlgshell = XtParent( dlgshell)) ;
    /* this should activate the fldMenu_cancel_CB which frees fldMenu */
    XtDestroyWidget( dlgshell);
  }
  return;
}

/*****************************************************************************/
/* orbitgui_fldmenu_reset_CB() -- callback for when the user hits reset */

void
orbitgui_fldmenu_reset_CB( Widget button_W, XtPointer client_data, XtPointer call_data)
{
FLDMENU *fldMenu = (FLDMENU *) client_data;
FLDMENU *fm;

long mask;
int i;
int scaleint;

  /* DPR1( "in fldmenu_reset_CB() // "); /**/

  /* go through each field-menu field and reset field to _txt0 */
  for ( fm=fldMenu; fm->type != FLD_END; fm++) {
    switch( fm->type) {
    case FLD_DBL8:
    case FLD_DBL:
    case FLD_UTC:
    case FLD_INT:
    case FLD_LNG:
    case FLD_LNG8:
    case FLD_TXT:
    case FLD_TXTFILEREAD:
    case FLD_TXTFILEWRITE:
      XmTextFieldSetString( fm->fld_w, fm->fld_txt0);
      break;
    case FLD_SLIDER_UPLEFT:
    case FLD_SLIDER_UPRIGHT:
      switch (fm->subtype) {
      case FLD_INT:
        scaleint = *fm->fld_int;
        break;
      case FLD_LNG:
        scaleint = *fm->fld_lng;
        break;
      case FLD_FORTINT:
        scaleint = *fm->fld_fortint;
        break;
      }
      XmScaleSetValue( fm->fld_w, scaleint);
      break;
    case FLD_BIT:
    case FLD_BITS:
      mask = 1L << fm->fld_lowbit;
      for ( i=0; i<fm->fld_numbits; i++, mask = mask << 1) {
        if ( mask & *fm->fld_lng) {
          XmToggleButtonSetState( fm->fld_ws[i], True, False);
        } else {
          XmToggleButtonSetState( fm->fld_ws[i], False, False);
        }
      }
      break;
    }
  }
  return;
}

/**********************************************************************/
/* orbitgui_fldmenu_rtn_CB() -- callback for when the user hits 
 *                                return in a string TextField widget.
 */
void
orbitgui_fldmenu_rtn_CB(text_w, client_data, call_data)
Widget text_w;
XtPointer client_data;
XtPointer call_data;
{
    /* DPR1( "in fldmenu_rtn_CB() // "); /**/
    XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
}

#define ISREAD (fm->type == FLD_TXTFILEREAD)

void
orbitgui_fldmenu_FSD_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
FLDMENU *fm = (FLDMENU *) client_data;
char *filename;

char *check_writeable();
char *check_readable();

  filename = ISREAD ? check_readable( w, client_data, call_data)
                    : check_writeable( w, client_data, call_data);
  if ( !filename) return;

  XmTextFieldSetString( fm->fld_w, filename);

  if ( filename) XtFree( filename);
  filename = (char *) 0;
  MyDW( w);
  return;
}

/**********************************************************/
/* setup to bring up file sel dialog */

void
orbitgui_fldmenu_FSD(Widget w, XtPointer client_data, XtPointer call_data)
{
FLDMENU *fm = (FLDMENU *) client_data;
Widget dialog;
void do_writeable_search();
void do_readable_search();
XmString title_string= NULL;
XmString pattern_string= NULL;
char *currentString = NULL;
char *firstAst, *lastDot, *lastSlash;
Widget        pmw = w;  /* parent managed widget */

  for ( ; pmw && !XtIsWMShell(pmw); pmw = XtParent(pmw)) ;

  /* build wildcard from current value in text widget */

  currentString = XmTextFieldGetString( fm->fld_w);

  /* if current value has a wildcard ("*"), use current value */

  if ( (firstAst=strchr( currentString, '*'))) {

    pattern_string = XmStringCreateLtoR( currentString, CHARSET);

  } else {

    /* find last dot and last slash */

    lastDot = strrchr( currentString, '.');
    lastSlash = strrchr( currentString, '/');

    /* if last dot is after the last slash, then use current value with
     * wildcard ("*") replacing anything between last slash and last dot
     * e.g.
     *    /a/b/c/de.f => /a/b/c/*.f
     *    gh.ij       => *.ij
     *    kl.mn.op    => *.op
     */

    if ( lastDot > lastSlash) {
    char *c = currentString;
    /* allocate space for current string 
     * + terminator
     * + 1 in case lastDot == currentString || (lastDot == (lastSlash + 1))
     */
    char *wildCard = (char *) malloc( strlen(currentString) + 2);
    char *cwc = wildCard;
      while ( c<=lastSlash && *c) *(cwc++) = *(c++);/* copy through lastSlash */
      *(cwc++) = '*';                                           /* append "*" */
      c = lastDot;
      while ( *c) *(cwc++) = *(c++);            /* append from lastDot to end */
      *cwc = '\0';                                       /* append terminator */
      pattern_string = XmStringCreateLtoR( wildCard, CHARSET);
    } else {
      pattern_string=XmStringCreateLtoR( "*", CHARSET);
    }
  }

  n = 0;
  XtSetArg (args[n], XmNpattern, pattern_string); n++;

  title_string=XmStringCreateLtoR( fm->lbl_txt, CHARSET);
  XtSetArg (args[n], XmNdialogTitle, title_string); n++;

  XtSetArg (args[n], XmNfileSearchProc, (ISREAD ? do_readable_search
                                                : do_writeable_search)); n++;
  dialog = XmCreateFileSelectionDialog ( pmw, "Files", args, n);
  XtSetSensitive (
      XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
  XtAddCallback (dialog, XmNokCallback, orbitgui_fldmenu_FSD_CB, client_data);
  XtAddCallback (dialog, XmNcancelCallback, MyDW_CB, NULL);

  XtManageChild (dialog);

  XmSF( title_string);
  XmSF( (void *) currentString);
  XmSF( pattern_string);

  return;
}

/**********************************************************************/
/* orbitgui_create_fldmenu_dialog -- create dialog  of field-menu */
void
orbitgui_create_fldmenu_dialog( Widget w
                              , char *title
                              , FLDMENU *fldMenu
                              )
{
Widget        dialog, form, sw, rowcol, rowcol2, miscw, form2, previeW;
Widget        bottomRowCol;
Widget        pmw = w;  /* parent managed widget */
int           i, numrows;
FLDMENU       *fm = fldMenu;
char          lclstr[30];
char          *cptr;
Arg           args[10];
int           n;
short         iii;
long          mask;
char          sfmt[80];
void          orbitgui_displayfile(Widget, XtPointer, XtPointer);
int           scaleint;
void orbitgui_setSensitivity( Widget, Widget);

  /* get manager widget of caller so this dialog will be destroyed 
   * if caller is destroyed 
   */
  for ( ; pmw && !XtIsWMShell(pmw); pmw = XtParent(pmw)) ;

  /* DPR1( "in create_fldmenu_dialog() // "); /**/

  /* find # of rows */
  for ( numrows=0; fm->type != FLD_END ; numrows++,fm++) ;

  /* create dialog, scrolled and RowColumn widgets */
  dialog = XtVaCreateWidget( title
                         , xmDialogShellWidgetClass, pmw
                         , XmNallowShellResize, True
                         , XmNdeleteResponse, XmDESTROY
                         , NULL);
  XtAddCallback(dialog,XmNdestroyCallback, orbitgui_fldmenu_cancel_CB, fldMenu);
  orbitgui_setSensitivity( w, dialog);

# define OFM_MAXROWS 15
  if ( numrows <= 15) {
    form = sw = dialog;
  } else { 
    form = XtVaCreateWidget( "form"
                                  , xmFormWidgetClass, dialog
                                  , XmNresizable, True
                                  , NULL);
    sw = XtVaCreateWidget( "scrolledrowcol"
       , xmScrolledWindowWidgetClass, form
       , XmNscrollingPolicy, XmAUTOMATIC
       , XmNvisualPolicy, XmCONSTANT
       , XmNleftAttachment, XmATTACH_FORM
       , XmNtopAttachment, XmATTACH_FORM
       , XmNscrollBarDisplayPolicy, XmAS_NEEDED
       , NULL);
  }

  /* numrows + 3 rows:  separators, OK/Reset, Cancel/Load/Save */

  rowcol = XtVaCreateWidget( "rowcol"
                           , xmRowColumnWidgetClass, sw /* dialog /**/
                           , XmNpacking, XmPACK_COLUMN /**/
                           , XmNnumColumns
                           , (short) numrows + ((sw==dialog) ? 3 : 0)
                           , XmNorientation, XmHORIZONTAL
                           , NULL);

  bottomRowCol = (sw==dialog) 
    ? rowcol
    : XtVaCreateWidget( "bottomRowCol"
                      , xmRowColumnWidgetClass, form
                      , XmNpacking, XmPACK_COLUMN
                      , XmNnumColumns
                      , (short) 2
                      , XmNorientation, XmHORIZONTAL
                      , XmNleftAttachment, XmATTACH_FORM
                      , XmNbottomAttachment, XmATTACH_FORM
                      , XmNtopAttachment, XmATTACH_WIDGET
                      , XmNtopWidget, sw
                      , NULL);

  DPR((stderr, "dialog rowcol = %08xx %08xx //", dialog, rowcol));

  /* loop through FLDMENU structures:
     - create label and text field widgets
     - fill in field for int's, long's and double's OR set radio/check buttons
     - save initial text in fm->fld_txt0 for reset button
   */

  for (fm=fldMenu; fm->type != FLD_END; fm++) {
    miscw= XtVaCreateManagedWidget ( fm->lbl_txt,
                                   xmLabelGadgetClass, rowcol, 
                                   NULL);

    switch( fm->type) {
    case FLD_DBL8:
      /* put from 1 to fm->subtype doubles into fm->fld_txt0 string */
      if ( fm->subtype) sprintf( fm->fld_txt0, FLD_DBLFMT, *fm->fld_dbl);
      else *fm->fld_txt0 = '\0';
      for ( i=1; i<fm->subtype; ++i) {
        sprintf( lclstr, FLD_DBLFMT, fm->fld_dbl[i]);
        strcat( fm->fld_txt0, " ");
        strcat( fm->fld_txt0, lclstr);
      }
      break;
    case FLD_LNG:
      sprintf( fm->fld_txt0
             , (*fm->fld_lng != ORBIT_NOCHANGEVAL) ? FLD_LNGFMT : "NOCHANGE"
             , *fm->fld_lng);
      break;
    case FLD_LNG8:
      /* put from 1 to fm->subtype longs into fm->fld_txt0 string */
      *fm->fld_txt0 = '\0';
      for ( i=0; i<fm->subtype; ++i) {
        sprintf( lclstr
              , (fm->fld_lng[i] != ORBIT_NOCHANGEVAL) ? FLD_LNGFMT : "NOCHANGE"
              , fm->fld_lng[i]);
        if (i) strcat( fm->fld_txt0, " ");
        strcat( fm->fld_txt0, lclstr);
      }
      break;
    case FLD_DBL:
      sprintf( fm->fld_txt0, FLD_DBLFMT, *fm->fld_dbl);
      break;
    case FLD_UTC:
      orbit_et2doy( fm->fld_dbl, fm->fld_txt0, fm->fld_txt_maxlen);
      break;
    case FLD_INT:
      sprintf( fm->fld_txt0, FLD_INTFMT, *fm->fld_int);
      break;
    case FLD_TXT:
    case FLD_TXTFILEREAD:
    case FLD_TXTFILEWRITE:
      strcpy( fm->fld_txt, fm->fld_txt0);
      break;
    }

    switch( fm->type) {
    case FLD_DBL8:
    case FLD_DBL:
    case FLD_UTC:
    case FLD_INT:
    case FLD_LNG:
    case FLD_LNG8:
    case FLD_TXT:
      fm->fld_w = XtVaCreateManagedWidget ("text_w",
                                      xmTextFieldWidgetClass, rowcol,
                                      XmNtraversalOn, True,
                                      NULL);
      XmTextFieldSetString( fm->fld_w, fm->fld_txt0);
      break;

    case FLD_TXTFILEREAD:
    case FLD_TXTFILEWRITE:
      form2 = XtVaCreateManagedWidget( "form2"
                           , xmFormWidgetClass, rowcol
                           , NULL);
      previeW = XtVaCreateManagedWidget( "Preview"
                               , xmPushButtonWidgetClass, form2
                               , XmNrightAttachment, XmATTACH_FORM
                               , XmNtopAttachment, XmATTACH_FORM
                               , XmNbottomAttachment, XmATTACH_FORM
                               , XmNtraversalOn, True
                               , NULL);
      miscw = XtVaCreateManagedWidget( "Browse"
                               , xmPushButtonWidgetClass, form2
                               , XmNrightAttachment, XmATTACH_WIDGET
                               , XmNrightWidget, previeW
                               , XmNtopAttachment, XmATTACH_FORM
                               , XmNbottomAttachment, XmATTACH_FORM
                               , XmNtraversalOn, True
                               , NULL);
      XtAddCallback ( miscw, XmNactivateCallback,
                       orbitgui_fldmenu_FSD, (XtPointer) fm);
      fm->fld_w = XtVaCreateManagedWidget("text_w"
                               , xmTextFieldWidgetClass, form2
                               , XmNtraversalOn, True
                               , XmNleftAttachment, XmATTACH_FORM
                               , XmNtopAttachment, XmATTACH_FORM
                               , XmNbottomAttachment, XmATTACH_FORM
                               , XmNrightAttachment, XmATTACH_WIDGET
                               , XmNrightWidget, miscw
                               , NULL);
      XmTextFieldSetString( fm->fld_w, fm->fld_txt0);

      XtAddCallback ( previeW, XmNactivateCallback,
                       orbitgui_displayfile, (XtPointer) fm->fld_w);

      break;

    case FLD_BIT:
    case FLD_BITS:
      for( iii = 0, cptr = fm->lbl_txt + 1 + strlen(fm->lbl_txt);
           *cptr; cptr += (1 + strlen(cptr)) ) iii++;
      fm->fld_numbits = iii;
      rowcol2 = XtVaCreateManagedWidget( "rowcol2"
                           , xmRowColumnWidgetClass, rowcol
                           , XmNpacking, XmPACK_COLUMN /**/
                           /* , XmNnumColumns, (short) 2 /**/
                           /* , XmNorientation, XmHORIZONTAL /**/
                           , XmNnumColumns, (short) 1 /* fm->fld_numbits /**/
                           , XmNorientation, XmHORIZONTAL /**/
                           , XmNradioBehavior
                             , (fm->type == FLD_BIT) ? True : False
                           , XmNradioAlwaysOne
                             , (fm->type == FLD_BIT) ? True : False
                           , NULL);

      n = 0;
      mask = 1 << fm->fld_lowbit;
      for( iii = 0, cptr = fm->lbl_txt + 1 + strlen(fm->lbl_txt);
           iii < fm->fld_numbits; cptr += (1 + strlen(cptr)) ) {
        /* trim leading space if something follows it 
         * so toggle labels can start with numeric characters 
         * e.g. if main label is "xyz" and toggle labels are "1" & "2",
         * the combined label would be "xyz\01\02\0", but C interprets 
         * the character sequence "\02" as the single character with a 
         * value of octal 2.  so enable the user to precede a togglebutton
         * label with a space.  the next line advances the character pointer 
         * by 1 if it points to a space.  this also shortens the length 
         * of the string by one so the "cptr +=" end-of-loop code above 
         * still works.
         */
        if ( *cptr == ' ' && strlen(cptr) > 1) cptr++;

        fm->fld_ws[iii] =
          XtCreateManagedWidget( cptr, xmToggleButtonWidgetClass, rowcol2 /**/
                               , args, n);
        if ( mask & *fm->fld_lng) {
          XmToggleButtonSetState( fm->fld_ws[iii], True, False);
        } else {
          XmToggleButtonSetState( fm->fld_ws[iii], False, False);
        }
        iii++;
        mask = mask << 1;
      }
      break;
    case FLD_SLIDER_UPLEFT:
    case FLD_SLIDER_UPRIGHT:
      fm->fld_w = XtVaCreateManagedWidget( "slider_w"
			, xmScaleWidgetClass, rowcol
			, XmNorientation, XmHORIZONTAL
			, XmNprocessingDirection, (fm->type==FLD_SLIDER_UPLEFT)
                                                  ? XmMAX_ON_LEFT
                                                  : XmMAX_ON_RIGHT
			, XmNminimum, (int) fm->fld_loend
			, XmNmaximum, (int) fm->fld_hiend
			, XmNscaleMultiple, 1
			, XmNwidth, (int) (1 + fm->fld_hiend - fm->fld_loend)
			, XmNdecimalPoints, (short) fm->fld_txt_maxlen
			, XmNshowValue, True
                        , XmNtraversalOn, True
			, NULL);
      /* load scale bar from int, long or fortint subtype via scaleint */
      switch (fm->subtype) {
      case FLD_INT:
        scaleint = *fm->fld_int;
        break;
      case FLD_LNG:
        scaleint = *fm->fld_lng;
        break;
      case FLD_FORTINT:
        scaleint = *fm->fld_fortint;
        break;
      }
      XmScaleSetValue( fm->fld_w, scaleint);
      break;
    }


    /* When user hits return, traverse fields */
    switch( fm->type) {
    case FLD_DBL8:
    case FLD_DBL:
    case FLD_UTC:
    case FLD_INT:
    case FLD_LNG:
    case FLD_LNG8:
    /* case FLD_SLIDER_UPLEFT: /**/
    /* case FLD_SLIDER_UPRIGHT: /**/
      XtAddCallback ( fm->fld_w, XmNactivateCallback,
                       orbitgui_fldmenu_rtn_CB, (XtPointer) 0);
      break;
    }

  }

  /* separators if no scroll window */

  if ( sw == dialog ) {
    miscw = XtVaCreateManagedWidget( "sep1"
                                   , xmSeparatorWidgetClass, rowcol
                                   , NULL);
    miscw = XtVaCreateManagedWidget( "sep1"
                                   , xmSeparatorWidgetClass, rowcol
                                   , NULL);
  }

  /* OK, Reset & Cancel button + a separator to even things up */

  miscw = XtVaCreateManagedWidget( "OK"
                                 , xmPushButtonWidgetClass, bottomRowCol
                                 , XmNtraversalOn, False
                                 , NULL);

  /* if OK callback set to readOnlyFlagFunction above, 
   * set the button to be insensitive
   */
  if ( fldMenu->client_call == orbitgui_fldmenu_readOnlyFlagFunction ) {
    XtSetSensitive( miscw, False);
  } else {
    XtAddCallback (miscw, XmNactivateCallback, orbitgui_fldmenu_ok_CB, fldMenu);
  }

  miscw = XtVaCreateManagedWidget( "Reset"
                                 , xmPushButtonWidgetClass, bottomRowCol
                                 , XmNtraversalOn, False
                                 , NULL);
  XtAddCallback (miscw, XmNactivateCallback,orbitgui_fldmenu_reset_CB,fldMenu);

  miscw = XtVaCreateManagedWidget( "Cancel"
                                 , xmPushButtonWidgetClass, bottomRowCol
                                 , XmNtraversalOn, False
                                 , NULL);
  XtAddCallback (miscw, XmNactivateCallback, orbitgui_fldmenu_cancel_CB, fldMenu);

  rowcol2 = XtVaCreateManagedWidget( "rowcol2"
                           , xmRowColumnWidgetClass, bottomRowCol
                           , XmNpacking, XmPACK_COLUMN
                           , XmNnumColumns, (short) 1
                           , XmNorientation, XmHORIZONTAL
                           , NULL);

  DPR((stderr, "rowcol2 = %08xx //", rowcol2));
  
  miscw = XtVaCreateManagedWidget( "Load Menu..."
                                 , xmPushButtonWidgetClass, rowcol2
                                 , XmNtraversalOn, False
                                 , NULL);
  XtAddCallback( miscw, XmNactivateCallback, orbitgui_fldmenu_FSB_load_CB
               , fldMenu);
  miscw = XtVaCreateManagedWidget( "Save Menu..."
                                 , xmPushButtonWidgetClass, rowcol2
                                 , XmNtraversalOn, False
                                 , NULL);
  XtAddCallback( miscw, XmNactivateCallback, orbitgui_fldmenu_FSB_save_CB
               , fldMenu);

  XtManageChild (rowcol);

  /* set scrolled window dimensions */
  if ( sw != dialog) {
  Widget vsb, hsb;
  Dimension rcW, rcH, vsbW, hsbH;

    XtManageChild (sw);
    XtManageChild (bottomRowCol);
    XtManageChild (form);

    XtVaGetValues( sw
                 , XmNverticalScrollBar, &vsb
                 , XmNhorizontalScrollBar, &hsb
                 , NULL);
    XtVaGetValues( vsb, XmNwidth, &vsbW, NULL);
    XtVaGetValues( hsb, XmNheight, &hsbH, NULL);
    XtVaGetValues( rowcol
                 , XmNwidth, &rcW
                 , XmNheight, &rcH
                 , NULL);
    rcH *= OFM_MAXROWS;
    rcH /= (numrows+3);
    XtVaSetValues( sw
                 , XmNwidth, rcW + ((vsbW*3)/2)
                 , XmNheight, rcH + hsbH
                 , NULL);
  }

  XtPopup ( dialog, XtGrabNone);
}
