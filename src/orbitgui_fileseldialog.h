/**********************************************************************/
/* orbitgui_fileseldialog.h - macro to setup file selection */

#ifndef _ORBITGUI_FILESELDIALOG_H_
#define _ORBITGUI_FILESELDIALOG_H_

#include <Xm/Xm.h>
#include <Xm/FileSB.h>

#include "local.h"
#include "debug.h"
#include "lesstifbug.h"

enum {
  PF_READ=0L    /* file must readable */
, PF_WRITE      /* file must be writeable */
, PF_RETURNDIR  /* PICKDIR - return writeable *DIRECTORY*, ending w/slash */
                /* PICKFILE - same as PF_WRITE */
};

#define XmFDT XmFONTLIST_DEFAULT_TAG

#define PICKFILE_CB PICKFILE_NOKEEP_CB
#define PICKFILE_NOKEEP_CB( CBNAME, RWPROC, WRITEABLE) \
        PICKFILE_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, 0)
#define PICKFILE_KEEPUP_CB( CBNAME, RWPROC, WRITEABLE) \
        PICKFILE_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, 1)

#define PICKDIR_CB PICKDIR_NOKEEP_CB
#define PICKDIR_NOKEEP_CB( CBNAME, RWPROC, WRITEABLE) \
        PICKDIR_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, 0)
#define PICKDIR_KEEPUP_CB( CBNAME, RWPROC, WRITEABLE) \
        PICKDIR_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, 1)

/**********************************************************/
/* PICKFILE_KEEPOPTION_CB - callback creation used by PICKFILE_SETUP below
 * file selection callback procedure to do something with a file
 * CBNAME = name of this callback
 * RWPROC = see PICKFILE_SETUP description below
 * WRITEABLE = see PICKFILE_SETUP description below 
 * KEEPUP = 0/1 => destroy/keep file selection dialog widget after OK
 */
#define PICKFILE_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, KEEPUP) \
void \
CBNAME(Widget PFw, XtPointer PFclient_data, XtPointer PFcall_data) { \
char *check_writeable(); \
char *check_readable(); \
char *PFfilnam; \
void RWPROC(); \
  \
  if ( !(PFfilnam = \
    /* assume writeable required unless PF_READ */ \
    ((WRITEABLE)==PF_READ) \
      ? check_readable( PFw, PFclient_data, PFcall_data) \
      : check_writeable( PFw, PFclient_data, PFcall_data)) ) \
    return; \
  \
  RWPROC( PFfilnam, PFclient_data); \
  XtFree( PFfilnam); \
  if ( !(KEEPUP)) { PFMyDW( PFw, NULL, NULL); } \
  return; \
}

/***********************************************************************/
/* PICKDIR_KEEPOPTION_CB - callback creation used by PICKDIR_SETUP below
 * file selection callback procedure to do something with files in a dir
 * CBNAME = name of this callback
 * RWPROC = see RWPROC in PICKFILE_SETUP description below
 * WRITEABLE = see WRITEABLE in PICKFILE_SETUP description below
 * KEEPUP = see KEEPUP in PICKFILE_SETUP description below
 */
#define PICKDIR_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, KEEPUP) \
void \
CBNAME(Widget PFw, XtPointer PFclient_data, XtPointer PFcall_data) { \
char *check_writeable(); \
char *check_readable(); \
char *PFfilnam; \
void RWPROC(); \
XmStringTable fileListItems; \
XmString directory; \
int fileListItemCount, i; \
char *filePtr; \
char *dirPtr; \
XmFileSelectionBoxCallbackStruct *cbs = \
  (XmFileSelectionBoxCallbackStruct *) PFcall_data; \
  \
  XtVaGetValues( PFw \
               , XmNfileListItems, &fileListItems \
               , XmNfileListItemCount, &fileListItemCount \
               , XmNdirectory, &directory \
               , NULL); \
  \
  DPR(( stderr, "DSB callback:")); \
  DPR(( stderr, " -%d items at 0x%08x\n", fileListItemCount, fileListItems)); \
  \
  if ( (WRITEABLE) == PF_RETURNDIR ) { \
    if ( XmStringGetLtoR( directory, XmFDT, &dirPtr)) { \
      RWPROC( dirPtr, PFclient_data);                 /* here's the beef */ \
      XtFree( dirPtr); \
    } \
  } else { \
    for ( i=0; i<fileListItemCount; ++i) {            /* step through files */ \
      DPR(( stderr, " -[%02d](0x%08x) =>", i, fileListItems[i])); \
      if ( XmStringGetLtoR( fileListItems[i], XmFDT, &filePtr)) { \
        DPR(( stderr, " filePtr at (0x%08x) is\n    >%s<\n", filePtr \
                    , filePtr ? filePtr : "AAAACK! (null)")); \
        RWPROC( filePtr, PFclient_data);                 /* here's the beef */ \
        XtFree( filePtr); \
      } else { \
        DPR(( stderr, " AAAACK! (XmStringGetLtoR failed)\n")); \
      } \
    } /* for i < fileListItemCount */ \
  } /* if WRITABLE == PF_RETURNDIR ... else ... */ \
  if ( !(KEEPUP)) { PFMyDW( PFw, NULL, NULL); } \
  \
  return; \
}

/**********************************************************/
/* macros to make PICKDIR/FILE_SETUP default to NOT keeping fsd widget up
 */
#define PICKDIR_SETUP PICKDIR_NOKEEP_SETUP
#define PICKDIR_NOKEEP_SETUP( FN, L, FILT, CB, RWP, W)\
        PICKDIR_KEEPOPTION_SETUP( FN, L, FILT, CB, RWP, W, 0)
#define PICKDIR_KEEPUP_SETUP( FN, L, FILT, CB, RWP, W)\
        PICKDIR_KEEPOPTION_SETUP( FN, L, FILT, CB, RWP, W, 1)

#define PICKFILE_SETUP PICKFILE_NOKEEP_SETUP
#define PICKFILE_NOKEEP_SETUP( FN, L, FILT, CB, RWP, W)\
        PICKFILE_KEEPOPTION_SETUP( FN, L, FILT, CB, RWP, W, 0)
#define PICKFILE_KEEPUP_SETUP( FN, L, FILT, CB, RWP, W)\
        PICKFILE_KEEPOPTION_SETUP( FN, L, FILT, CB, RWP, W, 1)

/**********************************************************/
/* button callback to bring up file sel dialog
 * FNNAME = name of this procedure
 * LABELTEXT = label to put in header of file sel dialog
 * FILTER = initial string filter for filenames
 * CBNAME = name of callback procedure
 * WRITEABLE = PF_READ to use do_readable_search() to filter for files;
 *             PF_WRITE to use do_writeable_search();
 *             PF_RETURNDIR to use do_writeable_search(), return *DIRECTORY* 
                            portion of filter with slash as last character
 * RWPROC = name of user-supplied procedure that reads or writes the file i.e.
 *            RWPROC( char *filnam, XtPointer pointer);
 */

#define PICKDIR_KEEPOPTION_SETUP( FNNAME, LABELTEXT, FILTER\
                                , CBNAME, RWPROC, WRITEABLE, KEEPUP)\
PICKDIR_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, KEEPUP) /* see macro above */\
PICK_SETUP2( FNNAME, LABELTEXT, FILTER, CBNAME, RWPROC, WRITEABLE,1)

#define PICKFILE_KEEPOPTION_SETUP( FNNAME, LABELTEXT, FILTER\
                                 , CBNAME, RWPROC, WRITEABLE, KEEPUP)\
PICKFILE_KEEPOPTION_CB( CBNAME, RWPROC, WRITEABLE, KEEPUP)\
PICK_SETUP2( FNNAME, LABELTEXT, FILTER, CBNAME, RWPROC, WRITEABLE,0)

#define PICK_SETUP2( FNNAME, LABELTEXT, FILTER, CBNAME, RWPROC, WRITEABLE,DIRS)\
void \
FNNAME(Widget PFw, XtPointer PFclient_data, XtPointer PFcall_data) \
{ \
Widget PFdialog; \
void do_writeable_search(); \
void do_readable_search(); \
XmString PFtitle_string= NULL; \
XmString PFpattern_string= NULL; \
Arg PFargs[10]; \
int PFn; \
Widget PFshell = XtParent(PFw); \
 \
  while (!XtIsShell(PFshell)) PFshell = XtParent(PFshell); \
 \
  PFtitle_string=XmStringCreateLtoR(LABELTEXT, XmSTRING_DEFAULT_CHARSET); \
  PFpattern_string=XmStringCreateLtoR(FILTER, XmSTRING_DEFAULT_CHARSET); \
  PFn = 0; \
  XtSetArg (PFargs[PFn], XmNpattern, PFpattern_string); PFn++; \
  XtSetArg (PFargs[PFn], XmNdialogTitle, PFtitle_string); PFn++; \
  XtSetArg (PFargs[PFn], XmNfileSearchProc \
             /* assume only PF_READ needs to be writeable */ \
           , (WRITEABLE==PF_READ) ? do_readable_search \
                                  : do_writeable_search); PFn++; \
  PFdialog = XmCreateFileSelectionDialog ( PFshell, "Files", PFargs, PFn); \
  XtSetSensitive ( \
      XmFileSelectionBoxGetChild (PFdialog, XmDIALOG_HELP_BUTTON), False); \
  if ( DIRS) { \
    XtUnmanageChild ( \
      XmFileSelectionBoxGetChild (PFdialog, XmDIALOG_TEXT)); \
    XtUnmanageChild ( \
      XmFileSelectionBoxGetChild (PFdialog, XmDIALOG_SELECTION_LABEL)); \
  } \
  XtAddCallback (PFdialog, XmNokCallback, CBNAME, PFclient_data); \
  XtAddCallback (PFdialog, XmNcancelCallback, PFMyDW, NULL); \
 \
  XtManageChild (PFdialog); \
 \
  PFXmSF( PFtitle_string); \
  PFXmSF( PFpattern_string); \
 \
  return; \
}

#define PFXmSF( A) if (A) { XmStringFree(A); A=(XmString)0; }

static 
void PFMyDW( Widget PFw, XtPointer PFclient_data, XtPointer PFcall_data) {
#ifdef LesstifFileSBBug
  LesstifFileSBBugFree(PFw)
#endif
  XtDestroyWidget(PFw);
  return;
}

#endif /* _ORBITGUI_FILESELDIALOG_H_ */
