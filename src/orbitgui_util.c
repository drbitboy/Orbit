/* modified for use with orbit program - BTCarcich */

/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* file_sel.c -- file selection dialog displays a list of all the writeable
 * files in the directory described by the XmNmask of the dialog.
 * This program demonstrates how to use the XmNfileSearchProc for
 * file selection dialog widgets.
 */
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h> 

#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>

#define XmSF XmStringFree

#include "local.h"
#include "debug.h"
#include "lesstifbug.h"

/* routine to determine if a file is accessible, a directory,
 * or write|read-able.  Return -1 on all errors or if the file is not
 * write|read-able.  Return 0 if it's a directory or 1 if it's a plain
 * write|read-able file.
 */
int
is_able(char *file, int dowrt)
{
struct stat s_buf;

  /* return 2 for case where - (stderr) is output */
  /* also for default entry " [ .. ] " */
  /* ***N.B. FILE named "-" cannot be used! */
  if ( !strcmp(file," [ .. ] ") ) 
    return( -1);

  else if ( !strcmp(file,"-") ) 
    return(2);

  /* if file can't be accessed (via stat()) return, except in case where
   * we want to write to a file which does not exist but would be writeable
   * - it would not be a directory or else it would exist
   * - access won't work, assume we can open this file 
   */
  else if (stat (file, &s_buf) == -1) {
    if (dowrt && errno == ENOENT) {
      /* if ( !access( file, W_OK)) return 1; /**/
      errno == 0;
      access( file, W_OK);
      if ( ENOENT) return 1;
    }
    return -1;

  /* if file is a directory, return 0 */
  } else if ((s_buf.st_mode & S_IFMT) == S_IFDIR)
    return 0;

  /* fail if we want to read file and it is not a regular file, OR 
   * if we cannot get the kind of access we want
   */
  else if ( (!dowrt && !(s_buf.st_mode & S_IFREG) )
         || access (file, dowrt?W_OK:R_OK) == -1)
    /* not a normal file to be read or it is not write|read-able */
    return -1;

  /* legitimate file that we can dowrt?write:read */
  return 1;
}

/* do_write|read-able_search() -- scan a directory and report only those files 
 * that are write|read-able.  Here, we let the shell expand the (possible)
 * wildcards and return a directory listing by using popen().
 * A *real* application should -not- do this; it should use the
 * system's directory routines: opendir(), readdir() and closedir().
 */
void
do_able_search(Widget w, XtPointer search_data, int dowrt)
{
char           *mask, buf[BUFSIZ], *p;
XmStringTable  names;
#define INIT_NNAMES 64
Cardinal       nNames;
int            i;
FILE           *pp, *popen();
XmFileSelectionBoxCallbackStruct *cbs = 
  (XmFileSelectionBoxCallbackStruct *) search_data;

  if (!XmStringGetLtoR (cbs->mask, XmFONTLIST_DEFAULT_TAG, &mask))
    return; /* can't do anything */

  DPR(( stderr, "do_able_search(%s) ... ", dowrt?"writeable":"readable"));

#ifdef LesstifFileSBBugFree
  LesstifFileSBBugFree( w)   /* free last set of XmNfileListItems */
#endif

  /* sprintf (buf, "/bin/ls -a %s 2> /dev/null", mask); /**/
  sprintf (buf, "/bin/ls -ad %s 2> /dev/null", mask);
  XtFree (mask);
  /* let the shell read the directory and expand the filenames */
  if (!(pp = popen (buf, "r"))) return;
  /* read output from popen() -- this will be the list of files */
  names = (XmStringTable) (long) (nNames=i=0);
  while (fgets (buf, sizeof buf, pp)) {
    if ( i == nNames) {
      /* double # of names or initialize to INIT_NNAMES */
      names = (XmStringTable) XtRealloc( (char *)names
        , sizeof( XmString) * (nNames ? (nNames *= 2) : (nNames=INIT_NNAMES)) );
    }
    if (p = index (buf, '\n')) *p = 0;
    /* only list files that are dowrt?write:read-able and not directories */
    if ( is_able (buf, dowrt) == 1 &&
         (names[i] = XmStringCreateLocalized (buf)) )
      i++;
  }
  pclose (pp);
  if (i) {
    XtVaSetValues (w,
      XmNfileListItems,      names,
      XmNfileListItemCount,  i,
      XmNdirSpec,            names[0],
      XmNlistUpdated,        True,
      NULL);
#ifndef LesstifFileSBBugFree
    while (i > 0) XmSF(names[--i]);
#endif
  } else
    XtVaSetValues (w,
      XmNfileListItems,      NULL,
      XmNfileListItemCount,  0,
      XmNlistUpdated,        True,
      NULL);

  DPR(( stderr, "on exit:  %d items at 0x%08x\n", i, names));

#ifndef LesstifFileSBBugFree
  if ( names) XtFree( (char *) names);
#endif
}

void
do_writeable_search(Widget w, XtPointer search_data)
{ do_able_search( w, search_data, 1); }
void
do_readable_search(Widget w, XtPointer search_data)
{ do_able_search( w, search_data, 0); }


/* a new file was selected -- check to see if it's write|read-able and not
 * a directory.  If it's write|read-able, return char * to it.  If it's a
 * directory, scan it just as tho the user had typed it in the mask
 * Text field and selected "Filter", and return a (char *) 0.  Otherwise,
 * return a (char *) 0. 
 *
 * returns pointer to XtFree'able (char *) filename for a write|read-able file
 * returns 0 otherwise
 *
 */

char *
check_able(Widget w, XtPointer client_data, XtPointer call_data, int dowrt)
{
char *file;
XmFileSelectionBoxCallbackStruct *cbs = 
  (XmFileSelectionBoxCallbackStruct *) call_data;

  /* get the string typed in the text field in char * format */
  if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
    return( (char *)0);

  if (*file != '/' && strcmp( file, "-") ) {
    /* if it's not an absolute path, determine the full pathname
     * of the selection by concatenating it to the "dir" part
     */
  char *dir, *newfile;
    if (XmStringGetLtoR (cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir)) {
      newfile = XtMalloc (strlen (dir) + 1 + strlen (file) + 1);
      sprintf (newfile, "%s%s", dir, file);
      XtFree( file);
      XtFree (dir);
      file = newfile;
    }
  }

  switch (is_able (file, dowrt)) {
  case 2 :
  case 1 :
    return( file);

  /* Motif handles the directory change, don't do it here */
  /* case 0 : { /**/
    /* a directory was selected, scan it */
    /* XmString str = XmStringCreateLocalized (file); /**/
      /* XmFileSelectionDoSearch (w, str); /**/
      /* XmSF (str); /**/
      /* XtFree (file); /**/
      /* file = (char *) 0; /**/
      /* break; /**/
    /* } /**/
  case 0:
  case -1:
    XtFree( file);
    file = (char *)0;
  }
  return( file);
}

char *
check_writeable(Widget w, XtPointer client_data, XtPointer call_data)
{ return check_able( w, client_data, call_data, 1); }

char *
check_readable(Widget w, XtPointer client_data, XtPointer call_data)
{ return check_able( w, client_data, call_data, 0); }

/* when widget (starter, typically a button) brings up a popup, 
 *   desensitize starter & set up callback to resensitize starter when 
 *   popup is destroyed
 */
void
orbitgui_resetSensitivity_CB( Widget popup
                         , XtPointer client_data, XtPointer call_data)
{
Widget starter = (Widget) client_data;
  XtVaSetValues( starter, XmNsensitive, True, NULL);
  return;
}
void
orbitgui_setSensitivity( Widget starter, Widget popup)
{
  XtVaSetValues( starter, XmNsensitive, False, NULL);
  XtAddCallback(popup,XmNdestroyCallback,orbitgui_resetSensitivity_CB,starter);
  return;
}

void
orbitgui_watchCursor( Widget shell, Boolean on) {
static Cursor cursor;
XSetWindowAttributes attrs;
Display *dpy;

  if ( !shell) return;

  dpy = XtDisplay( shell);
  if (!cursor) cursor = XCreateFontCursor (dpy, XC_watch);
  attrs.cursor = on ? cursor : None;
  XChangeWindowAttributes (dpy, XtWindow (shell), CWCursor, &attrs);
  XFlush (dpy);
  return;
}

#define CHARSET XmSTRING_DEFAULT_CHARSET

#define MAXWID 80
#define MAXLINE 20
#define APPENDSTR " ...\n"                  /* append APPENDSTR to long lines */
#define HEADERFMT "PREVIEW OF FILE '%s':\n"

#define MYBUFSIZ (((MAXWID+appendLen)*MAXLINE) + appendLen + hdrLen)

/**********************************************************/
/* orbitgui_displayfile - take contents TextField widget as filename, 
 * read file, display up to MAXWID columns of up to first MAXLINE lines
 */
void
orbitgui_displayfile(Widget w, XtPointer client_data, XtPointer call_data) {
Arg	args[20];
int	n;
Widget pmw=w;
Widget textW = (Widget) client_data;
Widget button, message_box;
char *message;
char tmpBuf[1024];
char *inpLine;
int nextChar;
char *fileName;
long numLine, appendLen, hdrLen;
XmString title_string= NULL, message_string= NULL, button_string= NULL;
FILE *f;

  /* get file name from TextField */

  fileName = XmTextFieldGetString( textW);
  if ( !fileName) return;
  if ( !*fileName) return;
  if ( !(f=fopen( fileName, "r"))) return;

  appendLen = strlen( APPENDSTR)+1;    /* length of "...\n" + null terminator */
  hdrLen = strlen( HEADERFMT)+1;     /* > length of header line format string */

  if ( !(message=malloc( MYBUFSIZ+strlen( fileName) )) ) return;

  sprintf( message, "PREVIEW OF FILE '%s':\n", fileName);
  inpLine = message + strlen(message);

  numLine = 0;
  while( fgets( inpLine, MAXWID+3, f)) {  /* read up to (MAXWID+1) char+\n+\0 */
  long lineLen = strlen( inpLine);
  char *nl = inpLine+lineLen-1;             /* position of \n if there is one */

    /* possibilities:
     * (1a) read a \n at inpLine+MAXWID or earlier          - copy line as is
     * (1b) read a \n after inpLine+MAXWID                  - append APPENDSTR
     * (2a) did not read a \n, but not to EOF               - append APPENDSTR
     * (2b) did not read a \n, but read to EOF after MAXWID - append APPENDSTR
     * (2c) did not read a \n, but read to EOF by MAXWID    - copy line as is
     * ***N.B. It should be impossible to have read a null line (no \n)
     */

    if ( !( (*nl == '\n' && (lineLen-1) <= MAXWID)    /* if neither (1a) ...  */
         || (*nl != '\n' && lineLen <= MAXWID) ) ) {  /* ... nor (2c) ...     */

      strcpy( inpLine+MAXWID, APPENDSTR);             /* ... append APPENDSTR */
      lineLen = MAXWID+appendLen-1;                   /* ... & adjust lineLen */

      while ( *nl != '\n') {           /* read to next \n or EOF if necessary */
        if ( !fgets( tmpBuf, 1024, f)) break;
        nl = tmpBuf + strlen(tmpBuf) - 1;
      }
    }
    inpLine += lineLen;                      /* move pointer to end of buffer */

    if ( (++numLine) == MAXLINE) break;
  }

  /* add APPENDSTR as last line if there are more lines */

  if ( fgets( tmpBuf, 1024, f)) {
    strcpy( inpLine, APPENDSTR);
  }

  fclose( f);

  for ( ; pmw && !XtIsWMShell(pmw); pmw = XtParent(pmw)) ;

  message_string= XmStringCreateLtoR(message, CHARSET);
  button_string= XmStringCreateLtoR("Close", CHARSET);
  title_string= XmStringCreateLtoR( fileName, CHARSET);
  XtFree( fileName);

  n= 0;
  XtSetArg(args[n], XmNdialogTitle, title_string), n++;
  XtSetArg(args[n], XmNokLabelString, button_string), n++;
  XtSetArg(args[n], XmNmessageString, message_string), n++;
  message_box= XmCreateMessageDialog( pmw, "filebox", args, n);

  button= XmMessageBoxGetChild(message_box, XmDIALOG_CANCEL_BUTTON);
  XtUnmanageChild(button);
  button= XmMessageBoxGetChild(message_box, XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);

  XmSF(title_string);
  XmSF(message_string);
  XmSF(button_string);
  free( message);

  XtManageChild( message_box);
  return;
}
