/* lesstifbug.h
 ***********************************************************************
 * There is a bug in older versions of Lesstif where the program will 
 * crash with a Segmentation Fault when the user does a manual Filter 
 * on File Selection boxes.  The macro in this header file takes care 
 * of the problem by freeing the previous set of XmNfileListItems. 
 * DO NOT FREE the XmStringTable entries after the call to XtVaSetValues
 * if this macro is used. 
 * DO FREE them if this macro is not used.
 *
 * Lesstif file selection box routines do not make a copy of XmNfileListItems.
 * free them here for use by custom XmNfileSearchProc & destroy routines.  
 * is the same true for the dirListItems?
 */

#ifndef _LESSTIFBUG_H_
#define _LESSTIFBUG_H_

#ifdef LesstifVersion

/*
#include <stdio.h>
#include <string.h>
/**/

#define LesstifFileSBBugFree(W) \
{ \
XmStringTable ltnames; \
int lti; \
  /* fprintf( stderr, "LesstifFileSBBugFree ... "); fflush(stderr); /**/ \
  XtVaGetValues( W \
               , XmNfileListItems,      &ltnames \
               , XmNfileListItemCount,  &lti \
               , NULL); \
  /* fprintf( stderr, "XtVaGetValues (%08xx, %d):  ", ltnames, lti); /**/ \
  /* fflush(stderr); /**/ \
  \
  if ( lti>0 && ltnames) { \
    \
    if ( lti == 1) {  /* don't free single name that is " [ .. ] " */ \
    char *item; \
      item = (char *) NULL; \
      if ( XmStringGetLtoR( ltnames[0], XmFONTLIST_DEFAULT_TAG, &item)) { \
        /* fprintf( stderr, "'%s' ", item); fflush( stderr); /**/ \
        if ( !strcmp( item, " [ .. ] ")) lti = -1; \
      } else lti = -1; \
      if ( item) XtFree(item); \
    } \
    \
    while ( lti>0) { \
      /* fprintf( stderr, "(%08xx,%d), ", ltnames[lti-1], lti-1); fflush(stderr); /**/ \
      XmSF( ltnames[--lti]); \
    } \
    /* fprintf( stderr, ". "); fflush(stderr); /**/ \
    if ( lti == 0) XtFree( (char *) ltnames); \
  } \
  /* fprintf( stderr, " done\n"); fflush(stderr); /**/ \
}

#endif /* #ifdef LesstifVersion */

#endif /* _LESSTIFBUG_H_ */
