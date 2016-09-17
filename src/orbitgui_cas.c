/* orbitgui_cas.c - gui for CAS & fragment controls */

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

#include "orbitgui_fieldmenu.h"
#include "orbit_cas.h"
#include "orbit_util.h"
#include "local.h"
#include "debug.h"

#include "orbitgui_fileseldialog.h"

long *orbit_genOlapLimits( ORBIT *, long, long *, long *);

#define CS XmFONTLIST_DEFAULT_TAG
#define XmSF(A) if ( A) { XmStringFree(A); A=(XmString) 0; }

/* MTL is macro for maximum text length + 1 for null terminator
 * - for saved text (fld_txt0) & other things
 */
#define UTCIDLEN (1+((UTCLEN>IDLEN)?UTCLEN:IDLEN))
#define MDL6 (6*FLD_MAXTXTLENPERDBL) /* max text length of 6 DBL */
#define MLL8 (8*FLD_MAXTXTLENPERLNG) /* max text length of 8 longs */
#define MTDL6 (1+( (UTCLEN>IDLEN) ? ((UTCLEN>MDL6) ? UTCLEN : MDL6) \
                                  : ((IDLEN>MDL6) ? IDLEN : MDL6) \
              ))
#define MTLL8 (1+( (UTCLEN>IDLEN) ? ((UTCLEN>MLL8) ? UTCLEN : MLL8) \
                                  : ((IDLEN>MLL8) ? IDLEN : MLL8) \
              ))

#define MDL3 (3*FLD_MAXTXTLENPERDBL) /* max text length of 3 doubles */
#define MTDL3 (1+( (UTCLEN>IDLEN) ? ((UTCLEN>MDL3) ? UTCLEN : MDL3) \
                                  : ((IDLEN>MDL3) ? IDLEN : MDL3) \
              ))

#define MTL(CASTYP) \
 ((CASTYP == CASTYPE_MSISEQDEF) ? MTLL8 : \
 (((CASTYP == CASTYPE_SHOOT) || (CASTYP == CASTYPE_CASOPNAV)) ? MTDL6 : \
 ((CASTYP == CASTYPE_REQ) ? UTCIDLEN : \
 ((CASTYP != CASTYPE_CAS && CASTYP != CASTYPE_CASDEF) ? MTDL3 : \
 (1+(CASTOP_MAXARGSLEN>UTCLEN) ? CASTOP_MAXARGSLEN : UTCLEN)))))

/* Orbitgui CAS Widget Stuff structure */
typedef struct OCWSstr {
  CAS *_cas;
  Widget _topshell;
  Widget _buttonForm;
  Widget _listw;
  void *_cur_item;
  void *_key;
  struct OCWSstr *next;
} OCWS;
#define topshell CASWStuff->_topshell
#define buttonForm CASWStuff->_buttonForm
#define listw CASWStuff->_listw
#define cur_item CASWStuff->_cur_item
#define key CASWStuff->_key

/* add button callback client_data after all CASTYPE/OPNAV types */

enum { CLOSECASLIST_BUTTON = CASTYPE_COUNT+OPNAVTYPE_COUNT
     , MODIFYCAS_BUTTON
     , REMOVECAS_BUTTON
     , RUNREPEATCAS_BUTTON
     , RUNALLCAS_BUTTON
     , LOADCAS_BUTTON
     , LOADSASF_BUTTON
     , SAVECAS_BUTTON
     , SAVESASFONEREQ_BUTTON
     , SAVESASFALLREQ_BUTTON
     , SAVEALLREQ2SASF_BUTTON
     , LOADALLREQ_BUTTON
     , LOADNRUNALLREQ_BUTTON
     , MSIFILL_BUTTON
     , NISFILL_BUTTON
     , AUTONAMEREQ_BUTTON
     , AUTOSAVEREQ_BUTTON
     , ORBITGUI_CAS_LAST
     };

#define WHOAMIFILL(FILLitem) \
WFILL( FILLitem, "MSIFILL", "NISFILL", "WSNBATGH-WHOAMI?")

#define WFILL( FILLitem, MSIFILLitem, NISFILLitem, NULLFILLitem) \
((FILLitem==MSIFILL_BUTTON) ? MSIFILLitem : \
((FILLitem==NISFILL_BUTTON) ? NISFILLitem : \
  NULLFILLitem))


/************************************************************************/
/* HERE'S THE BEEF - STATIC POINTER TO START OF A LINKED LIST OF        */
/*                   CAS Widget Stuff STRUCTURES                        */
static OCWS *firstCASWStuff;
/************************************************************************/

#define FINDOCWS(TOPSHELL) \
  for ( CASWStuff = firstCASWStuff; CASWStuff; CASWStuff = CASWStuff->next) { \
    if ( topshell == TOPSHELL) break; \
  }

void orbitgui_setSensitivity( Widget, Widget);
void orbitgui_create_CAS_fldMenu( CAS *, int, OCWS *, Widget);

/**********************************************************************/
/* simple-minded strdup & free
 */

enum { MYSNF_STRDUP=0L, MYSNF_FREE };

#define MYSTRDUP(A) myStrdupNFree( A, MYSNF_STRDUP)
#define MYFREE(A) myStrdupNFree( A, MYSNF_FREE)
/**/
/*
#define MYSTRDUP strdup
#define MYFREE free
/**/

char *
myStrdupNFree( char *c, long option) {
#define NUMROWS 40
#define LENROW 100
static char memory[NUMROWS][LENROW];
static char *allocFlags[NUMROWS];
long iRowStart, iRowEnd, i;
long nRows;
char *cPtr;

  cPtr = (char *) NULL;
  if ( !c) {
    fprintf( stderr, "***MY%s():  null pointer\n"
           , (option==MYSNF_FREE) ? "FREE" : 
             (option==MYSNF_STRDUP) ? "STRDUP" : "WSNBATGH-code-MYSNF-0"
           );
    fflush( stderr);
    return c;
  }

  switch( option) {

  case MYSNF_FREE:                       /* look for c in memory, free it ... */
    if ( c>=memory[0] && c<memory[NUMROWS]) {
      for ( i=0; i<NUMROWS; ++i) {
        if ( c == allocFlags[i]) {
          if ( !cPtr) {
            cPtr = c;
            fprintf( stderr, "MYFREE:  freeing row(s)");
          }
          fprintf( stderr, " %ld", i); fflush( stderr);
          allocFlags[i] = (char *) NULL;       /* ... by clearing allocFlag's */
        } else if ( cPtr) break;
      }
    }

    if ( !cPtr) {                    /* if not found in memory, try real free */
      fprintf( stderr
             , "MYSTRFREE:  pointer (0x%08lx,'%s') not in memory, trying free() ..."
             , (long) c, c);
      fflush( stderr);

      free( c);
    }
    break; /* case MYSNF_FREE */

  case MYSNF_STRDUP:
    nRows = (strlen(c)+LENROW) / LENROW;        /* len + terminator + roundup */

    /* look for nRows contiguous available rows */

    iRowStart = iRowEnd = 0;

    while ( iRowEnd<NUMROWS && (iRowEnd-iRowStart)<nRows) {
      if ( allocFlags[iRowEnd++]) iRowStart = iRowEnd;
    }

    if ( iRowStart < iRowEnd) {                                    /* success */
      cPtr=memory[iRowStart];
      for ( i=iRowStart; i<iRowEnd; ++i) allocFlags[i] = cPtr;
      strncpy( cPtr, c, (nRows?nRows:1)*LENROW);
      fprintf( stderr
             , "MYSTRDUP:  successfully reserved rows %ld-%ld at 0x%08lx '%s'"
             , iRowStart, iRowEnd-1, (long) cPtr, cPtr
             );
    } else {                                      /* failure, try real strdup */
      fprintf( stderr, "MYSTRDUP:  failed to reserve %ld rows; strdup()=0x%08lx"
                     , nRows, (long) (cPtr = strdup( c))
                     );
      fprintf( stderr, " '%s'", c);
    }
    break; /* case MYSNF_STRDUP */
  } /* switch option */
  fprintf( stderr, "\n"); fflush( stderr);
  return cPtr;

} /* myStrdupNFree( char *c, long option) { */

/**********************************************************************/
/* find Parent that is Window Manager Shell */

Widget 
orbitgui_findWMS( Widget w) {
  while (w && !XtIsWMShell (w)) w = XtParent (w);
  return(w);
}

/************************************************************************/
/* prepend prefix (or prefices) to names that are children of a top CAS */

#define CASCHILDPFX " \\"
#define CASCHILDPFXPFX "  "
#define PFXLEN 2
#define PFXPFXLEN 2

#define MAXPFXLEN (PFXLEN+PFXPFXLEN)

#define ISCHILD(STR) ( !strncmp( STR, CASCHILDPFX, PFXLEN) )
#define ISCHILD2(STR) \
  ( !strncmp( STR, CASCHILDPFXPFX, PFXPFXLEN) && ISCHILD( STR+PFXPFXLEN))

#define BUILDCASLISTNAME( PARENTCAS, STR, OFFSET, NAME) \
  OFFSET = 0; \
  if ( PARENTCAS) { \
    if ( PARENTCAS->top) { \
      strcpy( (STR)+OFFSET, CASCHILDPFXPFX); \
      OFFSET += strlen( CASCHILDPFXPFX); \
    } \
    strcpy( (STR)+OFFSET, CASCHILDPFX); \
    OFFSET += strlen( CASCHILDPFX); \
  } \
  strcpy( (STR)+OFFSET, NAME)

/******************************************************/
/* return CAS for a given position (1-based, not 0 based) in the list widget */

CAS *
orbitgui_CAS_getCASNFromListW( OCWS *CASWStuff, int nPos) {
XmString *list_items;
char *selected_name;
int itemCount;
CAS *foundCAS = (CAS *) 0;

  XtVaGetValues( listw
               , XmNitems, &list_items
               , XmNitemCount, &itemCount
               , NULL);

  if ( !nPos) nPos = itemCount;                /* position 0 => last position */

  if ( nPos < 1 || nPos > itemCount) return (CAS *) NULL;

  XmStringGetLtoR( list_items[nPos-1], CS, &selected_name);
  foundCAS = orbit_CAS_getCASByName( CASWStuff->_cas
                , selected_name +
                  (ISCHILD2(selected_name) ? MAXPFXLEN :
                  (ISCHILD(selected_name) ? PFXLEN : 0)) );
  XtFree( selected_name);
  return foundCAS;
}

/****************************************/
/* qsort routine to sort ints in increasing order */

int
qsort_int( const void *vp0, const void *vp1) {
int *ip0 = (int *) vp0;
int *ip1 = (int *) vp1;
  if ( *ip0 < *ip1) return -1;
  if ( *ip0 > *ip1) return +1;
  return 0;
}

/****************************************/
/* find which CASes (if any) are selected
 * - return malloced list of (CAS **) with last (CAS *) set to null
 * - if malloc fails, return (CAS **) NULL
 */

CAS **
orbitgui_CAS_getSelectedCASes( OCWS *CASWStuff) {
int *selected_posns = (int *) NULL;
int selected_count = 0;
int i;
CAS **foundCASes = (CAS **) NULL;

  XmListGetSelectedPos( listw, &selected_posns, &selected_count);

  if ( (selected_count>-1) && selected_posns) {

    foundCASes = (CAS **) malloc( (selected_count + 1) * sizeof( CAS *));

    if ( foundCASes) {

      /* sort positions if more than one position selected and 
       * positions do not monotonically increase
       */
      for ( i=1; i<selected_count; ++i) {
        if ( selected_posns[i-1] > selected_posns[i]) {
          qsort( (void *) selected_posns
               , (size_t) selected_count, (size_t) sizeof( int)
               , qsort_int);
          break;
        }
      }

      /* find CAS for each selected position */

      for ( i=0; i<selected_count; ++i) {
        foundCASes[i] = 
          orbitgui_CAS_getCASNFromListW( CASWStuff, selected_posns[i]);
      }
      foundCASes[selected_count] = (CAS *) NULL;   /* NULL at end of CAS list */
    } /* if foundCASes */

  }

  if ( selected_posns) XtFree( (void *) selected_posns);
  return foundCASes;
}

/*************************************/
/* find if exactly 1 CAS is selected */

CAS *
orbitgui_CAS_getSelectedCAS( OCWS *CASWStuff) {
int *selected_posns = (int *) 0;
int selected_count;
CAS *foundCAS = (CAS *) 0;
CAS **foundCASes = orbitgui_CAS_getSelectedCASes( CASWStuff);


  if ( foundCASes) {                           /* returned malloced (CAS **)? */
    if ( foundCASes[0])                     /* first selected CAS is non-null */
      if ( !foundCASes[1])     /* and second is null (indicating end of list) */
        foundCAS = foundCASes[0];  /* then return first (& only) selected CAS */
    free( (void *) foundCASes);                                  /* cleanup */
  }

  return foundCAS;
}

/***********************************************************
 * find position of item with given name in list widget
 * - return -1 if name is not in list widget
 */
int 
orbitgui_CAS_findPosOfNameInListW( char *name, Widget lw) {
XmString xmstr = (XmString) 0;
int pos;
  xmstr = XmStringCreateLtoR( name, CS);
  pos = XmListItemExists(lw,xmstr) ? XmListItemPos(lw,xmstr) : -1;
  XmSF( xmstr); 
  xmstr = (XmString) 0;
  return pos;
}

/**********************************************************************/
/* remove item name from listw that matches cas->_name
 * - add prefices for child cas
 */
int 
orbitgui_CAS_removeNameFromListW( char *name, Widget lw, CAS *casCAS) {
char lclstr[IDLEN+MAXPFXLEN];
int modpos;
int offset;              /* offset of name into lclstr */

  BUILDCASLISTNAME( casCAS, lclstr, offset, name);

  modpos = orbitgui_CAS_findPosOfNameInListW( lclstr, lw);
  if ( modpos > 0) XmListDeletePos( lw, modpos);

  return modpos;
}

/**********************************************************************/
/* remove cas from listw */
int 
orbitgui_CAS_removeCasFromListW( CAS *cas) {
OCWS *CASWStuff = (OCWS *) cas->_miscPtr;
Widget lw;
int modpos;

  if ( !CASWStuff) return -1;

  modpos = orbitgui_CAS_removeNameFromListW( cas->_name, listw, cas->top);
  return modpos;
}

/************************************************************************/
/* add (char *) cas->_name to list widget 
 * - allow no duplicate names
 * - strip any occurences of prefixes
 * - prepend prefix for children of a casCAS
 */
void 
orbitgui_CAS_addToListW( CAS *cas, int specPosn) {
char *name = cas->_name;
OCWS *CASWStuff = (OCWS *) cas->_miscPtr;
Widget lw = listw;      /* listw is macro into CASWStuff structure */
int ipos;
CAS *casCAS = cas->top;
CAS *foundCAS;

XmString xmstr = (XmString) 0;
/* XmString prevXmstr = (XmString) 0; /**/
char lclstr[IDLEN+MAXPFXLEN];
int modpos;              /* position in listw at which to add item */
int offset;              /* offset of name into lclstr */
int pfxlen = strlen( CASCHILDPFX);
int ppfxlen = strlen( CASCHILDPFXPFX);
int pfxdif, ppfxdif;

  /* strip CASCHILDPFXPFX or CASCHILDPFX from beginning of name */

  while ( !(pfxdif = strncmp(CASCHILDPFXPFX,name,pfxlen))
       || !(ppfxdif = strncmp(CASCHILDPFXPFX,name,ppfxlen)) ) {
  char *dest = name;
  char *src = name + (pfxdif ? ppfxlen : pfxlen);
    while ( *src) *(dest++) = *(src++);
    *dest = '\0';
  }

/* Macro to search for duplicate name from beginning of linked list
 * - first search:  look from beginning of list
 * - if first search found current cas, 
 *     do second search:  search after current cas 
 * - foundCAS will be NULL if there are no other CAS's with the same name
 */
#define FINDMATCHINGNAME \
  foundCAS = orbit_CAS_getCASByName( CASWStuff->_cas, name); \
  if ( foundCAS == cas) foundCAS = orbit_CAS_getCASByName( cas->next, name)

  /* find if CAS with this name is already in CAS list, if so, modify name */

  FINDMATCHINGNAME;
  if ( foundCAS) {
    modpos = strlen( name);                           /* append character */
    modpos = (modpos<(IDLEN-2)) ? modpos : (IDLEN-2); /* but not past end */
    strcpy( name+modpos, " ");                            /* append space */
    while ( foundCAS && name[modpos] < 126) {
      name[modpos]++;                                 /* modify appendage */
      while ( strchr( "\\\"`'():,;", name[modpos]) /* exclude \ " ` ' ( ) , ; */
           && name[modpos] < 126) name[modpos]++;
      FINDMATCHINGNAME;
    }
  }

  /* build item XmString for listw */

  BUILDCASLISTNAME( casCAS, lclstr, offset, name);
  xmstr = XmStringCreateLtoR( lclstr, CS);

  /* if a position is specified (specPosn != -1), set position to be that,
   * else if casCAS does not exist, set position to be end of list, ...
   */
  if ( specPosn != -1) ipos = specPosn;
  else if ( !casCAS) ipos = 0;

  /* ... else, casCAS exists and no position specified,
   * add cas after either casCAS or previous sibling 
   */
  else {
  CAS *prevSibCAS = casCAS;  /* initial guess:  add after TOP CAS */
  char prevStr[IDLEN+3];
  int i, maxi;
  CAS **casPtr;
  SUBFRAGS *lclSubFrags;

    /* find previous sibling, if any */

    switch( casCAS->_type) {
    case CASTYPE_CAS:

      for ( i=(cas->_type-1); i>-1; --i) {  /* start at prev sibling, go back */
        if ( casCAS->casCasList[i]) {       /* until sibling found */
          prevSibCAS = casCAS->casCasList[i];
          break;
        }
      }
      break;

    case CASTYPE_CASDEF:

      maxi = (cas->_type == CASTYPE_MSISEQDEF) ? cas->msiSeqDefTopIndex :
             (cas->_type == CASTYPE_NISSEQDEF) ? cas->nisTopIndex : 0;
      for ( i=(maxi-1); i>-1; --i) {
        if ( casCAS->casDefList[i]) {
          prevSibCAS = casCAS->casDefList[i];
          break;
        }
      }
      break;

    case CASTYPE_CASOPNAV:

      FOROPNAVSUBFRAGS(casCAS,lclSubFrags,casPtr) 
          if ( *casPtr == cas) break;
          if ( *casPtr) prevSibCAS = *casPtr;
        }
        if ( *casPtr == cas) break;
      }
      break;

    case CASTYPE_REQ:

      while ( prevSibCAS->topNext != cas) prevSibCAS = prevSibCAS->topNext;

      /* look for last child of prev sibling */

      switch( prevSibCAS->_type) {

      case CASTYPE_REQ:                            /* if first topNext is cas */
        break;

      case CASTYPE_CASDEF:
        for ( i=(MAXNUMSEQDEF-1); i>-1; --i) {
          if ( prevSibCAS->casDefList[i]) {
            prevSibCAS = prevSibCAS->casDefList[i];
            break;
          }
        }
        break;

      case CASTYPE_CASOPNAV:
        {
        CAS *holdCAS = prevSibCAS;
        FOROPNAVSUBFRAGS(holdCAS,lclSubFrags,casPtr) 
          if ( *casPtr) prevSibCAS = *casPtr;
        }}
        }
        break;

      case CASTYPE_CAS:
        for ( i=CASTYPE_COUNT-1; i>-1; --i) {
          if ( prevSibCAS->casCasList[i]) {
            prevSibCAS = prevSibCAS->casCasList[i];
            break;
          }
        }
        break;

      default:  /* WE SHOULD NEVER GET HERE unless prevSibCAS is a REQ */
        fprintf(stderr,"Got'ere!\n");fflush(stderr);
        break;
      }
      break;  /* case REQ of casCAS->_type */

    default:
      break;

    } /* switch casCAS->_type */

    /* build XmString item for listw from name of CAS 
     * that will precede the new one
     */
    BUILDCASLISTNAME( prevSibCAS->top, lclstr, offset, prevSibCAS->_name);

    /* if specified position is not -1, find prev CAS in list, 
     * set position to be after it
     */

    if( (ipos=orbitgui_CAS_findPosOfNameInListW( lclstr, lw)) < 0) ipos = 0;
    else ipos++;

    /* prevXmstr = XmStringCreateLtoR( lclstr, CS);
    /* ipos = (specPosn != -1) ? specPosn 
    /*                        : ( (XmListItemExists(lw,prevXmstr)) 
    /*                            ? (XmListItemPos(lw,prevXmstr) + 1)
    /*                            : 0 );
    /* XmSF( prevXmstr); /**/

  } /* if casCAS ... else */
  XmListAddItem( lw, xmstr, ipos);    /* add to list (finally) */

  XmSF( xmstr)
  return;
}

/****************************************************************************/
/* add/update miscPtr linked list(s), list widget
 * Input:  CAS *cas
 *         if ( cas->_malloced == CAS_MALLOC_NEW), add to miscPtr linked list
 *         *  assume ->prev = ->next = 0
 *         *  assume ->topNext set correctly if cas is CAS or CASDEF
 */

void
orbitgui_CAS_addToMiscPtr( CAS *cas) {
OCWS *CASWStuff = (OCWS *) cas->_miscPtr;           /* retrieve OCWS from cas */
int ipos;                                 /* where in listw to insert new cas */
char lclstr[IDLEN+MAXPFXLEN];
int offset;

#define T0BK tmpcas = (CAS *) 0; break/* add to end of list, list in dflt posn*/

#define REMOVEcasFROMLINKEDLIST \
  if ( cas->prev) cas->prev->next = cas->next; \
  else if ( CASWStuff->_cas == cas) CASWStuff->_cas = cas->next; \
  if ( cas->next) cas->next->prev = cas->prev

  /* add new CAS to OCWS */

  if ( cas->_malloced == CAS_MALLOC_NEW) {

    /* if pointer to head of linked list of CAS's is null ... */

    if ( !CASWStuff->_cas) {
      cas->prev = cas->next = (CAS *) 0;
      ipos = -1;

    } else {                        /* find CAS before which this cas will go */

    CAS *tmpcas;
      /* ... (1)  special case for new REQ/CAS/CASDEF/CASOPNAV:
       *          look for a selected cas; if found & it is of same type as new,
       *          add this one before it
       *          if a REQ/CAS/CASDEF is not selected, set tmpcas null ...
       */
      tmpcas = (CAS *) 0;    /* assume no matching selected cas will be found */
                                            /* before which to place this cas */

      /* code added August, 1999 to sort CASes w/null ->top by increasing time
       * - CASes w/null ->top will be at beginning of list
       * - bypasses a lot of code in the next section 
       */

      if ( !cas->top) {

        /* criteria for CAS after current cas
         * - must have null ->top or be first CAS w/non-null ->top
         * - must be first CAS w/null ->top w/->_et at or after current cas
         *
         * loop through list ...
         */
        for ( tmpcas=CASWStuff->_cas; tmpcas; tmpcas=tmpcas->next) {

          if ( tmpcas->top) {       /* stop at first CAS w/non-null ->top ... */
            break;
          }

          if ( tmpcas->_et >= cas->_et) {   /* OR at first w/->_et >= current */
            break;
          }
        }

        if ( tmpcas) {
          if ( !tmpcas->top) {  /* put it in listw before tmpcas w/null ->top */
            BUILDCASLISTNAME( tmpcas->top, lclstr, offset, tmpcas->_name);
            ipos = orbitgui_CAS_findPosOfNameInListW( lclstr, listw);
          } else {                              /* OR place it at end of list */
            ipos = -1;
          }

          REMOVEcasFROMLINKEDLIST;  /* move it to before tmpcas in linked list*/
          cas->next = tmpcas;
          cas->prev = tmpcas->prev;

        } /* null tmpcas implies list of CASes that all have null ->top 
           * & that also all have ->_et's earlier than cas->_et; 
           * null tmpcas will cause cas to be put at end of that list
           */

      } else if isANYTOP( *cas) switch (cas->_type) {
      case CASTYPE_REQ:
        if ( !(tmpcas=orbitgui_CAS_getSelectedCAS( CASWStuff)) ) break;
        if ( !isREQ( *tmpcas)) { T0BK; }
        BUILDCASLISTNAME( tmpcas->top, lclstr, offset, tmpcas->_name);
        ipos = orbitgui_CAS_findPosOfNameInListW( lclstr, listw);
        REMOVEcasFROMLINKEDLIST;
        cas->next = tmpcas;
        cas->prev = tmpcas->prev;
        break;
      case CASTYPE_CAS:
      case CASTYPE_CASDEF:
      case CASTYPE_CASOPNAV:
        if ( !(tmpcas=orbitgui_CAS_getSelectedCAS( CASWStuff)) ) break;
        if ( !isCAS( *tmpcas)
          && !isCASDEF( *tmpcas)
          && !isCASOPNAV( *tmpcas)
           ) { T0BK; }
        if ( cas == tmpcas) { T0BK; }  /* should not happen, but just in case */
        if ( cas->top != tmpcas->top) { T0BK; }/* must have same REQ as ->top */

        /* adjust topNext linked list (LL) - insert new cas as appropriate */
        if ( cas->top) {
        CAS **prevSibCASptr = &cas->top->topNext;      /* prev sibling to cas */
        CAS **prevSibTmpCASptr = prevSibCASptr;     /* prev sibling to tmpcas */

          while ( *prevSibCASptr && *prevSibCASptr != cas) 
            prevSibCASptr = &(*prevSibCASptr)->topNext;

          while ( *prevSibTmpCASptr && *prevSibTmpCASptr != tmpcas) 
            prevSibTmpCASptr = &(*prevSibTmpCASptr)->topNext;

          if ( *prevSibCASptr && *prevSibTmpCASptr) {
            *prevSibCASptr = cas->topNext;              /* remove cas from LL */
            cas->topNext = tmpcas;           /* put it back before tmpcas and */
            *prevSibTmpCASptr = cas;              /* after prev sib to tmpcas */
          }
          T0BK;        /* let it be placed in default position per topNext LL */
        }
        /* there is no REQ above this CAS (cas->top == 0) */
        BUILDCASLISTNAME( tmpcas->top, lclstr, offset, tmpcas->_name);
        ipos = orbitgui_CAS_findPosOfNameInListW( lclstr, listw);
        REMOVEcasFROMLINKEDLIST;
        cas->next = tmpcas;
        cas->prev = tmpcas->prev;
        break;
      default:
        break;
      }

      /* ... or (2)  if no special case REQ-before-selected-REQ found 
       *             (tmpcas=null), add new CAS after last cas in linked list
       */
      if ( !tmpcas) {
        for ( tmpcas = CASWStuff->_cas; tmpcas->next; tmpcas = tmpcas->next) ;
        cas->next = (CAS *) 0;
        cas->prev = tmpcas;
        ipos = -1;
      }
    }

    /* maintain integrity of linked list */

#   define FIXLIST \
    if ( cas->prev) { cas->prev->next = cas; }            /* ... from before */\
    else { CASWStuff->_cas = cas; }         /* new CAS is first in CASWStuff */\
    \
    if ( cas->next) { cas->next->prev = cas; }             /* ... from after */

    FIXLIST

    cas->_malloced = CAS_MALLOC;           /* new CAS now exists & is in list */

    /* add to list widget in position specified by ipos (default if ipos==-1)
     * - routine ensures no duplicate names
     */
    orbitgui_CAS_addToListW( cas, ipos);

    if isCASOPNAV(*cas) {   /* call self (i.e. miscPtr) for CASOPNAV children */
    SUBFRAGS *lclSubFrags;
    CAS **casPtr;
      FOROPNAVSUBFRAGS( cas, lclSubFrags, casPtr)
        (*casPtr)->_miscPtr = cas->_miscPtr;
        orbitgui_CAS_addToMiscPtr( *casPtr);
      }}
    }

  } else { /* if CAS_MALLOCED_NEW ... */

    /* CAS is not new; check for changes in ->_name &/or 
     * in time (->_et) of cas with null ->top
     *
     * if name changed for existing CAS, remove old listw entry
     */
    if ( strcmp( cas->_name, cas->_savename)) {
      ipos = orbitgui_CAS_removeNameFromListW( cas->_savename, listw, cas->top);
      /* add to list widget to same position
       * - routine ensures no duplicate names
       */
      orbitgui_CAS_addToListW( cas, ipos);
    }

    /* if ->top is null, check if time sorting is still ok */

    if ( !cas->top) {
    int changepos = 0;         /* no adjustment required if changepos is != 0 */

      /* test for previous CAS with time after current CAS
       * - prev->top test is probably not requred
       */ 
      if ( cas->prev) if ( !cas->prev->top) if (cas->prev->_et > cas->_et) {
        changepos = 1;
      }

      /* test for next CAS with time before current CAS
       * - next->top test IS required
       */
      if ( cas->next) if ( !cas->next->top) if (cas->next->_et < cas->_et) {
        changepos = 1;
      }

      if ( changepos) {   /* adjust position in list */
      CAS *tmpcas;
      int oldpos;
        oldpos = orbitgui_CAS_removeNameFromListW( cas->_name, listw, cas->top);
        REMOVEcasFROMLINKEDLIST;
        cas->prev = cas->next = (CAS *) NULL;

        /* look for CAS before which to place current cas */
        for ( tmpcas=CASWStuff->_cas; tmpcas; tmpcas=tmpcas->next) {

          if ( tmpcas->top) {       /* stop at first CAS w/non-null ->top ... */
            break;
          }

          if ( tmpcas->_et >= cas->_et) {   /* OR at first w/->_et >= current */
            break;
          }
        }

        if ( tmpcas) {

          /* placement in listw (list widget) */

          if ( !tmpcas->top) {  /* put it in listw before tmpcas w/null ->top */
            BUILDCASLISTNAME( tmpcas->top, lclstr, offset, tmpcas->_name);
            ipos = orbitgui_CAS_findPosOfNameInListW( lclstr, listw);
          } else {                             /* OR place it at end of listw */
            ipos = 0;
          } /* if !tmpcas->top */

          /* placement in linked list */

          cas->next = tmpcas;                         /* insert before tmpcas */
          cas->prev = tmpcas->prev;


        } else {              /* add at end of list of CASes all w/null ->top */

          /* find last CAS in list */
          for ( tmpcas=CASWStuff->_cas; tmpcas->next; tmpcas=tmpcas->next) ;

          cas->next = (CAS *) NULL;
          cas->prev = tmpcas;
          ipos = 0;

        } /* if tmpcas */


        FIXLIST                                 /* maintain integrity of list */

        orbitgui_CAS_addToListW( cas, ipos);         /* add cas back to listw */

        /* move children of cas in listw (list widget)
         * - no need to move them around linked list
         * - if cas moved down, make same movement i.e. oldpos -> ipos
         * - if cas moved up, make movement with incremented oldpos & ipos
         */
        while (1) {

          /* find next CAS in listw, check if it is a child CAS of cas */

          if ( (ipos < oldpos) && ipos) { /* adjust positions if cas moved up */
            oldpos++; ipos++;
          }

          /* get CAS at oldpos; break if it is not a child or a child's child 
           * of cas
           */
          tmpcas = orbitgui_CAS_getCASNFromListW( CASWStuff, oldpos);
          if ( !tmpcas) break;
          if ( !tmpcas->top) break;
          if ( tmpcas->top != cas && tmpcas->top->top != cas) break;

          XmListDeletePos( listw, oldpos);   /* delete name from old position */
          orbitgui_CAS_addToListW( tmpcas, ipos);        /* & add back to new */

        } /* while 1 */

      } /* if changepos */

    } /* if !cas->top */

  } /* if _malloced == CAS_MALLOC_NEW ... else .. */

  return;
} /* orbitgui_CAS_addToMiscPtr( CAS *cas) { */

/****************************************************/
/* remove cas pointer from listW & from linked list */

void
orbitgui_CAS_removeCasFromList( CAS *cas) {
OCWS *CASWStuff = (OCWS *) cas->_miscPtr; /* retrieve OCWS from cas */

  orbitgui_CAS_removeCasFromListW( cas);  /* remove from listW */

  /* update prev link in next cas */
  if ( cas->next) cas->next->prev = cas->prev;

  /* update next link in prev cas if prev cas exists or update start of list */
  if ( cas->prev) cas->prev->next = cas->next;
  else if ( cas == CASWStuff->_cas) CASWStuff->_cas = cas->next;
  return;
}

/****************/
/* delete a cas */
void
orbitgui_CAS_deleteCAS( CAS *cas) {
OCWS *CASWStuff = (OCWS *)cas->_miscPtr;
void orbit_CAS_deleteAnyCas( CAS*, void (*)(CAS *));

  orbit_CAS_deleteAnyCas( cas, orbitgui_CAS_removeCasFromList);
  return;
}

/*************************************/
/* call back from any CAS field menu */

int 
orbitgui_CAS_fldMenu_ok_CB( void *vcas, FLDMENU *fldMenu) {
CAS *cas = (CAS *) vcas;
OCWS *CASWStuff = (OCWS *) cas->_miscPtr;
CAS *nextCas, *lastCas;
int numflds;
FLDMENU *fm;
char *utctxt0;
int icount, icountExp, icountFilt, i, ii, iBit, numBits, topChanged;
int minexpflt, lastvalid;
long mtl = MTL(cas->_type);
long iArg, subType;

  /* get address of UTC text after FLDMENU structs + fld_txt0's */
  for ( numflds = 1, fm=fldMenu; fm->type != FLD_END; ++fm, ++numflds) ;
  utctxt0 = ((char *) (fldMenu + numflds)) + (mtl * numflds);

  /* update scheduling point UNLESS this CAS has a parent */

  if ( !cas->top) {
    orbit_utc2et( utctxt0, &cas->_et);
  }

  orbitgui_CAS_addToMiscPtr( cas);    /* copy to (OCWS *) _miscPtr */

/* macro to find fm that matches a given member of cas-> that is a pointer */
#define FINDFMM( FMFLD, MBR, ERR, BREAKONERR) \
        FINDFM( FMFLD, &cas->MBR, ERR, BREAKONERR)

/* macro to find fm that matches a given member of cas-> that is a pointer */
#define FINDFMP( FMFLD, MBR, ERR, BREAKONERR) \
        FINDFM( FMFLD, cas->MBR, ERR, BREAKONERR)

/* macro to find fm that matches a given pointer */
#define FINDFM( FMFLD, CASMBR, ERR, BREAKONERR) \
  for ( fm=fldMenu; fm->type != FLD_END && fm->FMFLD != (CASMBR) \
      ; ++fm) ; \
  \
  if ( fm->FMFLD != (CASMBR)) { \
    fprintf( stderr \
           , "WARNING:  orbitgui_CAS_fldMenu_ok/cancel_CB:  %s-%s-%d\n" \
           , "Program error, contact programmer, code WSNBATGH" \
           , orbit_CAS_FragToName( cas) \
           , 0); \
    fflush( stderr); \
    BREAKONERR /* optional break on error */ \
  }

  /* macros to fill in data from one of multiple fields up to an actual count
   * - the last valid entry that is duplicated into subsequent array elements is
   *   determined by fm->subtype & OFFSET
   * ***N.B. WILL USE FIRST OLD VALUE IF SUBTYPE IS 0 I.E. FIELD IS NULL OR
   *         OTHERWISE CAUSES fm->subtype (from sscanf()) TO BE 0
   * ***N.B. PREREQUISITE:  fm points to the relevant field menu field
   */

# define FILLIN( OFFSET, MBR, ACTCOUNT) \
    lastvalid = (OFFSET) + fm->subtype - (((OFFSET)+fm->subtype)?1:0); \
    for ( i=fm->subtype+(OFFSET); i<ACTCOUNT && i<((OFFSET)+fm->fld_txt_maxlen)\
        ; ++i) { \
      cas->MBR[i]=cas->MBR[lastvalid]; \
    }
# define FILLINDFLT( OFFSET, MBR, ACTCOUNT, DFLTVAL) \
  FILLIN( OFFSET, MBR, ACTCOUNT) \
  while ( i<((OFFSET)+fm->fld_txt_maxlen) ) cas->MBR[i++]=(DFLTVAL)

# define FILLIN0( MBR, ACTCOUNT) FILLIN( 0, MBR, ACTCOUNT)
# define FILLIN0DFLT( MBR, ACTCOUNT, DFLTVAL) \
  FILLINDFLT( 0, MBR, ACTCOUNT, DFLTVAL)

# define FILLIN0MAX( MBR) FILLIN0( MBR, fm->fld_txt_maxlen)

# define FILLIN0MAXDFLT( MBR, DFLTVAL) \
  FILLIN0DFLT( MBR, fm->fld_txt_maxlen,DFLTVAL)

  /*************************************/
  /* special handling for each CASTYPE */

  switch ( cas->_type) {

#define CASES_NOPARAMS               /* these fragments have no parameters */ \
       CASTYPE_MSIPARK: \
  case CASTYPE_NISPARK: \
  case CASTYPE_XGRSPARK: /**/ \
  case CASTYPE_NISCAL: \
  case CASTYPE_NISBUFFLUSH: \
  case CASTYPE_MSIRELATT: \
  case CASTYPE_NISRELATT: \
  case CASTYPE_XGRSRELATT: \
  case CASTYPE_NAVRELATT

  case CASES_NOPARAMS:
    fprintf(stderr,"about to free 0x%08lx", (long) fldMenu->lbl_txt);
    fflush(stderr);
    free( fldMenu->lbl_txt);          /* free first menu label strduped below */
    fprintf(stderr,"\n"); fflush(stderr);
    break;

  case CASTYPE_AUTOEXPOSE:      /* these fragments require no post-processing */
  case CASTYPE_LOADFILT:
    break;

  case CASTYPE_XGRSCONFIG:                           /* convert bits to codes */
    cas->xgrsConfPosn =
      orbit_getFrmByBit( cas->xgrsConfBits & XGRSCONFBITall_Posn
                       , XGRSCONFI000Posn, xgrsConfPosnArray);
    cas->xgrsConfDir =
      orbit_getFrmByBit( cas->xgrsConfBits & XGRSCONFBITall_Dir
                       , XGRSCONFI000Dir, xgrsConfDirArray);
    break;

  case CASTYPE_MSICONFIG:                            /* convert bits to codes */
    cas->msiConfFullIm =
      orbit_getFrmByBit( cas->msiConfBits & MSICONFBITall_FullIm
                       , MSICONFI000FullIm, msiConfFullImArray);
    cas->msiConfSumIm =
      orbit_getFrmByBit( cas->msiConfBits & MSICONFBITall_SumIm
                       , MSICONFI000SumIm, msiConfSumImArray);
    break;

  case CASTYPE_NISCONFIG:                            /* convert bits to codes */
    cas->nisConfSlitDriveSel =
      orbit_getFrmByBit( cas->nisConfBits & NISCONFBITall_SlitDriveSel
                       , NISCONFI000SlitDriveSel, nisConfSlitDriveSelArray);
    cas->nisConfShutDriveSel =
      orbit_getFrmByBit( cas->nisConfBits & NISCONFBITall_ShutDriveSel
                       , NISCONFI000ShutDriveSel, nisConfShutDriveSelArray);
    cas->nisConfVoltSet =
      orbit_getFrmByBit( cas->nisConfBits & NISCONFBITall_VoltSet
                       , NISCONFI000VoltSet, nisConfVoltSetArray);
    cas->nisConfScanDriveSel =
      orbit_getFrmByBit( cas->nisConfBits & NISCONFBITall_ScanDriveSel
                       , NISCONFI000ScanDriveSel, nisConfScanDriveSelArray);
    break;

  /* SHOOT:  ensure _delayStart is 0.0; extend missing values out to end */

  case CASTYPE_SHOOT:
    cas->_delayStart = 0.0;
    icount = cas->msiShootCount;
    if ( icount > 12) icount = 12;
    if ( icount < 1) icount = 1;

#define FILLIN2( MBR, DFLTVAL) \
        FILLINDFLT( 0,                     MBR, icount, DFLTVAL); fm++; \
        FILLINDFLT( fm[-1].fld_txt_maxlen, MBR, icount, DFLTVAL); fm++

    FINDFMP( fld_lng, msiShootSeq, 0, break;)
    FILLIN2( msiShootSeq, 0);
    FILLIN2( msiShootDel, 0);
    FILLIN2( msiShootImageTypeDbl, 1.3);

    break;

  /* MSISEQDEF:  convert bits to dpcm, mode, compression table & algorithm
   *             check range on numImages (min 0, max 8 or  # of _msecExp/_filt)
   *             check ranges on _msecExp & _filt, fill in unused values
   */
  case CASTYPE_MSISEQDEF:
    cas->msiSeqDefCmpTbl =
      orbit_getFrmByBit( cas->msiSeqDefBits & MSISEQDEFBITallCmpTbl
                       , MSISEQDEFI000CmpTbl, msiSeqDefCmpTblArray);
    cas->msiSeqDefCmpAlg =
      orbit_getFrmByBit( cas->msiSeqDefBits & MSISEQDEFBITallCmpAlg
                       , MSISEQDEFI000CmpAlg, msiSeqDefCmpAlgArray);
    cas->msiSeqDefDpcm =
      orbit_getFrmByBit( cas->msiSeqDefBits & MSISEQDEFBITallDpcm
                       , MSISEQDEFI000Dpcm, msiSeqDefDpcmArray);
    cas->msiSeqDefMode =
      orbit_getFrmByBit( cas->msiSeqDefBits & MSISEQDEFBITallMode
                       , MSISEQDEFI000Mode, msiSeqDefModeArray);

    fm = fldMenu + numflds - 3;
    minexpflt = (fm[0].subtype<fm[1].subtype) ? fm[0].subtype : fm[1].subtype;
    fm[0].subtype = fm[1].subtype = minexpflt;

    /* range-check number of images
     * fill in any empty slots from lowest of icountExp & icountFilt 
     * and zero out higher slots 
     */

#ifdef NUMIMG
#undef NUMIMG
#endif
#define NUMIMG cas->msiSeqDefNumImages

    NUMIMG = (NUMIMG<0) ? 0 : ((NUMIMG>8) ? 8 : NUMIMG);

    FILLIN0DFLT( msiSeqDefMsecExp, NUMIMG, 0);
    fm++;
    FILLIN0DFLT( msiSeqDefFilt, NUMIMG, 0);

    for ( i=NUMIMG; i<8; ++i) 
      cas->msiSeqDefMsecExp[i] = cas->msiSeqDefFilt[i] = 0;

#undef NUMIMG

    break;

  /* DS56:  convert coord sys bit to frame type */
  case CASTYPE_DS56:
    cas->ds56ReUse =
      orbit_getFrmByBit( cas->ds56Bits & DS56BITallREUSE
                       , DS56I000reUSE, ds56ReUseArray);
    cas->ds56FrmType =
      orbit_getFrmByBit( cas->ds56Bits & DS56BITallSys
                       , DS56I000Sys, ds56SysArray);
    break;

  /* DS40:  convert coord sys bits to frame types */
  case CASTYPE_DS40XGRS:
  case CASTYPE_DS40:
  case CASTYPE_DS40FULL:
    cas->ds40AimptFrmType =
      orbit_getFrmByBit( cas->ds40Bits & DS40BITallSys
                       , DS40I000Sys, ds40SysArray);

    if isDS40XGRS( *cas) break;                  /* done for xgrs_point_quick */

    cas->ds40AimptSelect =
      orbit_getFrmByBit( cas->ds40Bits & DS40BITallSel
                       , DS40I000Sel, ds40SelArray);

    if isDS40FULL( *cas) {       /* roll algorithm read for FULL DS40 only */
      cas->ds40RollFrmType =
        orbit_getFrmByBit( cas->ds40Bits & DS40BITallSysRoll
                         , DS40I000SysRoll, ds40SysRollArray);
    }

    break;

  /* MSITR:  fill out Seq, Delta, Type
   *         ***N.B. NO, IMAGE TYPE IS NOW A DOUBLE (1.0 - 2.5)
   */
  case CASTYPE_MSITR:

    /* duplicate < 3 seq #('s), delta(s) & image type(s) as necessary */

    FINDFMP( fld_lng, msiRepSeq, 0, break;)
    FILLIN0MAX( msiRepSeq)
    fm++;
    FILLIN0MAX( msiRepDel)
    fm++;
    FILLIN0MAX( msiRepImageTypeDblTR)

    break;

  /* MSISR/DR/DSR:  convert image type bits into image type
   *                ***N.B. NO, IMAGE TYPE IS NOW A DOUBLE (1.0 - 2.5)
   * MSIDR/DSR:  fill out single Seq, Delta, Iter (DSR only) to two values
   * MSIDSR: convert from start 1 & 2 imaging into seq 2 start
   */
  case CASTYPE_MSIDSR:

    /* DSR ONLY:  if only single RepDelay value given, double it */

    FINDFMP( fld_lng, msiRepDelay, 0, ;)
    if ( fm->subtype == 1) cas->msiRepDelay[1] = 2 * cas->msiRepDelay[0];

    cas->msiRepSeq2Start = cas->msiRepDelay[1] - cas->msiRepDelay[0];

    /* DSR ONLY:  if only single iter given, duplicate first */

    FINDFMP( fld_lng, msiRepIter, 1, ;)
    FILLIN0MAX( msiRepIter)

  case CASTYPE_MSIDR:

    /* DR & DSR ONLY:  duplicate single seq # & delta */

    FINDFMP( fld_lng, msiRepSeq, 2, break;)
    FILLIN0MAX( msiRepSeq)
    fm++;
    FILLIN0MAX( msiRepDel)

  case CASTYPE_MSISR:

    break;

  /* NISSU2/NISSU1:  convert aperture, gain & step dir bits 
   * to aperture, gain & step dir (step dir only for SU2, not SU)
   */
  case CASTYPE_NISSU2:
  case CASTYPE_NISSU1:
    cas->nisSuAperture =
        orbit_getFrmByBit( cas->nisSuBits & NISSUBITallAper
                         , NISSUI000Aper, nisSuAperArray);
    cas->nisSuGain =
        orbit_getFrmByBit( cas->nisSuBits & NISSUBITallGain
                         , NISSUI000Gain, nisSuGainArray);
    if ( cas->_type == CASTYPE_NISSU2) {
      cas->nisSuStepDir =
        orbit_getFrmByBit( cas->nisSuBits & NISSUBITall_Dir
                         , NISSUI000Dir, nisSuStepDirArray);
    }
    break;

  /* NISEX/SR/DR:  convert aperture & gain bits to aperture & gain */
  case CASTYPE_NISEX:
  case CASTYPE_NISSR:
  case CASTYPE_NISDR:

    if isNISSR( *cas) {

      /* for single repeat, copy mirror position & bits 
       * from setup variables to sequence 1 variables
       */
      cas->nisRepBits[0] = cas->nisRepBitsSetup;
      cas->nisRepMirrorPosn[0] = cas->nisRepMirrorPosnSetup;

    } else if isNISDR( *cas) {

      /* for double repeat, convert setup bits to setup variables */

      cas->nisRepApertureSetup = 
        orbit_getFrmByBit( cas->nisRepBitsSetup & NISREPBITallAper
                         , NISREPI000Aper, nisRepAperArray);
      cas->nisRepGainSetup = 
        orbit_getFrmByBit( cas->nisRepBitsSetup & NISREPBITallGain
                         , NISREPI000Gain, nisRepGainArray);
    }

    numBits = isNISDR(*cas) ? 2 : 1;   /* two sets of bits for NISDR, else 1 */
    for ( iBit=0; iBit < numBits; ++iBit) {

      /* allow NOCHANGE for NISDR only */

      cas->nisRepAperture[iBit] =
        orbit_getFrmByBit( cas->nisRepBits[iBit] 
                         &((numBits==1)?NISREPBITallAper : NISREPBITallAperNoch)
                         , NISREPI000Aper
                         , (numBits==1)? nisRepAperArray : nisRepAperNochArray);
      cas->nisRepGain[iBit] =
        orbit_getFrmByBit( cas->nisRepBits[iBit] 
                         &((numBits==1)?NISREPBITallGain : NISREPBITallGainNoch)
                         , NISREPI000Gain
                         , (numBits==1)? nisRepGainArray : nisRepGainNochArray);
    } /* for iBit<numBits */

    if ( numBits == 2) {

      /* DR:  duplicate single seq #, mirror position & delta */

      FINDFMP( fld_lng, nisRepSeq, 0, break;)
      FILLIN0MAX( nisRepSeq)
      fm++;
      FILLIN0MAX( nisRepMirrorPosn)
      fm += 5;   /* skip Aperture 1 & 2, Gain 1 & 2 */
      FILLIN0MAX( nisRepDel)

    }

    break;

  /* NISSEQDEF:  no special handling required */
  case CASTYPE_NISSEQDEF:
    break;

  /* REQ CAS:  1) read possibly-modified argument strings & interpret
   *           2) see if ADD CAS or ADD CASDEF sliders are non-zero
   *              if so, add sub-CASes as appropriate
   */

  case CASTYPE_REQ:

    lastCas = cas;
    for ( nextCas = cas->topNext; nextCas; nextCas = nextCas->topNext) {
                                        /* nothing to interpret */
      lastCas = nextCas;                /* find end of list */
    }

#define ADDTOP( ADDMBR, CASTYP, CASTEMPLATE) \
    for ( i=0; i<cas->ADDMBR; ++i) { \
    FILE *f; \
      f = fopen( cas->CASTEMPLATE, "r"); \
      if ( f) { \
        fclose( f); \
        orbit_CAS_readFile_underTop( cas->CASTEMPLATE, (void *)0, cas \
                                   , orbitgui_CAS_addToMiscPtr); \
      } else { \
      CAS *newCas = orbit_CAS_new( CASTYP); \
        if ( newCas) { \
          topChanged = 1; \
          newCas->top = cas; \
          orbit_CAS_updateTop( newCas); \
          newCas->_miscPtr = cas->_miscPtr; \
          strcpy( newCas->_savename, newCas->_name); \
          orbitgui_CAS_addToMiscPtr( newCas); \
          lastCas = newCas; \
        } \
      } \
    }

    topChanged = 0;
    ADDTOP( reqAddTop, CASTYPE_CAS, reqAddCasTemplate)
    ADDTOP( reqAddCasDef, CASTYPE_CASDEF, reqAddCasDefTemplate)

    /* find each bit turned on */

    if ( cas->reqAddCasOpnavBits) {
    long bits = cas->reqAddCasOpnavBits;
    long iBitPos;
      cas->reqAddCasOpnavBits = 1;
      for ( iBitPos=0; iBitPos<OPNAVTYPE_COUNT; ++iBitPos) {
        if ( (1L<<iBitPos) & bits) {
          ADDTOP( reqAddCasOpnavBits, CASTYPE_COUNT+iBitPos
                , reqAddCasOpnavTemplate)
        }
      }
    }
    break;

  /* CAS CAS:  see if fields are either null when the corresponding bit is set 
   *           or are non-null when the bit is not set - if so, add or 
   *           delete sub-CAS as appropriate
   */

  case CASTYPE_CAS:

    topChanged = 0;

    for ( i=0; i<CASTYPE_COUNT; ++i) {
    long thisbit, bitset;

      thisbit = 1L << i;
      bitset = thisbit & cas->casBits;

      /* bit set && null pointer - make new CAS */

      if ( bitset && !cas->casArgListPtrs[i]) {

        /* create new CAS of type i; if successful, add it to top cas */

        cas->casCasList[i] = orbit_CAS_new( i);
        if ( cas->casCasList[i]) {
        CAS *newCas = cas->casCasList[i];

          /* convert new cas to argument string, place in topcas */

          newCas->top = cas;
          orbit_CAS_updateTop( newCas);
          topChanged = 1;       /* bit changed, set flag to bring up new menu */

          /* add new cas to top cas and to (OCWS *) _miscPtr
           * - name in List Widget will have '\' prepended
           */

          newCas->_miscPtr = cas->_miscPtr;
          strcpy( newCas->_savename, newCas->_name);
          orbitgui_CAS_addToMiscPtr( newCas);

        /* if failed to create new CAS, say so - we don't need to clear bit
         * because new fldMenu will take care of that
         */
        } else {
          fprintf( stderr
                 , "Problem creating new CAS of type %s for top CAS %s\n"
                 , orbit_CAS_TypeToName(i), cas->_name);
        }

      } else {

        /* bit clear && non-null pointer - delete cas */

        if ( !bitset && cas->casArgListPtrs[i]) {
          topChanged = 1;       /* bit changed, set flag to bring up new menu */
          orbitgui_CAS_deleteCAS( cas->casCasList[i]);
        } else {
          /* neither of the above & non-null pointer - read field */
          if ( cas->casArgListPtrs[i]) {
          CAS *tmpCAS =
            orbit_CAS_ArgsToCAS( cas->casArgListPtrs[i], cas->casCasList[i]);
            if ( !tmpCAS) {
              fprintf( stderr, "Problem reading %s CAS arguments\n"
                     , orbit_CAS_TypeToName( cas->casCasList[i]->_type));
            }
          }
        }
      }
    }

    /* if fields changed, bring up new field menu
     * - original field menu will be destroyed when 1 is return'ed below
     * - use listw which is child of same WMShell as button that initiated 
     *   original field menu
     * ***N.B. DISABLED - didn't seem to work
     */

/*
    if ( topChanged) {
      orbitgui_create_CAS_fldMenu( cas, cas->_type, CASWStuff, listw);
    }
/**/

    break;   /* case CASTYPE_CAS */

  /* CASDEF CAS:  1) read possibly-modified argument strings & interpret
   *              2) see if ADD MSISEQDEF or NISSEQDEF sliders are non-zero
   *                 if so, add sub-CASes as appropriate
   */

  case CASTYPE_CASDEF:

    for ( ii=0; ii<MAXNUMSEQDEF; ++ii) {
      if ( cas->casDefList[ii]) {
      CAS *tmpCAS = 
        orbit_CAS_ArgsToCAS( cas->casDefListPtrs[ii], cas->casDefList[ii]);
        if ( !tmpCAS) {
          fprintf( stderr
          , "orbitgui_CAS_fldMenu_ok_CB:  Problem reading %s CAS arguments\n"
          , orbit_CAS_TypeToName( cas->casDefList[ii]->_type));
        }
      }
    }

#define ADDSEQDEF( ADDMBR, CASTYP, TOPINDEX) \
    for ( i=0; i<cas->ADDMBR; ++i) { \
      for ( ii=0; ii < MAXNUMSEQDEF; ++ii) {  \
        if ( !cas->casDefList[ii]) break; \
      } \
      if ( ii < MAXNUMSEQDEF) { \
        cas->casDefList[ii] = orbit_CAS_new( CASTYP); \
        if ( cas->casDefList[ii]) { \
        CAS *newCas = cas->casDefList[ii]; \
          topChanged = 1; \
          newCas->TOPINDEX = ii; \
          newCas->top = cas; \
          orbit_CAS_updateTop( newCas); \
          newCas->_miscPtr = cas->_miscPtr; \
          strcpy( newCas->_savename, newCas->_name); \
          orbitgui_CAS_addToMiscPtr( newCas); \
        } \
      } \
    }

    topChanged = 0;
    ADDSEQDEF( casDefAddMsi, CASTYPE_MSISEQDEF, msiSeqDefTopIndex)
    ADDSEQDEF( casDefAddNis, CASTYPE_NISSEQDEF, nisTopIndex)

    break;      /* case CASTYPE_CASDEF */

  /* OPNAV CAS - get bit fields, extend missing values out to end */

  case CASTYPE_CASOPNAV:

    fprintf(stderr,"about to free 0x%08lx", (long) fldMenu->lbl_txt);
    fflush(stderr);
    free( fldMenu->lbl_txt);        /* free first menu label strduped below */
    fprintf(stderr,"\n");fflush(stderr);

    subType = cas->opnavSubType;
    
    if ( opnavArgFragCount[subType][OPNAVARG_PRIO] > 0) {
      FINDFMP( fld_lng, opnavBits, 0, break;)
    } else {
      FINDFMP( fld_lng, opnavSlewDur, 0, break;)
    }

    subType = cas->opnavSubType;
    for ( iArg=0; iArg<OPNAVARG_COUNT; ++iArg) {
    long di, iPass;

      di = opnavArgFragCount[subType][iArg];
      if ( di > 0 ) switch (iArg) {

      case OPNAVARG_PRIO:
        cas->opnavPrio[0] = (cas->opnavBits[0] & 1L) ? 0 : 1;
        fm++;
        break;

#       define FILLINOPNAV(MBR) \
        if ( di == 1) { \
          fm++; \
        } else { \
        int iOffset = 0; \
          while ( di > 0) { \
          static char c[255]; \
            fprintf(stderr,"about to MYFREE 0x%08lx", (long) fm->lbl_txt); \
            fflush(stderr); \
            MYFREE( fm->lbl_txt); /* from STRDUP() below */ \
            fprintf(stderr,"\n"); fflush(stderr); \
            /* special code to determine if there were actually 0 fields */ \
            if ( fm->subtype == 1) { \
              if ( 0.0 == ((fm->type==FLD_LNG8) ? (double)fm->fld_lng[0] \
                                                : fm->fld_dbl[0]) ) { \
                if ( sscanf(fm->fld_txt0, "%s", c) != 1) fm->subtype = 0; \
              } \
            } \
            FILLIN( iOffset, MBR, iOffset+di); \
            di -= fm->fld_txt_maxlen; \
            iOffset += fm->fld_txt_maxlen; \
            fm++; \
          } \
        }

      case OPNAVARG_SLEWDUR:
        FILLINOPNAV(opnavSlewDur)
        break;

      case OPNAVARG_STARTNTHSLEW:
      case OPNAVARG_MISCINT:
      case OPNAVARG_FRAMES:
        while ( di--) {
          fm++;
        }
        break;

      case OPNAVARG_DS40AIMXYZ:
      case OPNAVARG_DS40AIMSHORT:
      case OPNAVARG_DS40BOREXYZ: /* these three handled by _DS40COORDSYS */
        break;
      case OPNAVARG_DS40COORDSYS:            /* this must have the largest di */
        for ( iPass=0; iPass<di; ++iPass) {

          cas->opnavDs40[iPass]->ds40AimptFrmType =     /* Aimpoint Coord Sys */
            orbit_getFrmByBit( cas->opnavDs40[iPass]->ds40Bits & DS40BITallSys
                             , DS40I000Sys, ds40SysArray);
          fm++;

          fm++;                                            /* Aimpoint Vector */



          if ( opnavArgFragCount[subType][OPNAVARG_DS40AIMSHORT] > iPass) {

            cas->opnavDs40[iPass]->ds40AimptSelect = /*Aimpt shortcut (select)*/
              orbit_getFrmByBit( cas->opnavDs40[iPass]->ds40Bits & DS40BITallSel
                               , DS40I000Sel, ds40SelArray);
            fm++;
          }

          fm++;                                           /* Boresight Vector */

        } /* for iPass */

        break; /* case OPNAVARG_DS40COORDSYS */


      case OPNAVARG_DS56:
        for ( iPass=0; iPass<di; ++iPass) {

          fm++;                                         /* DS56 scan duration */

          /* bits:  coord sys, change, pause after */

          cas->opnavDs56[iPass]->ds56FrmType =                   /* coord sys */
            orbit_getFrmByBit( cas->opnavDs56[iPass]->ds56Bits & DS56BITallSys
                             , DS56I000Sys, ds56SysArray);
          fm++;

          fm++; fm++;                                  /* change, pause after */

          fm++; fm++; fm++;/* VECs:  rate duration, ds56 rate, pause duration */
        }
        break;

      case OPNAVARG_IM_TYPE:
        /* use FILLINOPNAV to fill in to fld_txt_maxlen, which may go beyond
         * opnavShoot[0]->msiShootCount, but it does not matter
         */
        FILLINOPNAV( opnavShoot[0]->msiShootImageTypeDbl)

        /**************************************
        /* DOUBLE_K:  image type funny business
         */
        if (subType == OPNAVTYPE_DOUBLE_K) {
         cas->opnavShoot[1]->msiShootImageTypeDbl[0] =  /*- 2nd shoot's imtyp */
           cas->opnavShoot[0]->msiShootImageTypeDbl[1]; /* is stored in 1st's */
        }

        break;

      case OPNAVARG_IM_DELTA:
        /* same here as for OPNAVARG_IM_TYPE */
        FILLINOPNAV( opnavImDeltas);

        break; /* case OPNAVARG_IM_DELTA */

      default:
        break;
      } /* switch iArg */

    } /* for iArg<OPNAVARG_COUNT */

    orbit_CAS_opnavSetChildren( cas);  /* propagate timings et al to children */

    break;      /* case CASTYPE_CASOPNAV */

  default:
    break;
  } /* switch cas->_type */

  if ( cas->top) orbit_CAS_updateTop( cas); /* update cas->top->casArgListPtr */

  free( fldMenu);
  return 1;          /* delete field menu */
} /* orbitgui_CAS_fldMenu_ok_CB( CAS *cas, FLDMENU *fldMenu) { */

/*******************************************/

int 
orbitgui_CAS_fldMenu_cancel_CB( CAS *cas, FLDMENU *fldMenu) {
SUBFRAGS *lclSubFrags;
CAS **casPtr;
FLDMENU *fm;

  /* free cas if it came from a "xxx" button */

  if ( cas->_malloced == CAS_MALLOC_NEW) {

    if isCASOPNAV(*cas) {                    /* free any OPNAV children first */
      FOROPNAVSUBFRAGS( cas, lclSubFrags, casPtr) 
        free( *casPtr);
      }}
    }
    free( cas);                                        /* free the cas itself */
  }

  switch ( cas->_type) {
  case CASES_NOPARAMS:
    fprintf(stderr,"about to free 0x%08lx", (long) fldMenu->lbl_txt); \
    free( fldMenu->lbl_txt);          /* free first menu label strduped below */
    fprintf(stderr,"\n");fflush(stderr);
    break;
  case CASTYPE_CASOPNAV:
    fprintf(stderr,"about to free 0x%08lx", (long) fldMenu->lbl_txt); \
    free( fldMenu->lbl_txt);          /* free first menu label strduped below */
    fprintf(stderr,"\n");fflush(stderr);

    if ( opnavArgFragCount[cas->opnavSubType][OPNAVARG_SLEWDUR] > 1) {
      FINDFM( fld_lng, cas->opnavSlewDur, 100, ;)
      if ( fm->type != FLD_END) {
        MYFREE( fm->lbl_txt);
        if ( fm->fld_txt_maxlen == 6) 
          if ( opnavArgFragCount[cas->opnavSubType][OPNAVARG_SLEWDUR] > 6)
            MYFREE( (fm+1)->lbl_txt);
      }
    }

    if ( opnavArgFragCount[cas->opnavSubType][OPNAVARG_IM_TYPE] > 1) {
      FINDFM( fld_dbl, cas->opnavShoot[0]->msiShootImageTypeDbl, 101, ;)
      if ( fm->type != FLD_END) {
        MYFREE( fm->lbl_txt);
        if ( fm->fld_txt_maxlen == 6) 
          if ( opnavArgFragCount[cas->opnavSubType][OPNAVARG_IM_TYPE] > 6)
            MYFREE( (fm+1)->lbl_txt);
      }
    }

    if ( opnavArgFragCount[cas->opnavSubType][OPNAVARG_IM_DELTA] > 1) {
      FINDFM( fld_lng, cas->opnavImDeltas, 102, ;)
      if ( fm->type != FLD_END) {
        MYFREE( fm->lbl_txt);
        if ( fm->fld_txt_maxlen == 6) 
          if ( opnavArgFragCount[cas->opnavSubType][OPNAVARG_IM_DELTA] > 6)
            MYFREE( (fm+1)->lbl_txt);
      }
    }
    /**********************************
     * DOUBLE_K:  reset image type funny business
     */
    if ( cas->opnavSubType == OPNAVTYPE_DOUBLE_K) {
      cas->opnavShoot[0]->msiShootImageTypeDbl[1] = 
        cas->opnavShoot[0]->msiShootImageTypeDbl[0];
    }
    break; /* case CASTYPE_CASOPNAV) */

  default:
    break;
  }

  free( fldMenu);
  return 1;
} /* orbitgui_CAS_fldMenu_cancel_CB( CAS *cas, FLDMENU *fldMenu) { */

/********************************************************/
/* create menu to modify cas;
 * - casType ignored if cas != 0
 * - create cas of type casType if cas == 0
 *   ***N.B. if CASTYPE_COUNT <= casType < (CASTYPE_COUNT + OPNAVTYPE_COUNT),
 *           orbit_CAS_new will create cas of type CASTYPE_CASOPNAV and 
 *           subType (casType-CASTYPE_COUNT), so do not use casType after 
 *           that point
 */

void 
orbitgui_create_CAS_fldMenu( CAS *cas, int casType, OCWS *CASWStuff, Widget w) {

#define FMLBL(ID) \
    ID \
  , "Sched pt (UTC)" \
  , "Delay to start, s" \
  , "Enabled?\0In=>enabled\0"

#define NFMLBL0 4

/* special Scheduling Point label for CASes that have parents */

static char *ds40XgrsFmLbl[] = {
    FMLBL( "DS40XGRS ID")
  , "DS40XGRS Slew Duration"
  , "DS40XGRS Aimpt Coord Sys\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40XGRS Aimpt (km or deg)"
  , (char *) 0
  };

static char *xgrsConfFmLbl[] = {
    FMLBL( "XGRSCONFIG ID")
  , "XGRSCONFIG Position\0AL\0CLR\0HOME\0MG\0"
  , "XGRSCONFIG Direction\0FOR\0REV\0"
  , (char *) 0
  };

static char *ds56CASFmLbl[] = {
    FMLBL( "DS56 ID")
  , "DS56 Reuse Prev Scan\0TRUE\0FALSE\0"
  , "DS56 Scan Duration"
  , "DS56 Coord Sys\0J2K\0NAZ\0ABF\0ACI\0SBF\0"
  , "DS56 Change Dir\0X\0Y\0Z\0"
  , "DS56 Pause After\0X\0Y\0Z\0"
  , "DS56 Rate Duration (X/Y/Z), s"
  , "DS56 Rate (X/Y/Z), ./s"
  , "DS56 Pause Duration (X/Y/Z), s"
  , (char *) 0
  };

static char *ds40CASFmLbl[] = {
    FMLBL( "DS40 ID")
  , "DS40 Slew Duration"
  , "DS40 Aimpt Coord Sys\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt (km or deg)"
  , "DS40 Aimpt Shortcut\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Boresight (X/Y/Z)"
  , (char *) 0
  };

static char *ds40FullCASFmLbl[] = {
    FMLBL( "DS40 FULL ID")
  , "DS40 Slew Duration"
  , "DS40 Aimpt Coord Sys\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt (km or deg)"
  , "DS40 S/C Roll Vec (X/Y/Z)"
  , "DS40 Aimpt Shortcut\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Roll Ref (X/Y/Z)"
  , "DS40 Boresight (X/Y/Z)"
  , "DS40 Roll Algorithm\0J2k\0SCI\0ECI\0Nad\0"
  , (char *) 0
  };

static char *nisSu2FmLbl[] = {
    FMLBL( "NIS SU2 ID")
  /* , "NISSU2 Start Seq'ing, s" /**/
  , "NISSU2 Sequence"
  , "NISSU2 Init Mir Pos"
  , "NISSU2 Aperture\0IN\0OUT\0SHTR\0SLIT\0"
  , "NISSU2 Gain\0 10X\0 1X\0"
  , "NISSU2 Posns/SuperScan"
  , "NISSU2 Step Direction\0FORWARD\0BACKWARD\0"
  , (char *) 0
  };

static char *nisSuFmLbl[] = {
    FMLBL( "NIS SU ID")
  , "NISSU1 Start Seq'ing, s"
  , "NISSU1 Sequence"
  , "NISSU1 Init Mir Pos"
  , "NISSU1 Aperture\0IN\0OUT\0SHTR\0SLIT\0"
  , "NISSU1 Gain\0 10X\0 1X\0"
  , "NISSU1 Posns/SuperScan"
  , "NISSU1 # of Super Scans"
  , (char *) 0
  };

static char *nisCASEXFmLbl[] = {
    FMLBL( "NIS Execute ID")
  , "NISEX Start Seq'ing, s"
  /* ***N.B. No SETUP */
  , "NISEX Sequence"
  , "NISEX Mirror Pos"
  , "NISEX Aperture\0IN\0OUT\0SHTR\0SLIT\0"
  , "NISEX Gain\0 10X\0 1X\0"
  , (char *) 0
  };

static char *nisCASSRFmLbl[] = {
    FMLBL( "NIS Single Repeat ID")
  , "NISSR Start Seq'ing, s"
  , "NISSR Setup, s"
  , "NISSR Setup Mirror Pos"
  , "NISSR Setup Aperture\0IN\0OUT\0SHTR\0SLIT\0"
  , "NISSR Setup Gain\0 10X\0 1X\0"
  , "NISSR Sequence # 1"
  , "NISSR Interval 1"
  , "NISSR Iterations"
  , (char *) 0
  };

static char *nisCASDRFmLbl[] = {
    FMLBL( "NIS Double Repeat ID")
  , "NISDR Start Seq'ing, s"
  , "NISDR Setup, s"
  , "NISDR Setup Mirror Pos"
  , "NISDR Setup Aperture\0IN\0OUT\0SHTR\0SLIT\0"
  , "NISDR Setup Gain\0 10X\0 1X\0"
  , "NISDR Sequence # 1 & 2"
  , "NISDR Mirror Pos 1 & 2"
  , "NISDR Aperture 1\0IN\0OUT\0SHTR\0SLIT\0NOCH\0"
  , "NISDR Aperture 2\0IN\0OUT\0SHTR\0SLIT\0NOCH\0"
  , "NISDR Gain 1\0 10X\0 1X\0NOCH\0"
  , "NISDR Gain 2\0 10X\0 1X\0NOCH\0"
  , "NISDR Interval 1 & 2"
  , "NISDR Iterations" 
  , (char *) 0
  };

static char *msiCASTRFmLbl[] = {
    FMLBL( "MSI Triple Repeat ID")
  , "MSITR Start imaging, s"
  , "MSITR Seqs 1 2 3 #'s"
  , "MSITR Deltas 1 2 3, s"
  , "MSITR Image Types 1 2 3"
  , "MSITR # Iterations"
  , (char *) 0
  };

static char *msiCASSRFmLbl[] = {
    FMLBL( "MSI Single Repeat ID")
  , "MSISR Start imaging, s"
  , "MSISR Seq #"
  , "MSISR Delta btw Seq 1 executes, s"
  , "MSISR # Iterations"
  , "MSISR Image Type"
  , (char *) 0
  };

static char *msiCASDRFmLbl[] = {
    FMLBL( "MSI Double Repeat ID")
  , "MSIDR Start imaging, s"
  , "MSIDR Seqs 1 & 2 #'s"
  , "MSIDR Seq Deltas 1 to 2 & 2 to 1, s"
  , "MSIDR Paired Seqs # Iters"
  , "MSIDR Image Type"
  , (char *) 0
  };

static char *msiCASDSRFmLbl[] = {
    FMLBL( "MSI Dbl Stag Rep ID")
  , "MSIDSR Seqs 1 & 2 Starts imaging, s"
  , "MSIDSR Seqs 1 & 2 #'s"
  , "MSIDSR Seq Deltas 1 to 1 & 2 to 2, s"
  , "MSIDSR Seqs 1 & 2 # Iters"
  /* , "MSIDSR 1st Seq 2 start after 1st Seq 1, s" */
  , "MSIDSR Image Type"
  , (char *) 0
  };

static char *msiCASShootFmLbl[] = {
    "SHOOT ID"
  , "CAS Sched pt (UTC)" \
  /* , "Delay to start, s" /* NO DELAY FOR SHOOT */
  , "Enabled?\0In=>enabled\0"
  , "SHOOT # of Executes"
  , "SHOOT Seq #'s 1-6"
  , "SHOOT Seq #'s 7-12"
  , "SHOOT Image Deltas 1-6, s"
  , "SHOOT Image Deltas 7-12, s"
  , "SHOOT Image Types 1-6"
  , "SHOOT Image Types 7-12"
  , (char *) 0
  };

static char *msiCASSeqDefFmLbl[] = {
    FMLBL( "MSISEQDEF ID")
  , "MSISEQDEF Sequence # (1-30)"
  , "MSISEQDEF # of Images"
  , "MSISEQDEF Interval, s"
  , "MSISEQDEF Comp Table\0-\0 1\0 2\0 3\0 4\0 5\0 6\0 7\0"
  , "MSISEQDEF Flag DPCM\0On\0Off\0"
  , "MSISEQDEF Comp alg\0None\0Fast\0Rice\0"
  , "MSISEQDEF Pixels (1-16)"
  , "MSISEQDEF Mode\0Auto\0Man\0"
  , "MSISEQDEF Images' durations, ms"
  , "MSISEQDEF Images' filters"
  , (char *) 0
  };

static char *nisCASSeqDefFmLbl[] = {
    FMLBL( "NISSEQDEF ID")
  , "NISSEQDEF Sequence ID (3-15)"
  , "NISSEQDEF # of scans"
  , "NISSEQDEF Seconds btw scans"
  , "NISSEQDEF # obs / scan"
  , "NISSEQDEF Cal interval"
  , "NISSEQDEF # Spectra/obs"
  , "NISSEQDEF # of rests"
  , "NISSEQDEF # of darks"
  , "NISSEQDEF Mirror steps/obs"
  , "NISSEQDEF Seconds btw obs"
  , (char *) 0
  };

/* no params for MSI/NIS/XGRSPARK, MSI/NIS/XGRS/NAVRELATT, NISCAL/BUFFLUSH */

static char *noParamsFmLbl[] = { 
  FMLBL( "")                       /* label will be allocated later */
, (char *) 0
  };

static char *msiAutoFmLbl[] = {
    FMLBL( "MSIAUTO ID")
  , "MSIAUTO T Im Delta"
  , "MSIAUTO Filt For T Im"
  , "MSIAUTO T Im Exp Time"
  , "MSIAUTO Allowable Sat"
  , "MSIAUTO Target Saturation DN"
  , "MSIAUTO Ovr Exp Flbk Tim"
  , "MSIAUTO Noise Offset"
  , (char *) 0
  };

static char *msiLoadFiltFmLbl[] = {
    FMLBL( "MSILOADFILT ID")
  , "MSILOADFILT Sens F0"
  , "MSILOADFILT Sens F1"
  , "MSILOADFILT Sens F2"
  , "MSILOADFILT Sens F3"
  , "MSILOADFILT Sens F4"
  , "MSILOADFILT Sens F5"
  , "MSILOADFILT Sens F6"
  , "MSILOADFILT Sens F7"
  , (char *) 0
  };

static char *msiConfFmLbl[] = {
    FMLBL( "MSICONFIG ID")
  , "MSICONFIG Full Im\0NONE\0UNCHG\0VC2\0"
  , "MSICONFIG Sum Im\0NONE\0UNCHG\0VC30\0"
  , (char *) 0
  };

static char *nisConfFmLbl[] = {
    FMLBL( "NISCONFIG ID")
  , "NISCONFIG Slit Drive Select\0NOCHG\0PRI\0SEC\0"
  , "NISCONFIG Shutter Drive Select\0NOCHG\0PRI\0SEC\0"
  , "NISCONFIG Voltage Setting\0NOCHG\0 15V\0 20V\0"
  , "NISCONFIG Scan Mirror Drive Select\0NOCHG\0PRI\0SEC\0"
  , (char *) 0
  };

static char *reqCASFmLbl[] = {
    "REQ"
  , REQ_SCHED_POINT
  , REQ_START_DELAY
  , "Enabled?\0In=>enabled\0"
  , "Add N CAS(s)"
  , "CAS Template"
  , "Add N CASDEF(s)"
  , "CASDEF Template"
  , "Add CAS_OPNAV_\0A\0B\0BP\0C\0C_A\0CE\0"
  , "Add CAS_OPNAV_\0CT\0D\0E\0E_2\0E_3\0E_4\0"
  , "Add CAS_OPNAV_\0F  \0G\0H\0I\0J\0K\0D_K\0"
  , "CASOPNAV Template"
  , (char *) 0
  };

#define NCASDEFLBL 6
static char *casDefFmLbl[NCASDEFLBL+MAXNUMSEQDEF+1] = {
    "CASDEF"
  , CASDEF_SCHED_POINT
  , CASDEF_START_DELAY
  , "Enabled?\0In=>enabled\0"
  , "Add N MSISEQDEF(s)"
  , "Add N NISSEQDEF(s)"
  /* plus up to 60 children */
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
  , (char *) 0 /* plus null terminator */
  };

#define NTOPLBL 4
static char *casCASFmLbl[] = {
    "CAS"
  , CAS_SCHED_POINT
  , CAS_START_DELAY
  , "Enabled?\0In=>enabled\0"
#define NCASCASFMLBL (NTOPLBL+6)/* Base labels + 6 sets of labels for buttons */
  , "Add/Delete\0DS40\0FULL\0XGRS\0DS56\0" /* NADIR\0" */
  , "Add/Delete\0SHOOT\0MSISR\0MSIDR\0MSIDSR\0MSITR\0"
  , "Add/Delete\0NISEX\0NISSR\0NISDR\0NISSU2\0NISSU1\0"
  , "Add/Delete (1&ONLY1)\0NISCAL\0NISBUFL\0NISREL\0NISCONF\0NISPARK\0"
  , "Add/Delete (1&ONLY1)\0AUTOEXP\0LDFILT\0MSIREL\0MSICONF\0MSIPARK\0"
  , "Add/Delete (1&ONLY1)\0XGRSREL\0XGRSCONF\0XGRSPARK\0"
  , (char *) 0  /* DS40 */
  , (char *) 0  /* FULL */
  , (char *) 0  /* XGRS */
  , (char *) 0  /* DS56 */
  , (char *) 0  /* SHOOT */
  , (char *) 0  /* MSISR */
  , (char *) 0  /* MSIDR */
  , (char *) 0  /* MSIDSR */
  , (char *) 0  /* MSITR */
  , (char *) 0  /* NISEX */
  , (char *) 0  /* NISSR */
  , (char *) 0  /* NISDR */
  , (char *) 0  /* NISSU2 */
  , (char *) 0  /* NISSU1 */
  , (char *) 0  /* NISCAL */
  , (char *) 0  /* NISBUFL */
  , (char *) 0  /* NISREL */
  , (char *) 0  /* NISCONF */
  , (char *) 0  /* NISPARK */
  , (char *) 0  /* AUTOEXP */
  , (char *) 0  /* LDFILT */
  , (char *) 0  /* MSIREL */
  , (char *) 0  /* MSICONF */
  , (char *) 0  /* MSIPARK */
  , (char *) 0  /* XGRSREL */
  , (char *) 0  /* XGRSCONF */
  , (char *) 0  /* XGRSPARK */
  , (char *) 0  /* terminator - for FLD_END */
  };

static char *casOpnavFmLbl[] = {
  "CAS_OPNAV_C_A_CRUISExxxxxxxxx"        /* longest possible name + for opnav */
, CASOPNAV_SCHED_POINT
, CASOPNAV_START_DELAY
, "Enabled?\0In=>enabled\0"
#define NCASOPNAVFMLBL (NTOPLBL+0)                        /* Base labels only */
/* up to 34 fields (CAS_OPNAV_DOUBLE_K) */
, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0, (char *) 0
, (char *) 0, (char *) 0, (char *) 0, (char *) 0
, (char *) 0                                      /* terminator - for FLD_END */
};

char *casOpnavPrioSource[] = { "Priority\0False\0True\0" };

#define CASOPNAV_SLEWDURFMT "Slew durations (%ld-%ld) (s)"

char *casOpnavSlewDurSource[] = { 
    "Slew duration (s)"
  , "Slew durations (01-06) (s)"
 };
char *casOpnavStartNthSlewSource[] = { "Start second slew (s)" };

char *casOpnavDeltaK[] = { "Delta K (s)" };

char *casOpnavDs56Source[] = {
    "DS56 Scan Duration"
  , "DS56 Coord Sys\0J2K\0NAZ\0ABF\0ACI\0SBF\0"
  , "DS56 Change Dir\0X\0Y\0Z\0"
  , "DS56 Pause After\0X\0Y\0Z\0"
  , "DS56 Rate Duration (X/Y/Z), s"
  , "DS56 Rate (X/Y/Z), ./s"
  , "DS56 Pause Duration (X/Y/Z), s"

  , "DS56 Scan Duration 1"
  , "DS56 Coord Sys 1\0J2K\0NAZ\0ABF\0ACI\0SBF\0"
  , "DS56 Change Dir 1\0X\0Y\0Z\0"
  , "DS56 Pause After 1\0X\0Y\0Z\0"
  , "DS56 Rate Duration 1 (X/Y/Z), s"
  , "DS56 Rate 1 (X/Y/Z), ./s"
  , "DS56 Pause Duration 1 (X/Y/Z), s"

  , "DS56 Scan Duration 2"
  , "DS56 Coord Sys 2\0J2K\0NAZ\0ABF\0ACI\0SBF\0"
  , "DS56 Change Dir 2\0X\0Y\0Z\0"
  , "DS56 Pause After 2\0X\0Y\0Z\0"
  , "DS56 Rate Duration 2 (X/Y/Z), s"
  , "DS56 Rate 2 (X/Y/Z), ./s"
  , "DS56 Pause Duration 2 (X/Y/Z), s"
  };

char *casOpnavNFramesSource[] = {
    "# Frames to collect"
  , "# Frames to collect 1"
  , "# Frames to collect 2"
  };

#define CASOPNAV_IMGTYPFMT "Image type (%ld-%ld)"

char *casOpnavImgTypSource[] = {
    "Image type"
  , "Image type (01-06)........"    /* 1-(2-6) */
  , "Image type (07-12)........"    /* 7-(8-12) */
};

#define CASOPNAV_IMGDELTAFMT "Image delta (%ld-%ld)"

char *casOpnavImgDeltaSource[] = {
    "Image delta"
  , "Image delta (01-06)xxxxxxxxx"   /* 1-(2-6) */
  , "Image delta (07-12)xxxxxxxxx"   /* 7-(8-12) */
  , "Image delta 2 (01-06)xxxxxxxxx"   /* 1-(2-6) */
  , "Image delta 2 (07-12)xxxxxxxxx"   /* 7-(8-12) */
};

char *casOpnavDs40Source[] = {
    "DS40 Aimpt Coord Sys\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt (km or deg)"
  , "DS40 Aimpt Shortcut\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Boresight (X/Y/Z)"

  , "DS40 Aimpt Coord Sys 1\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt 1 (km or deg)"
  , "DS40 Aimpt Shortcut 1\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Boresight 1 (X/Y/Z)"

  , "DS40 Aimpt Coord Sys 2\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt 2 (km or deg)"
  , "DS40 Aimpt Shortcut 2\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Boresight 2 (X/Y/Z)"

  , "DS40 Aimpt Coord Sys 3\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt 3 (km or deg)"
  , "DS40 Aimpt Shortcut 3\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Boresight 3 (X/Y/Z)"

  , "DS40 Aimpt Coord Sys 4\0J2k\0ECI\0SCI\0ACI\0Nad\0ABF\0"
  , "DS40 Aimpt 4 (km or deg)"
  , "DS40 Aimpt Shortcut 4\0Aim\0SCI\0ECI\0Nad\0"
  , "DS40 Boresight 4 (X/Y/Z)"
};

int numflds, i, icount, ii, mtl, numFilenames, iBit, numBits;
char **lcllbl0, *txt0, *txtFn;
FLDMENU *fldMenu, *fm;
CAS *lclcas = cas;
static long casNum;
UTC utc;
int iArg;
long subType;
char *strDupes[100];
int iStrDupe;

  for ( iStrDupe=100; iStrDupe; ) strDupes[--iStrDupe] = (char *) NULL;

# define STRDUP(A) (strDupes[iStrDupe++] = MYSTRDUP(A)) /**/
# define REALSTRDUP(A) (strDupes[iStrDupe++] = strdup(A)) /**/
# define STRDUPRTN while ( iStrDupe--) { MYFREE(strDupes[iStrDupe]); } return
# define STRDUPLIST \
  while ( iStrDupe--) { \
  char *c; \
    c = strDupes[iStrDupe]; \
    fprintf( stderr, "strDupes[%d]='%s'(0x%08lx)\n" \
           , iStrDupe, c?c:"<null>", (long)c); \
  } \
  fflush( stderr)

  /* if cas argument is null CAS pointer create new CAS struct */

  if ( !lclcas) {

    if ( !(lclcas=orbit_CAS_new( casType)) ) return;

    casType = lclcas->_type;                 /* reset casType in case someone */
                                       /* ignores the _CASOPNAV warning above */

    /* store ptr to OCWS struct; used later by orbitgui_CAS_fldmenu_ok_CB
     * get UTC from cur_item pointer
     */
    lclcas->_miscPtr = (void *) CASWStuff;
    orbitgui_return_utc_ci( cur_item, &utc);

  /* if cas is not null CAS pointer, use existing cas 
   * - convert cas->_ptrEt to UTC text - save et to local UTC struct
   */
  } else { 

    /* if this CAS has a parent, it is not using it's own et, so 
     * calculate the actual scheduling point (before delay)
     */
    if ( cas->top) cas->_et = CASSTART( cas) - cas->_delayStart;

    orbit_et2utc( cas->_et, &utc);       /* *_ptrEt = _et for top level CASes */

    /* orbit_et2utc( *cas->_ptrEt, &utc); /* old line */

  }

  /*************************************************************/
  /***** AFTER THIS POINT, USE ONLY lclcas TO REFER TO CAS *****/ 
  /*************************************************************/

  /* set up to use apropo labels for lclcas->_type */

  switch ( lclcas->_type) {
  case CASTYPE_SHOOT:        lcllbl0 = msiCASShootFmLbl; break;
  case CASTYPE_MSISEQDEF:    lcllbl0 = msiCASSeqDefFmLbl; break;
  case CASTYPE_NISSEQDEF:    lcllbl0 = nisCASSeqDefFmLbl; break;
  case CASTYPE_NISSU2:       lcllbl0 = nisSu2FmLbl; break;
  case CASTYPE_NISSU1:       lcllbl0 = nisSuFmLbl; break;
  case CASTYPE_NISEX:        lcllbl0 = nisCASEXFmLbl; break;
  case CASTYPE_NISSR:        lcllbl0 = nisCASSRFmLbl; break;
  case CASTYPE_NISDR:        lcllbl0 = nisCASDRFmLbl; break;
  case CASTYPE_MSISR:        lcllbl0 = msiCASSRFmLbl; break;
  case CASTYPE_MSIDR:        lcllbl0 = msiCASDRFmLbl; break;
  case CASTYPE_MSIDSR:       lcllbl0 = msiCASDSRFmLbl; break;
  case CASTYPE_MSITR:        lcllbl0 = msiCASTRFmLbl; break;
  case CASTYPE_DS56:         lcllbl0 = ds56CASFmLbl; break;
  case CASTYPE_DS40XGRS:     lcllbl0 = ds40XgrsFmLbl; break;
  case CASTYPE_DS40:         lcllbl0 = ds40CASFmLbl; break;
  case CASTYPE_DS40FULL:     lcllbl0 = ds40FullCASFmLbl; break;
  case CASTYPE_REQ:          lcllbl0 = reqCASFmLbl; break;
  case CASES_NOPARAMS:       lcllbl0 = noParamsFmLbl; break;
  case CASTYPE_AUTOEXPOSE:   lcllbl0 = msiAutoFmLbl; break;
  case CASTYPE_LOADFILT:     lcllbl0 = msiLoadFiltFmLbl; break;
  case CASTYPE_MSICONFIG:    lcllbl0 = msiConfFmLbl; break;
  case CASTYPE_NISCONFIG:    lcllbl0 = nisConfFmLbl; break;
  case CASTYPE_XGRSCONFIG:   lcllbl0 = xgrsConfFmLbl; break;
  case CASTYPE_CAS:
    lcllbl0 = casCASFmLbl;

    numflds = NCASCASFMLBL;/*for CASCAS, add as many lbls as non-null arglists*/
    lclcas->casBits = 0;
    for (i=0; i<CASTYPE_COUNT; ++i) {
      if (lclcas->casArgListPtrs[i]) {
        lclcas->casBits |= (1L<<i);
        lcllbl0[numflds++] = orbit_CAS_TypeToName(i);
      }
    }
    lcllbl0[numflds++] = (char *) 0;  /* terminator - FLD_END */
    break;

  case CASTYPE_CASDEF:

    lcllbl0 = casDefFmLbl;

    numflds = NCASDEFLBL;                   /* for CASDEF, same as for CASCAS */
    for ( i=0; i<MAXNUMSEQDEF; ++i) {
      if ( lclcas->casDefList[i]) {
        lcllbl0[numflds++] = orbit_CAS_TypeToName(lclcas->casDefList[i]->_type);
      }
    }
    lcllbl0[numflds++] = (char *) 0;  /* terminator - FLD_END */
    break;

  case CASTYPE_CASOPNAV:
    lcllbl0 = casOpnavFmLbl;              /* count number of fields req'd ... */
    subType = lclcas->opnavSubType;                   /* ... for this subType */
    numflds = NCASOPNAVFMLBL;
    for ( iArg=0; iArg<OPNAVARG_COUNT; ++iArg) {
    long di;
    static char dummyStr[] = { "dummyStr" };
      di = opnavArgFragCount[subType][iArg];
      switch (iArg) {
      case OPNAVARG_SLEWDUR:
      case OPNAVARG_STARTNTHSLEW:
        di = (di + 5) / 6;                                  /* max 6 per line */
        break;
      case OPNAVARG_DS56:
        di = 7 * di;      /* scanDur, sys, chg, hold, ratedur, rate, pausedur */
        break;
      case OPNAVARG_IM_TYPE:
      case OPNAVARG_IM_DELTA:
        di = (di + 5) / 6;                                  /* max 6 per line */
        break;
      default:
        /* di = di */
        break;
      } /* switch subType */
      while ( di-- > 0) {
        lcllbl0[numflds++] = dummyStr;    /* actual strings will be set later */
      }
    } /* for iArg<OPNAVARG_COUNT */
    lcllbl0[numflds] = (char *) NULL; /* so counter loop below finds this one */
    break;

  default:
    fprintf( stderr, "***orbitgui_create_CAS_fldMenu:  Unknown CASTYPE (%d)\n"
           , lclcas->_type);
    fflush( stderr);
    return;   /* assumes no STRDUP above here yet */

  } /* switch lclcas->_type */

  /* count labels - i.e. non-null fields */
  for ( numflds=0; lcllbl0[numflds]; numflds++) ;
  numflds++;    /* FLD_END */

  /* allocate field menu structures 
   *  plus space for fld_txt0 fields 
   *  plus space for UTC text string
   *  plus space for filenames
   */
  mtl = MTL(lclcas->_type);
  numFilenames = (lclcas->_type == CASTYPE_REQ) ? 3 : 0;
  fldMenu = (FLDMENU *) malloc((numflds * (sizeof( FLDMENU) + mtl)) + UTCLEN+1
          + MAXLENFILNAM * numFilenames);

  txt0 = (char *) (fldMenu + numflds);
  txtFn = txt0 + (numflds*mtl) + UTCLEN+1;

# ifdef MKFLDLNG
# undef MKFLDLNG
# undef MKFLDLNGP
# undef MKFLDBIT
# undef MKFLDBITS
# endif

# define MKFLDLNG(A) MKFLDANYLNG(fm,lclcas->A); NEXTFM
# define MKFLDLNGP(A) MKFLDANYLNGP(fm,lclcas->A); NEXTFM

# define MKFLDBIT(CASBITS,ZEROBIT) \
  MKFLDANYBIT(fm,lclcas->CASBITS,ZEROBIT); \
  NEXTFM
# define MKFLDBITS(CASBITS,ZEROBIT) \
  MKFLDANYBITS(fm,lclcas->CASBITS,ZEROBIT); \
  NEXTFM

# ifdef NEXTFM
# undef NEXTFM
# endif
# define NEXTFM fm++, i++

  /* setup fieldmenu structure pointers to labels & to text strings */
  for ( fm=fldMenu, i=0; i<numflds; NEXTFM) {
    fm->lbl_txt = lcllbl0[i];
    fm->fld_txt0 = txt0 + (i * mtl);
  }

  fm = fldMenu;
  i = 0;

  /* ->_name */
  fm->type = FLD_TXT; fm->fld_txt = lclcas->_name;
  strncpy( fm->fld_txt0, lclcas->_name, IDLEN+1);
  fm->fld_txt0[IDLEN] = '\0';
  fm->fld_txt_maxlen = IDLEN;
  NEXTFM;

  /* ->_et - scheduling point, as UTC text
   *   - use extra string at end of txt0 strings
   *   - get UTC text from UTC struct member _utcstr
   *   - ok callback will convert UTC string back to ->_et
   */
  fm->type = FLD_TXT; fm->fld_txt = txt0 + (mtl * numflds);
  strncpy( fm->fld_txt, utc._utcstr, UTCLEN+1);
  fm->fld_txt[UTCLEN] = '\0';
  strncpy( fm->fld_txt0, fm->fld_txt, UTCLEN+1);
  fm->fld_txt_maxlen = UTCLEN;
  NEXTFM;

  /* ->_delayStart offset from scheduling point
   *  - not used for SHOOT, set to 0 to be sure
   */
  if isSHOOT( *lclcas) lclcas->_delayStart = 0.0;
  else {
    fm->type = FLD_DBL; fm->fld_dbl = &lclcas->_delayStart; 
    NEXTFM;
  }

  /* _enabled */
  fm->type = FLD_BITS; fm->fld_lng = &lclcas->_enabled;
  fm->fld_lowbit = 0;
  NEXTFM;

  /* field menu setup unique to each CASTYPE */

  switch ( lclcas->_type) {

#define SETFRMBIT( CASBITS, ALLBITS, FRM, ZEROPOSN, ARRAY, DFLTFRM) \
    lclcas->CASBITS &= (~ALLBITS);       /* clear bits */ \
    lclcas->CASBITS |=  \
      orbit_getBitByFrm( lclcas->FRM, ZEROPOSN, ARRAY); \
    if ( !(lclcas->CASBITS & ALLBITS)) { \
      lclcas->FRM = DFLTFRM; \
      lclcas->CASBITS |= orbit_getBitByFrm( DFLTFRM, ZEROPOSN, ARRAY); \
    }

  case CASES_NOPARAMS:       /* no parameters, allocate space for first label */
    { char tmplbl[255];                         /* using strdup(), free later */
      sprintf( tmplbl, "%s ID", orbit_CAS_TypeToName( lclcas->_type));
      fldMenu->lbl_txt = REALSTRDUP( tmplbl);
    }
    break;

  case CASTYPE_AUTOEXPOSE:
    MKFLDLNG( msiAutoTImDelta);
    MKFLDLNG( msiAutoFile4TIm);
    MKFLDLNG( msiAutoTImExpTime);
    MKFLDLNG( msiAutoAllowSat);
    MKFLDLNG( msiAutoTargSatDN);
    MKFLDLNG( msiAutoOvrExpFlbkTim);
    MKFLDLNG( msiAutoNoiseOffset);
    break;

  case CASTYPE_LOADFILT: {
  int counter;
    for ( counter=0; counter<8; ++counter) { 
      MKFLDLNGP( msiLoadFiltSensArray+counter);
    }
    break;
  }

  case CASTYPE_XGRSCONFIG:
    SETFRMBIT( xgrsConfBits, XGRSCONFBITall_Posn, xgrsConfPosn
             , XGRSCONFI000Posn
             , xgrsConfPosnArray, XGRSCONFI000Posn)
    SETFRMBIT( xgrsConfBits, XGRSCONFBITall_Dir, xgrsConfDir
             , XGRSCONFI000Dir
             , xgrsConfDirArray, XGRSCONFI000Dir)
    MKFLDBIT( xgrsConfBits, XGRSCONFI000Posn);
    MKFLDBIT( xgrsConfBits, XGRSCONFI000Dir);
    break;

  case CASTYPE_MSICONFIG:
    SETFRMBIT( msiConfBits, MSICONFBITall_FullIm, msiConfFullIm
             , MSICONFI000FullIm
             , msiConfFullImArray, MSICONFI000FullIm)
    SETFRMBIT( msiConfBits, MSICONFBITall_SumIm, msiConfSumIm
             , MSICONFI000SumIm
             , msiConfSumImArray, MSICONFI000SumIm)
    MKFLDBIT( msiConfBits, MSICONFI000FullIm);
    MKFLDBIT( msiConfBits, MSICONFI000SumIm);
    break;

  case CASTYPE_NISCONFIG:
    SETFRMBIT( nisConfBits, NISCONFBITall_SlitDriveSel, nisConfSlitDriveSel
             , NISCONFI000SlitDriveSel
             , nisConfSlitDriveSelArray, NISCONFI000SlitDriveSel)
    SETFRMBIT( nisConfBits, NISCONFBITall_ShutDriveSel, nisConfShutDriveSel
             , NISCONFI000ShutDriveSel
             , nisConfShutDriveSelArray, NISCONFI000ShutDriveSel)
    SETFRMBIT( nisConfBits, NISCONFBITall_VoltSet, nisConfVoltSet
             , NISCONFI000VoltSet
             , nisConfVoltSetArray, NISCONFI000VoltSet)
    SETFRMBIT( nisConfBits, NISCONFBITall_ScanDriveSel, nisConfScanDriveSel
             , NISCONFI000ScanDriveSel
             , nisConfScanDriveSelArray, NISCONFI000ScanDriveSel)
    MKFLDBIT( nisConfBits, NISCONFI000SlitDriveSel);
    MKFLDBIT( nisConfBits, NISCONFI000ShutDriveSel);
    MKFLDBIT( nisConfBits, NISCONFI000VoltSet);
    MKFLDBIT( nisConfBits, NISCONFI000ScanDriveSel);
    break;

  /*******************************************/
  case CASTYPE_MSITR:

    /* start seq imaging, msi sequence(s) to use, delta(s), type(s), iter */

    fm->type = FLD_LNG; 
    fm->fld_lng = lclcas->msiRepDelay; 
    NEXTFM;
    fm->type = FLD_LNG8;
    fm->subtype = fm->fld_txt_maxlen = 3;
    fm->fld_lng = lclcas->msiRepSeq;
    NEXTFM;
    fm->type = FLD_LNG8;
    fm->subtype = fm->fld_txt_maxlen = 3;
    fm->fld_lng = lclcas->msiRepDel;
    NEXTFM;
    fm->type = FLD_DBL8;
    fm->subtype = fm->fld_txt_maxlen = 3;
    fm->fld_dbl = lclcas->msiRepImageTypeDblTR;
    NEXTFM;
    fm->type = FLD_LNG;
    fm->fld_lng = lclcas->msiRepIter;
    NEXTFM;

    break;

  /*******************************************/
  case CASTYPE_MSISR:
  case CASTYPE_MSIDR:
  case CASTYPE_MSIDSR:

    /* convert from _imageType to _bits */

    /* SETFRMBIT( msiRepBits, MSIREPBITallType, msiRepImageType, MSIREPI000Type
    /*         , msiRepTypeArray, MSIREPIfulType)
    /**/

    /* start seq imaging:  one value for SR & DR; two values for DSR */

    if isMSIDSR( *lclcas) {
      fm->type = FLD_LNG8;
      fm->subtype = fm->fld_txt_maxlen = 2;
      lclcas->msiRepDelay[1] = lclcas->msiRepDelay[0] + lclcas->msiRepSeq2Start;
    } else {
      fm->type = FLD_LNG;
    }
    fm->fld_lng = lclcas->msiRepDelay;
    NEXTFM;

    /* msi sequence(s) to use:  one for SR; two for DR & DSR */

    if isMSISR( *lclcas) {
      fm->type = FLD_LNG;
    } else {
      fm->type = FLD_LNG8;
      fm->subtype = fm->fld_txt_maxlen = 2;
    }
    fm->fld_lng = lclcas->msiRepSeq;
    NEXTFM;

    /* delta(s):  one value for SR; two values for DR & DSR */

    if isMSISR( *lclcas) {
      fm->type = FLD_LNG;
    } else {
      fm->type = FLD_LNG8;
      fm->subtype = fm->fld_txt_maxlen = 2;
    }
    fm->fld_lng = lclcas->msiRepDel;
    NEXTFM;

    /* iterations:  one value for SR & DR, two values for DSR */

    if isMSIDSR( *lclcas) {
      fm->type = FLD_LNG8;
      fm->subtype = fm->fld_txt_maxlen = 2;
    } else {
      fm->type = FLD_LNG;
    }
    fm->fld_lng = lclcas->msiRepIter;
    NEXTFM;

    /* DISABLED:  see start seq imaging above */
    /* DSR:  1st seq 2 start after seq 1 */

    /* if isMSIDSR( *lclcas) {
    /*   fm->type = FLD_LNG;
    /*   fm->fld_lng = &lclcas->msiRepSeq2Start;
    /*   NEXTFM;
    /* }
    /**/

    /* Image type */

    fm->type = FLD_DBL; fm->fld_dbl = &lclcas->msiRepImageTypeDbl;
    NEXTFM;

    /* old code, image type used to be in the bits */
    /* fm->type = FLD_BIT; fm->fld_lng = &lclcas->msiRepBits;
    /* fm->fld_lowbit = MSIREPI000Type;
    /* NEXTFM;
    /**/

    break;

  case CASTYPE_NISSU2:
  case CASTYPE_NISSU1:

    SETFRMBIT( nisSuBits, NISSUBITallAper, nisSuAperture
             , NISSUI000Aper
             , nisSuAperArray, NISSUIslitAper)
    SETFRMBIT( nisSuBits, NISSUBITallGain, nisSuGain
             , NISSUI000Gain
             , nisSuGainArray, NISSUI000Gain)
    if ( lclcas->_type == CASTYPE_NISSU2) {
      SETFRMBIT( nisSuBits, NISSUBITall_Dir, nisSuStepDir
               , NISSUI000Dir
               , nisSuStepDirArray, NISSUI000Dir)
    }

    /* Rep delay (START_SEQUENCING) for SU1 only */

    if ( lclcas->_type == CASTYPE_NISSU1 ) {
      MKFLDLNG( nisSuRepDelay);
    } else { lclcas->nisSuRepDelay = 0; }

    MKFLDLNG( nisSuSeq);
    MKFLDLNG( nisSuMirrorPosn);
    MKFLDBIT( nisSuBits, NISSUI000Aper);
    MKFLDBIT( nisSuBits, NISSUI000Gain);
    MKFLDLNG( nisSuPpss);
    if ( lclcas->_type == CASTYPE_NISSU2) {
      MKFLDBIT( nisSuBits, NISSUI000Dir);
    } else {
      MKFLDLNG( nisSuNoss);
    }

    break;

  /*******************************************/
  case CASTYPE_NISEX:
  case CASTYPE_NISSR:
  case CASTYPE_NISDR:

    /* start sequencing
     * setup              - ***N.B. not for NISEX
     * seq # 1
     * setup mirror posn  - not visible, duplicate from seq # 1
     * setup aperture     -  "
     * setup gain         -  "
     * seq # 1 mirror posn
     * seq # 1 aperture
     * seq # 1 gain         NISEX stops here
     * seq # 1 delta        NISDR:  delta start seq 1 to seq 2
     * seq # 2              NISDR only
     * seq # 2 mirror posn  NISDR only
     * seq # 2 aperture     NISDR only
     * seq # 2 gain         NISDR only
     * seq # 2 delta        NISDR only
     * iterations
     */

    /* set 2 bits for NISDR, otherwise set 1 */

    numBits = isNISDR( *lclcas) ? 2 : 1;
    for ( iBit=0; iBit < numBits; ++iBit) {

      /* allow NOCHANGE for NISDR only */

      SETFRMBIT( nisRepBits[iBit]
               , ((numBits==1) ? NISREPBITallAper : NISREPBITallAperNoch)
               , nisRepAperture[iBit]
               , (numBits==1) ? NISREPI000Aper : NISREPI000AperNoch
               , (numBits==1) ? nisRepAperArray : nisRepAperNochArray
               , NISREPIslitAper)

      SETFRMBIT( nisRepBits[iBit]
               , ((numBits==1) ? NISREPBITallGain : NISREPBITallGainNoch)
               , nisRepGain[iBit]
               , (numBits==1) ? NISREPI000Gain : NISREPI000GainNoch
               , (numBits==1) ? nisRepGainArray : nisRepGainNochArray
               , NISREPI000Gain)

    } /* for iBit<numBits */

    /* start sequencing */
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisRepDelay; NEXTFM;

    /* NIS EXECUTE - seq #, mirror position, aperture, gain */
    if isNISEX( *lclcas) {
      fm->type = FLD_LNG; fm->fld_lng = lclcas->nisRepSeq; NEXTFM;
      fm->type = FLD_LNG; fm->fld_lng = lclcas->nisRepMirrorPosn; NEXTFM;
      fm->type = FLD_BIT; fm->fld_lng = lclcas->nisRepBits;
      fm->fld_lowbit = NISREPI000Aper;
      NEXTFM;
      fm->type = FLD_BIT; fm->fld_lng = lclcas->nisRepBits;
      fm->fld_lowbit = NISREPI000Gain;
      NEXTFM;
      break;
    }

    /* NIS SINGLE & DOUBLE REPEAT - use setup variables */

    if isNISSR( *lclcas) {

      /* - for single repeat, copy seq 1 mirror position & bits into 
       *   setup variables, copy back out in callback (see above)
       */
      lclcas->nisRepMirrorPosnSetup = lclcas->nisRepMirrorPosn[0];
      lclcas->nisRepBitsSetup = lclcas->nisRepBits[0];

    } else {

      /* - for double repeat, use setup variables to set setup bits */

      SETFRMBIT( nisRepBitsSetup, NISREPBITallGain, nisRepGainSetup
               , NISREPI000Gain
               , nisRepGainArray, NISREPI000Gain)

      SETFRMBIT( nisRepBitsSetup, NISREPBITallAper, nisRepApertureSetup
               , NISREPI000Aper
               , nisRepAperArray, NISREPIslitAper)
    }

    /* SETUP - time, mirror posn, aperture, gain */
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisRepSetup; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisRepMirrorPosnSetup; NEXTFM;
    fm->type = FLD_BIT; fm->fld_lng = &lclcas->nisRepBitsSetup;
                        fm->fld_lowbit = NISREPI000Aper;
    NEXTFM;
    fm->type = FLD_BIT; fm->fld_lng = &lclcas->nisRepBitsSetup;
                        fm->fld_lowbit = NISREPI000Gain;
    NEXTFM;

    /* seq 1 number */
    if ( numBits == 1) {
      fm->type = FLD_LNG;
    } else {
      fm->type = FLD_LNG8;
      fm->subtype = fm->fld_txt_maxlen = 2;
    }
    fm->fld_lng = lclcas->nisRepSeq;
    NEXTFM;

    /* seq 1 mirror position, aperture & gain only needed for double repeat
     * - for single repeat, get them from *Setup
     */
    if ( numBits == 2) {

      /* Seq 1 & 2 Mirror Positions */
      fm->type = FLD_LNG8; fm->fld_lng = lclcas->nisRepMirrorPosn;
      fm->subtype = fm->fld_txt_maxlen = 2;
      NEXTFM;

      /* Seq 1 Aperture - allow for NOCHANGE */
      fm->type = FLD_BIT; fm->fld_lng = lclcas->nisRepBits;
      fm->fld_lowbit = NISREPI000AperNoch;
      NEXTFM;

      /* Seq 2 Aperture - allow for NOCHANGE */
      fm->type = FLD_BIT; fm->fld_lng = lclcas->nisRepBits+1;
      fm->fld_lowbit = NISREPI000AperNoch;
      NEXTFM;

      /* Seq 1 Gain - allow for NOCHANGE */
      fm->type = FLD_BIT; fm->fld_lng = lclcas->nisRepBits;
      fm->fld_lowbit = NISREPI000GainNoch;
      NEXTFM;

      /* Seq 2 Gain - allow for NOCHANGE */
      fm->type = FLD_BIT; fm->fld_lng = lclcas->nisRepBits+1;
      fm->fld_lowbit = NISREPI000GainNoch;
      NEXTFM;
    }

    /* seq 1 interval */
    if ( numBits == 1) {
      fm->type = FLD_LNG;
    } else {
      fm->type = FLD_LNG8;
      fm->subtype = fm->fld_txt_maxlen = 2;
    }
    fm->fld_lng = lclcas->nisRepDel;
    NEXTFM;

    /* repeat # iterations */
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisRepIter; NEXTFM;

    break;

  /*******************************************/
  case CASTYPE_NISSEQDEF:

    /* Seq#, numscan,secbtwscan,numobs,calint,spec/obs,numrests,numdarks,
     * mirrorsteps, secbtwobs
     */
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisIdNum; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisNumScans; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisSecBtwScan; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisNumObs; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisCalInterval; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisSecPerObs; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisNumRests; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisNumDarks; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisStepMirror; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->nisSecBtwObs; NEXTFM;

    break;

  /*******************************************/
  case CASTYPE_SHOOT:

    icount = lclcas->msiShootCount;

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->msiShootCount; NEXTFM;

    fm->type = FLD_LNG8; fm->fld_lng = lclcas->msiShootSeq;
    fm->subtype = (icount>6) ? 6 : icount;
    fm->fld_txt_maxlen = 6;
    NEXTFM;
    fm->type = FLD_LNG8; fm->fld_lng = lclcas->msiShootSeq+6;
    fm->subtype = (icount>6) ? (icount-6) : 0;
    fm->fld_txt_maxlen = 6;
    NEXTFM;

    fm->type = FLD_LNG8; fm->fld_lng = lclcas->msiShootDel;
    fm->subtype = (icount>6) ? 6 : icount;
    fm->fld_txt_maxlen = 6;
    NEXTFM;
    fm->type = FLD_LNG8; fm->fld_lng = lclcas->msiShootDel+6;
    fm->subtype = (icount>6) ? (icount-6) : 0;
    fm->fld_txt_maxlen = 6;
    NEXTFM;

    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->msiShootImageTypeDbl;
    fm->subtype = (icount>6) ? 6 : icount;
    fm->fld_txt_maxlen = 6;
    NEXTFM;
    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->msiShootImageTypeDbl+6;
    fm->subtype = (icount>6) ? (icount-6) : 0;
    fm->fld_txt_maxlen = 6;
    NEXTFM;

    break;

  /*******************************************/
  case CASTYPE_MSISEQDEF:

    SETFRMBIT( msiSeqDefBits, MSISEQDEFBITallCmpTbl, msiSeqDefCmpTbl
             , MSISEQDEFI000CmpTbl
             , msiSeqDefCmpTblArray, MSISEQDEFInonCmpTbl)

    SETFRMBIT( msiSeqDefBits, MSISEQDEFBITallCmpAlg, msiSeqDefCmpAlg
             , MSISEQDEFI000CmpAlg
             , msiSeqDefCmpAlgArray, MSISEQDEFInonCmpAlg)

    SETFRMBIT( msiSeqDefBits, MSISEQDEFBITallDpcm, msiSeqDefDpcm
             , MSISEQDEFI000Dpcm
             , msiSeqDefDpcmArray, MSISEQDEFIonDpcm)

    SETFRMBIT( msiSeqDefBits, MSISEQDEFBITallMode, msiSeqDefMode
             , MSISEQDEFI000Mode
             , msiSeqDefModeArray, MSISEQDEFIautMode)

    /* "MSISEQDEF Sequence # (1-30)" */
    /* "MSISEQDEF # of Images" */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->msiSeqDefIdNum;
    NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->msiSeqDefNumImages;
    NEXTFM;

    /* "MSISEQDEF Interval, s" */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->msiSeqDefInterval;
    NEXTFM;

    /* "MSISEQDEF Comp Table\0-\01\02\03\04\05\06\07\0" */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->msiSeqDefBits;
    fm->fld_lowbit = MSISEQDEFI000CmpTbl;
    NEXTFM;

    /* "MSISEQDEF Flag DPCM\0On\0Off\0" */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->msiSeqDefBits;
    fm->fld_lowbit = MSISEQDEFI000Dpcm;
    NEXTFM;

    /* "MSISEQDEF Comp alg\0None\0Fast\0Rice\0" */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->msiSeqDefBits;
    fm->fld_lowbit = MSISEQDEFI000CmpAlg;
    NEXTFM;

    /* "MSISEQDEF Pixels (1-16)" */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->msiSeqDefPixels;
    NEXTFM;

    /* "MSISEQDEF Mode\0Auto\0Man\0" */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->msiSeqDefBits;
    fm->fld_lowbit = MSISEQDEFI000Mode;
    NEXTFM;

    /* "MSISEQDEF Images' durations, ms" */
    /* "MSISEQDEF Images' filters" */

    fm->type = FLD_LNG8; fm->fld_lng = lclcas->msiSeqDefMsecExp;
    fm->subtype = lclcas->msiSeqDefNumImages;
    fm->fld_txt_maxlen = 8;
    NEXTFM;

    fm->type = FLD_LNG8; fm->fld_lng = lclcas->msiSeqDefFilt;
    fm->subtype = lclcas->msiSeqDefNumImages;
    fm->fld_txt_maxlen = 8;
    NEXTFM;

    break;

  /*******************************************/
  case CASTYPE_DS40XGRS:
  case CASTYPE_DS40:
  case CASTYPE_DS40FULL:

    /* convert from aimpt frame type to bit
     * - _...Bits only used in this fldMenu, rest of app uses _...FrmType/Sel
     */
    SETFRMBIT( ds40Bits, DS40BITallSys, ds40AimptFrmType, DS40I000Sys
             , ds40SysArray, Inadir)

    /* slew duration */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->ds40SlewDuration;
    NEXTFM;

    /* aimpoint coordinate system */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->ds40Bits;
    fm->fld_lowbit = DS40I000Sys;
    NEXTFM;

    /* aimpoint vector */

    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds40AimptVec;
    fm->subtype = fm->fld_txt_maxlen = 3;
    NEXTFM;

    if isDS40XGRS(*lclcas) break;                /* done for xgrs_point_quick */

    /* convert from frame type to bit for AimptSelect & RollFrmType */

    SETFRMBIT( ds40Bits, DS40BITallSel, ds40AimptSelect, DS40I000Sel
             , ds40SelArray, Isci)
    SETFRMBIT( ds40Bits, DS40BITallSysRoll, ds40RollFrmType, DS40I000SysRoll
             , ds40SysRollArray, Ieci)

    /* FULL DS40 only - spacecraft roll vector */

    if isDS40FULL(*lclcas) {
      fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds40ScRollVec;
      fm->subtype = fm->fld_txt_maxlen = 3;
      NEXTFM;
    }

    /* aimpoint select/shortcut */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->ds40Bits;
    fm->fld_lowbit = DS40I000Sel;
    NEXTFM;

    /* FULL DS40 only - roll reference vector aimpoint */

    if isDS40FULL(*lclcas) {
      fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds40RollRefVec;
      fm->subtype = fm->fld_txt_maxlen = 3;
      NEXTFM;
    }

    /* virtual boresight vector */

    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds40VbVec;
    fm->subtype = fm->fld_txt_maxlen = 3;
    NEXTFM;

    /* FULL DS40 only - roll algorithm */

    if isDS40FULL(*lclcas) {
      fm->type = FLD_BIT; fm->fld_lng = &lclcas->ds40Bits;
      fm->fld_lowbit = DS40I000SysRoll;
      NEXTFM;
    }

    break;

  /****************************************************/
  case CASTYPE_DS56:

    /* convert ->_reUse & ->_frmType to bit 
     * - ds56Bits only used in this fldMenu 
     * - default for bad _frmType is Inadir
     */
    SETFRMBIT( ds56Bits, DS56BITallREUSE, ds56ReUse, DS56I000reUSE
             , ds56ReUseArray, DS56IreUSE_not)
    SETFRMBIT( ds56Bits, DS56BITallSys, ds56FrmType, DS56I000Sys
             , ds56SysArray, Inadir)

    /* RE_USE_PREV_SCAN */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->ds56Bits;
    fm->fld_lowbit = DS56I000reUSE;
    NEXTFM;

    /* DS56 scan duration */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->ds56ScanDur;
    NEXTFM;

    /* bits:  coord sys, change, pause after */

    fm->type = FLD_BIT; fm->fld_lng = &lclcas->ds56Bits;
    fm->fld_lowbit = DS56I000Sys;
    NEXTFM;

    fm->type = FLD_BITS; fm->fld_lng = &lclcas->ds56Bits;
    fm->fld_lowbit = DS56IxCHG;
    NEXTFM;

    fm->type = FLD_BITS; fm->fld_lng = &lclcas->ds56Bits;
    fm->fld_lowbit = DS56IxPAUSE;
    NEXTFM;

    /* VECs:  rate duration, ds56 rate, pause duration */

    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds56RatePauseDur[0];
    fm->subtype = fm->fld_txt_maxlen = 3;
    NEXTFM;

    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds56RateVec;
    fm->subtype = fm->fld_txt_maxlen = 3;
    NEXTFM;

    fm->type = FLD_DBL8; fm->fld_dbl = lclcas->ds56RatePauseDur[1];
    fm->subtype = fm->fld_txt_maxlen = 3;
    NEXTFM;
    break;

  case CASTYPE_REQ:

    lclcas->reqAddTop = 0;   /* init # sub-CASes to add to 0 */
    lclcas->reqAddCasDef = 0;
    lclcas->reqAddCasOpnavBits = 0;

    /* long text field, sliders not working properly under linux, at least */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->reqAddTop; NEXTFM;

    fm->type = FLD_TXTFILEREAD;
    fm->fld_txt = lclcas->reqAddCasTemplate;
    fm->fld_txt_maxlen = MAXLENFILNAM;
    fm->fld_txt0 = txtFn;
    strncpy( txtFn, fm->fld_txt, MAXLENFILNAM);
    txtFn += MAXLENFILNAM; txtFn[-1] = '\0';
    NEXTFM;

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->reqAddCasDef; NEXTFM;

    fm->type = FLD_TXTFILEREAD;
    fm->fld_txt = lclcas->reqAddCasDefTemplate;
    fm->fld_txt_maxlen = MAXLENFILNAM;
    fm->fld_txt0 = txtFn;
    strncpy( txtFn, fm->fld_txt, MAXLENFILNAM);
    txtFn += MAXLENFILNAM; txtFn[-1] = '\0';
    NEXTFM;

    MKFLDBITS( reqAddCasOpnavBits, OPNAVTYPE_A);
    MKFLDBITS( reqAddCasOpnavBits, OPNAVTYPE_CT);
    MKFLDBITS( reqAddCasOpnavBits, OPNAVTYPE_F);

    fm->type = FLD_TXTFILEREAD;
    fm->fld_txt = lclcas->reqAddCasOpnavTemplate;
    fm->fld_txt_maxlen = MAXLENFILNAM;
    fm->fld_txt0 = txtFn;
    strncpy( txtFn, fm->fld_txt, MAXLENFILNAM);
    txtFn += MAXLENFILNAM; txtFn[-1] = '\0';
    NEXTFM;

    break;

  case CASTYPE_CAS:

    /* bits to determine which fields are used */

    MKFLDBITS( casBits, CASTYPE_DS40);
    MKFLDBITS( casBits, CASTYPE_SHOOT);
    MKFLDBITS( casBits, CASTYPE_NISEX);
    MKFLDBITS( casBits, CASTYPE_NISCAL);
    /* MKFLDBITS( casBits, CASTYPE_NISBUFFLUSH); */
    MKFLDBITS( casBits, CASTYPE_AUTOEXPOSE);
    MKFLDBITS( casBits, CASTYPE_XGRSRELATT);

    /* load the text field pointers & the fldMenu structures */

    for ( ii=0; ii<CASTYPE_COUNT; ++ii) {
      if ( lclcas->casArgListPtrs[ii]) {
        fm->type = FLD_TXT;
        fm->fld_txt = lclcas->casArgListPtrs[ii];
        strncpy( fm->fld_txt0, fm->fld_txt, CASTOP_MAXARGSLEN);
        fm->fld_txt_maxlen = (CASTOP_MAXARGSLEN<lclcas->casArgListSize[ii]) 
                           ? CASTOP_MAXARGSLEN : lclcas->casArgListSize[ii];
        NEXTFM;
      }
    }
    break;

  case CASTYPE_CASDEF:

    lclcas->casDefAddMsi = 0;   /* init # sub-CASes to add to 0 */
    lclcas->casDefAddNis = 0;

    /* long text field, sliders not working properly under linux, at least */

    fm->type = FLD_LNG; fm->fld_lng = &lclcas->casDefAddMsi; NEXTFM;
    fm->type = FLD_LNG; fm->fld_lng = &lclcas->casDefAddNis; NEXTFM;

    /* load the text field pointers & the fldMenu structures */

    for ( ii=0; ii<MAXNUMSEQDEF; ++ii) {
      if ( lclcas->casDefListPtrs[ii]) {
        fm->type = FLD_TXT;
        fm->fld_txt = lclcas->casDefListPtrs[ii];
        strncpy( fm->fld_txt0, fm->fld_txt, CASTOP_MAXARGSLEN);
        fm->fld_txt_maxlen = (CASTOP_MAXARGSLEN<lclcas->casDefListSize[ii]) 
                           ? CASTOP_MAXARGSLEN : lclcas->casDefListSize[ii];
        NEXTFM;
      }
    }

    break;

  case CASTYPE_CASOPNAV:

    { char tmplbl[255];         /* alloc space for first label using strdup() */
      sprintf( tmplbl, "%s ID", orbit_CAS_FragToName( lclcas));
      fldMenu->lbl_txt = REALSTRDUP( tmplbl);
    }

    /* for each argument type
     * - set fm->lbl_txt from casOpnav*Source
     *   - sprintf to fm->lbl_txt if necessary
     * - set fm->type, ->fld_* pointer, ->subtype, ->fld_lowbit, &c
     */
    subType = lclcas->opnavSubType;
    for ( iArg=0; iArg<OPNAVARG_COUNT; ++iArg) {
    long di, iPass;
    char **lbl0;
      di = opnavArgFragCount[subType][iArg];
      if ( di > 0 ) switch (iArg) {

      case OPNAVARG_PRIO:
        fm->lbl_txt = casOpnavPrioSource[0];
        lclcas->opnavBits[0] = 1L << ((lclcas->opnavPrio[0]) ? 1 : 0);
        MKFLDBIT( opnavBits[0], 0);
        break;

      case OPNAVARG_SLEWDUR:
        if ( di == 1) {
          fm->lbl_txt = casOpnavSlewDurSource[0];
          MKFLDLNGP( opnavSlewDur);
        } else {
        long iOfs = 0;
          while ( di > 0) {
            sprintf( fm->lbl_txt=STRDUP(casOpnavSlewDurSource[(iOfs/6)+1])
                   , CASOPNAV_SLEWDURFMT
                   , (iOfs + 1), iOfs + ((di<6) ? di : 6));
            fm->type = FLD_LNG8; fm->fld_lng = lclcas->opnavSlewDur+iOfs;
            fm->subtype = (di<6) ? di : 6;
            fm->fld_txt_maxlen = fm->subtype;
            di -= fm->fld_txt_maxlen;
            iOfs += fm->fld_txt_maxlen;
            NEXTFM;
          }
        }
        break;

      case OPNAVARG_MISCINT:
        if ( di == 1) {
          fm->lbl_txt = casOpnavDeltaK[0];
          MKFLDLNGP( opnavMiscInt);
        }
        break;

      case OPNAVARG_STARTNTHSLEW:
        if ( di == 1) {
          fm->lbl_txt = casOpnavStartNthSlewSource[0];
          MKFLDLNGP( opnavStartNthSlew);
        }
        break;

      case OPNAVARG_DS40AIMXYZ:
      case OPNAVARG_DS40AIMSHORT:
      case OPNAVARG_DS40BOREXYZ: /* these three handled by _DS40COORDSYS */
        break;
      case OPNAVARG_DS40COORDSYS:            /* this must have the largest di */
        lbl0 = (di==1) ? casOpnavDs40Source : (casOpnavDs40Source+4);
        for ( iPass=0; iPass<di; ++iPass) {

          SETFRMBIT( opnavDs40[iPass]->ds40Bits, DS40BITallSys /*AimptCoordSys*/
                   , opnavDs40[iPass]->ds40AimptFrmType
                   , DS40I000Sys, ds40SysArray, Inadir)
          fm->lbl_txt = *(lbl0++);
          MKFLDBIT( opnavDs40[iPass]->ds40Bits, DS40I000Sys);

          fm->lbl_txt = *(lbl0++);                         /* Aimpoint Vector */
          fm->type = FLD_DBL8;
          fm->fld_dbl = lclcas->opnavDs40[iPass]->ds40AimptVec;
          fm->subtype = fm->fld_txt_maxlen = 3;
          NEXTFM;

          fm->lbl_txt = *(lbl0++);           /* Aimpt shortcut (aimpt select) */

                     /* ***N.B. prev statement must be outside next if clause */

          if ( opnavArgFragCount[subType][OPNAVARG_DS40AIMSHORT] > iPass) {
            SETFRMBIT( opnavDs40[iPass]->ds40Bits, DS40BITallSel
                     , opnavDs40[iPass]->ds40AimptSelect
                     , DS40I000Sel, ds40SelArray, Isci)
            MKFLDBIT( opnavDs40[iPass]->ds40Bits, DS40I000Sel);
          }

          fm->lbl_txt = *(lbl0++);                        /* Boresight Vector */
          fm->type = FLD_DBL8;
          fm->fld_dbl = lclcas->opnavDs40[iPass]->ds40VbVec;
          fm->subtype = fm->fld_txt_maxlen = 3;
          NEXTFM;

        } /* for iPass */

        break; /* case OPNAVARG_DS40COORDSYS */


      case OPNAVARG_DS56:           /* copied from CASTYPE_DS56 w/o REUSESCAN */

        lbl0 = (di==1) ? casOpnavDs56Source : casOpnavDs56Source+7;
        /* convert ->_frmType to bit 
         * - ds56Bits only used in this fldMenu 
         * - default for bad _frmType is Inadir
         */
        for ( iPass=0; iPass<di; ++iPass) {
          SETFRMBIT( opnavDs56[iPass]->ds56Bits, DS56BITallSys
                   , opnavDs56[iPass]->ds56FrmType, DS56I000Sys
                   , ds56SysArray, Inadir)


          /* DS56 scan duration */

          fm->lbl_txt = *(lbl0++);
          MKFLDLNG( opnavDs56[iPass]->ds56ScanDur);

          /* bits:  coord sys, change, pause after */

          fm->lbl_txt = *(lbl0++);
          MKFLDBIT( opnavDs56[iPass]->ds56Bits, DS56I000Sys);
          fm->lbl_txt = *(lbl0++);
          MKFLDBITS( opnavDs56[iPass]->ds56Bits, DS56IxCHG);
          fm->lbl_txt = *(lbl0++);
          MKFLDBITS( opnavDs56[iPass]->ds56Bits, DS56IxPAUSE);

          /* VECs:  rate duration, ds56 rate, pause duration */

          fm->lbl_txt = *(lbl0++);
          fm->type = FLD_DBL8;
          fm->fld_dbl = lclcas->opnavDs56[iPass]->ds56RatePauseDur[0];
          fm->subtype = fm->fld_txt_maxlen = 3;
          NEXTFM;

          fm->lbl_txt = *(lbl0++);
          fm->type = FLD_DBL8;
          fm->fld_dbl = lclcas->opnavDs56[iPass]->ds56RateVec;
          fm->subtype = fm->fld_txt_maxlen = 3;
          NEXTFM;

          fm->lbl_txt = *(lbl0++);
          fm->type = FLD_DBL8;
          fm->fld_dbl = lclcas->opnavDs56[iPass]->ds56RatePauseDur[1];
          fm->subtype = fm->fld_txt_maxlen = 3;
          NEXTFM;
        }

        break;

      case OPNAVARG_FRAMES:
        for ( iPass=0; iPass<di; ++iPass) {
          fm->lbl_txt = casOpnavNFramesSource[iPass + ((di>1)?1:0)];
          MKFLDLNG( opnavShoot[iPass]->msiShootCount);
        }
        break;

#     define WCH(IT,ID) ((iArg==OPNAVARG_IM_TYPE) ? (IT) : (ID))

      case OPNAVARG_IM_TYPE:
      case OPNAVARG_IM_DELTA:
        if ( di == 1) {
          fm->lbl_txt = WCH(casOpnavImgTypSource[0],casOpnavImgDeltaSource[0]);
          fm->type = WCH(FLD_DBL,FLD_LNG);
          WCH( fm->fld_dbl = lclcas->opnavShoot[0]->msiShootImageTypeDbl
             , (double *) (fm->fld_lng = lclcas->opnavImDeltas)
             );
          NEXTFM;
        } else {
        long iOfs = 0;
          while ( di > 0) {
            fm->lbl_txt=STRDUP(WCH( casOpnavImgTypSource
                                  , casOpnavImgDeltaSource)[(iOfs/6)+1]);
            sprintf( fm->lbl_txt
                   , WCH( CASOPNAV_IMGTYPFMT, CASOPNAV_IMGDELTAFMT)
                   , ((iOfs%12) + 1), (iOfs%12) + ((di<6) ? di : 6));
            fm->type = WCH(FLD_DBL8,FLD_LNG8);
            WCH( fm->fld_dbl= lclcas->opnavShoot[0]->msiShootImageTypeDbl+iOfs
               , (double *)(fm->fld_lng=lclcas->opnavImDeltas+iOfs)
               );
            fm->fld_txt_maxlen = (di<6) ? di : 6;

            switch ( subType) {

            /* for OPNAVTYPE_DOUBLE_K get fm->subtype from cas ...
             * and store imtyp for 2nd shoot in 1st shoot's imtyp array
             */
            case OPNAVTYPE_DOUBLE_K:
              if ( iArg==OPNAVARG_IM_TYPE) {
                /***************************
                 * DOUBLE_K:  image type funny business
                 * - store 2nd shoot's imtyp in 1st shoot's array
                 */
                lclcas->opnavShoot[0]->msiShootImageTypeDbl[1] =
                  lclcas->opnavShoot[1]->msiShootImageTypeDbl[0];

                fm->subtype = di - iOfs;
                if ( fm->subtype > 6) fm->subtype = 6;
                else if (fm->subtype < 0) fm->subtype = 0;

                break;
              }

            /* else drop through to single _K for IM_DELTA */

            case OPNAVTYPE_K: /* for OPNAVTYPE_K get fm->subtype from cas ... */
              /*************************
               * ***N.B. this will only work if the max number of images 
               *         per shoot frag (currently 12) is a multiple of the 
               *         max number of entries of image delta or image type
               */
              fm->subtype = lclcas->opnavShoot[iOfs/12]->msiShootCount
                          - (iOfs%12) ;

              if ( fm->subtype > 6) fm->subtype = 6;
              else if (fm->subtype < 0) fm->subtype = 0;
              break;

            default:                      /* ... else from fm->fld_txt_maxlen */
              fm->subtype = fm->fld_txt_maxlen;
              break;

            } /* switch subType */
            di -= fm->fld_txt_maxlen;
            iOfs += fm->fld_txt_maxlen;
            NEXTFM;
          }
        }

        break; /* case OPNAVARG_IM_TYPE/_IM_DELTA */

      default:
        break;
      } /* switch iArg */

    } /* for iArg<OPNAVARG_COUNT */

    break; /* case CASTYPE_CASOPNAV */

  /*************************************************/
  default:
    free( fldMenu);
    STRDUPRTN;
    break;
  } /* switch lclcas->_type */

  /* last fm struct:  add cancel callback & filetype */
  fm->type = FLD_END;
  fm->client_call = orbitgui_CAS_fldMenu_cancel_CB;
  strcpy( fm->fld_txt0, "*.");
  strcat( fm->fld_txt0, WHOAMI( lclcas->_type));

  /* add ok callback & CAS pointer to first fm struct UNLESS
   * lclcas is child of OPNAV CAS, use NULL function to disable OK button
   */
  fldMenu->client_call = 
   (!lclcas->top) ? orbitgui_CAS_fldMenu_ok_CB
     : !isCASOPNAV(*lclcas->top) ? orbitgui_CAS_fldMenu_ok_CB
                                 : orbitgui_fldmenu_readOnlyFlagFunction;
  fldMenu->client_data = lclcas;

  /* save name in case it changes so we know to update listw */
  strcpy( lclcas->_savename, lclcas->_name);

  orbitgui_create_fldmenu_dialog( buttonForm, WHOAMI( lclcas->_type), fldMenu);

  STRDUPLIST;
  return;          /* it is the responsibility of _ok/cancel_CB to free these */
} /* orbitgui_create_CAS_fldMenu() */

/**********************************************************/
/* wrapper to call orbit_CAS_sasf2CAS() with addToMiscPtr */

void
orbitgui_CAS_sasf2CAS( char *c, void *v) {
  orbit_CAS_sasf2CAS( c, v, orbitgui_CAS_addToMiscPtr);
  return;
}

/**************************************************************/
/* create routines to respond to cases LOADSASF_BUTTON: below */

#define LOADSASF_CB orbitgui_CAS_sasf2CAS_CB

PICKFILE_KEEPUP_SETUP( LOADSASF_CB, "Select OPNAV SASF file to read", "*.sasf"
                     , orbitgui_CAS_sasf2CAS_ok_CB, orbitgui_CAS_sasf2CAS
                     , PF_READ)

/**********************************************************/
/* wrapper to call orbit_CAS_readFile() with addToMiscPtr */

void
orbitgui_CAS_readFileWrap( char *c, void *v) {
  orbit_CAS_readFile( c, v, orbitgui_CAS_addToMiscPtr);
  return;
}

/**********************************************************/
/* wrapper to call orbit_CAS_readFile() with addToMiscPtr */

void
orbitgui_CAS_readNRunFileWrap( char *c, void *v) {
OCWS *CASWStuff = (OCWS *) v;
void orbitgui_CASmenu_widget_CB(Widget, XtPointer, XtPointer);
int itemCount;

# define DELALLITEMS \
  for ( XtVaGetValues( listw , XmNitemCount, &itemCount , NULL); \
        itemCount > 0; \
        XtVaGetValues( listw , XmNitemCount, &itemCount , NULL)) { \
    XmListSelectPos( listw, 1, False); \
    orbitgui_CASmenu_widget_CB( topshell, (XtPointer) REMOVECAS_BUTTON, (XtPointer) 0); \
  }

  DELALLITEMS

  orbit_CAS_readFile( c, v, orbitgui_CAS_addToMiscPtr);

  orbitgui_CASmenu_widget_CB( topshell, (XtPointer) RUNALLCAS_BUTTON, (XtPointer) 0);

  DELALLITEMS

  return;
}

/******************************************************************************/
/* create routines to respond to cases SAVECAS_BUTTON & LOADCAS_BUTTON: below */

#define LOADCAS_CB orbitgui_CAS_readFile

PICKFILE_KEEPUP_SETUP( LOADCAS_CB, "Select REQ file to read", "*.REQ"
                     , orbitgui_CAS_readFile_ok_CB, orbitgui_CAS_readFileWrap
                     , PF_READ)

#define LOADALLREQ_CB orbitgui_CAS_readAllReq

#ifdef _USE_TMP_H_
#include "tmp.h"
#else
PICKDIR_KEEPUP_SETUP( LOADALLREQ_CB, "Set & click Filter, click OK to read all"
                    , "*.REQ"
                    , orbitgui_CAS_readAllReq_ok_CB, orbitgui_CAS_readFileWrap
                    , PF_READ)
#endif

#define LOADNRUNALLREQ_CB orbitgui_CAS_readNRunAllReq

#ifdef _USE_TMP_H_
#include "tmp.h"
#else
PICKDIR_KEEPUP_SETUP( LOADNRUNALLREQ_CB, "Set & click Filter, click OK to read & run all"
                    , "*.REQ"
                    , orbitgui_CAS_readNRunAllReq_ok_CB, orbitgui_CAS_readNRunFileWrap
                    , PF_READ)
#endif

#define AUTOSAVEREQ_CB orbitgui_CAS_autoSaveReq

PICKDIR_SETUP( AUTOSAVEREQ_CB, "Set & click Filter, click OK to write to dir"
             , ""
             , orbitgui_CAS_autoSaveReq_ok_CB, orbit_CAS_autoSaveReq
             , PF_RETURNDIR)

#define SAVECAS_CB orbitgui_CAS_saveFile

PICKFILE_SETUP( SAVECAS_CB, "Select REQ file to write", "*.REQ"
              , orbitgui_CAS_saveFile_ok_CB, orbit_CAS_saveFile, PF_WRITE)

#define SAVESASFONEREQ_CB orbitgui_CAS_saveSasfFileOneReq

PICKFILE_SETUP( SAVESASFONEREQ_CB, "Select SASF file to write", "*.sasf"
            , orbitgui_CAS_saveSasfFileOneReq_ok_CB
            , orbit_CAS_saveSasfFileOneReq, PF_WRITE)

#define SAVESASFALLREQ_CB orbitgui_CAS_saveSasfFileAllReq

PICKFILE_SETUP( SAVESASFALLREQ_CB, "Select SASF file to write", "*.sasf"
            , orbitgui_CAS_saveSasfFileAllReq_ok_CB
            , orbit_CAS_saveSasfFileAllReq, PF_WRITE)

#define RUNCAS( CASTORUN) orbit_gen_runCas( CASTORUN, (ORBIT *) NULL, cur_item)

enum {
  NISFILL_IdoSTRETCH=0
, NISFILL_InoSTRETCH
};

#define NISFILL_I000STRETCH NISFILL_IdoSTRETCH
#define NISFILL_BITdoSTRETCH (1L<<NISFILL_IdoSTRETCH)
#define NISFILL_BITnoSTRETCH (1L<<NISFILL_InoSTRETCH)

/*****************************/
/* cancel CASFILL field menu */

int
orbitgui_createCASFILL_fldMenu_cancel_CB( CAS *cas, FLDMENU *fldMenu) {
CAS *delCas = cas;
CAS *nextCas = cas;
FLDMENU *fm = fldMenu;
CAS **selCasPtrs = (CAS **) (fldMenu+1)->client_data;

  while ( delCas=nextCas) {                      /* free linked list of CASes */
    nextCas = delCas->next;
    free( delCas);
    if ( nextCas == cas) break;                         /* stop if list loops */
  }

  /* free selected (CAS **) pointer */
  if ( selCasPtrs) free( selCasPtrs);

  free( fldMenu);                       /* free fldMenu structs + other stuff */
  return 1;                            /* - return 1 so field menu is removed */
} /* orbitgui_createCASFILL_fldMenu_cancel_CB() */

/*************************/
/* ok CASFILL field menu */

int
orbitgui_createCASFILL_fldMenu_ok_CB( CAS *cas, FLDMENU *fldMenu) {
OCWS *CASWStuff;
FLDMENU *fm;
CAS *startCAS = (CAS *) NULL;
CAS *nisExCas = (CAS *) NULL;
CAS *nisSeqDefCas = (CAS *) NULL;
CAS *msiRepCas = (CAS *) NULL;
CAS *msiSeqDefCas = (CAS *) NULL;
CAS *exeCas, *seqDefCas;
CAS *req;
/* CAS *topPrev; */
CAS *casDefCas;
CAS *casCas;
CAS *nullCasPtr = (CAS *) NULL;
int numflds, i, icount, ii, mtl, iBit, numBits, iSelCas;
long opt, *testInterval, *olapLimits;
ORBIT *scOrbit;
long m, b;
long minSDT;  /* min interval btw real msiseqdefs - used to set min ctl limit */
long startTime, iVal, scanLen, nScans, lastDelayStart, extraDelay;
long toggleSeqNum;
int lastvalid;                                        /* to make FILLIN* work */
long doStretch = 0;
long addlObsPerScan;
long secPerBtwObs;               /* time from start of 1 obs to start of next */
double firstEt;

/* get pointer to selected CAS' pointers from second fldMenu structure */
CAS **selCasPtrs = (CAS **) (fldMenu+1)->client_data;

  /* (1) check CASes */

# define FILLCLEANUP \
  if ( cas) { \
    if ( cas->next) free( cas->next); \
    free( cas); \
  } \
  if ( selCasPtrs) free( selCasPtrs); \
  free( fldMenu); \
  if ( olapLimits) free( olapLimits)

# define FILLPROGERR(I) \
  { \
    fprintf(stderr,"orbitgui_createCASFILL_fldMenu_ok_CB Program error, %s%d\n"\
                  , "code WSNBATGH-", I); \
    fflush( stderr); \
    FILLCLEANUP; \
    return 1; \
  }

  olapLimits = (long *) NULL;

  if ( !cas) FILLPROGERR(0)
  if ( !cas->next || (cas->next!=cas->prev)) FILLPROGERR(1)

  seqDefCas = cas;                            /* first CAS in list is seq def */
  exeCas = seqDefCas->next;                     /* next CAS is NISEX or SHOOT */

  CASWStuff = (OCWS *) seqDefCas->_miscPtr;            /* for cur_item, et al */
  orbitgui_return_scorbit_ci( cur_item, &scOrbit);

  switch ( seqDefCas->_type) {
  case CASTYPE_NISSEQDEF:
    if ( exeCas->_type != CASTYPE_NISEX) FILLPROGERR(2)
    break;
  case CASTYPE_MSISEQDEF:
    if ( exeCas->_type != CASTYPE_MSISR) FILLPROGERR(3)
    break;
  default:
    FILLPROGERR(4)
    break;
  }

  /* (2) post-process field menu items */

  for ( numflds = 1, fm=fldMenu; fm->type != FLD_END; ++fm, ++numflds) ;

  /* if no CASes selected, get start time & duration from field menu */

  if ( !selCasPtrs ? 1 : !*selCasPtrs ? 1 : 0) {
    /* first FLDMENU is UTC text string */
    orbit_utc2et( fldMenu->fld_txt, &exeCas->_et);          /* Sched pt (UTC) */

    /* find _duration, test interval is next */
    FINDFM( fld_lng, &exeCas->_duration, 0, 0;)
    else {
      fm++; testInterval = fm->fld_lng;
    }

  /* if at least 1 CAS selected, first field is test interval
   * exeCas->_et & _duration will be set during each pass
   * - set _et here for use in searching for real MSISEQDEF
   */

  } else {
    testInterval = fldMenu->fld_lng;
    exeCas->_et = CASSTART( selCasPtrs[0]);
  }

  /* fill out short entries
   * - overlap limits
   */
  FINDFM( fld_dbl, seqDefCas->_olapLims, 0, 0;)
  else {
    if ( fm->subtype == 1) fm->fld_dbl[1] = fm->fld_dbl[0];
  }

  /* control limits */

  FINDFM( fld_lng, seqDefCas->_ctlLims, 0, 0;)
  else {
    if ( fm->subtype == 1) fm->fld_lng[1] = fm->fld_lng[0];
  }

  /* sequence type-specific items */

  switch( seqDefCas->_type) {

  case CASTYPE_MSISEQDEF:

    /* (4-MSI) set misc values */

    /* in MSISEQDEF:
     * - get number of images in real MSISEQDEF to determine minimum interval
     * - set number of images to 2 for orbit_genOlapLimits() call
     */

    seqDefCas->msiSeqDefNumImages = 2;

    /* - control variable */

    seqDefCas->_ctlVar = &seqDefCas->msiSeqDefInterval;

    /* - find real MSISEQDEF, get number of images for later setting of 
     *   minimum control limit, default to 1
     * - adjust min control limit to at least 2 seconds per 
     *   image in real MSISEQDEF plus 1 second
     *   - adjust max limit also if necessary 
     */
    startCAS = orbitgui_get1stCASFromCurItem( cur_item);
    if ( (msiSeqDefCas=orbit_CAS_getMSISeqDefByNum( startCAS
                                                   , exeCas->_et
                                                   , exeCas->msiRepSeq[0])) ) {
      minSDT = 1 + (2 * msiSeqDefCas->msiSeqDefNumImages);
    } else {
      minSDT = 3;
    }

    if ( seqDefCas->_ctlLims[0] < minSDT) {
      seqDefCas->_ctlLims[0] = minSDT;
      if ( seqDefCas->_ctlLims[1] < seqDefCas->_ctlLims[0]) {
        seqDefCas->_ctlLims[1] = seqDefCas->_ctlLims[0];
      }
    }

    /* (5-MSI) put sequence-specific things into scOrbit->_agen */

    /* nothing to do here */

    break;

  case CASTYPE_NISSEQDEF:

    /* fill out short entry(ies) in fragments */

    FINDFM( fld_lng, exeCas->nisRepSeq, 0, 0;)
    else {
      if ( fm->subtype == 1) {
        if ( exeCas->nisRepSeq[0] < 15) 
          exeCas->nisRepSeq[1] = exeCas->nisRepSeq[0] + 1;
        else 
          exeCas->nisRepSeq[1] = exeCas->nisRepSeq[0] - 1;
      }
    }

    /* (3-NIS) convert bits to values */

    exeCas->nisRepAperture[0] =
      orbit_getFrmByBit( exeCas->nisRepBits[0] & NISREPBITallAper
                       , NISREPI000Aper, nisRepAperArray);
    exeCas->nisRepGain[0] =
      orbit_getFrmByBit( exeCas->nisRepBits[0] & NISREPBITallGain
                       , NISREPI000Gain, nisRepGainArray);

    /* (4-NIS) set misc values */

    /* - sec btw scans:  assume mirror steps at 4 Hz, add 1 second */

    seqDefCas->nisSecBtwScan = (seqDefCas->nisNumObs + 3) / 4; /* reset @ 4Hz */

    /* - mirror step size:  1 unless wide fov ("both out") */

    seqDefCas->nisStepMirror =
      (exeCas->nisRepAperture[0] == NISREPIbothoutAper) ? 2 : 1;

    /* - control variable */

    seqDefCas->_ctlVar = &seqDefCas->nisSecPerObs;

    /* (5-NIS) - put sequence-specific things into scOrbit->_agen
     *         - find whether to stretch number of NISSEQDEF obs/scan if
     *           time is available
     */
    scOrbit->_agen._StartingStep = exeCas->nisRepMirrorPosn[0];
    scOrbit->_agen._NISFOV = seqDefCas->nisStepMirror;

    doStretch = *(fldMenu+numflds-2)->fld_lng & NISFILL_BITdoSTRETCH;

    break;
  }

  /* (6) put generic items into scOrbit->_agen */

  if ( scOrbit->_agen._FrmOverlap <= scOrbit->_agen._FrmOverlap2) {
    scOrbit->_agen._FrmOverlap = seqDefCas->_olapLims[0];
    scOrbit->_agen._FrmOverlap2 = seqDefCas->_olapLims[1];
  } else {
    scOrbit->_agen._FrmOverlap = seqDefCas->_olapLims[1];
    scOrbit->_agen._FrmOverlap2 = seqDefCas->_olapLims[0];
  }

  scOrbit->_agen._seqDef = (void *) seqDefCas;

  /************************************************************/
  /* start loop through selected CASes to determine _et & _duration
   * - exit via breaks at end of loop
   * - all that changes is exeCas->_et & exeCas->_duration
   * - if no selected CASes, this will run through once
   */

  iSelCas=0;
  while ( 1) {
  CAS *selReq, *cas0, *cas1;

    /* get top parent of next selected CAS */
    selReq = !selCasPtrs
             ? (CAS *) NULL
             : !selCasPtrs[iSelCas]
               ? (CAS *) NULL
               : !selCasPtrs[iSelCas]->top
                 ? selCasPtrs[iSelCas]
                 : !selCasPtrs[iSelCas]->top->top
                   ? selCasPtrs[iSelCas]->top
                   : selCasPtrs[iSelCas]->top->top;

    /* save current REQ in selected pointer list to test later so
     * we don't repeat a REQ because it & one or more of its children are 
     * selected ***N.B. assumes selCasPtrs are sorted in same order as 
     *                  items in list
     */
    if ( selReq) selCasPtrs[iSelCas] = selReq;

    /* find first cas of REQ selReq->topNext list, else null */

    cas1 = !selReq ? nullCasPtr : !isREQ(*selReq) ? nullCasPtr :selReq->topNext;

    /* macros to find DS40 starts & ends */

#   define SUBDS40X(C,X) ((C)->casCasList[CASTYPE_DS40] \
                          ? (C)->casCasList[CASTYPE_DS40] \
                          : (C)->casCasList[CASTYPE_DS40FULL] \
                            ? (C)->casCasList[CASTYPE_DS40FULL] \
                            : (C)->casCasList[CASTYPE_DS40XGRS] \
                              ? (C)->casCasList[CASTYPE_DS40XGRS] \
                              : (X))

    /* DS40 start must be a DS40 only */

#   define SUBDS40START(C) SUBDS40X( C, (CAS *) NULL)

    /* DS40 end may be a DS40 or a RELATT */
    /* - ignore NAVRELATT, they should only be in CASOPNAV's */

#   define SUBDS40END(C) SUBDS40X( C \
                         , ((C)->casCasList[CASTYPE_MSIRELATT] \
                            ? (C)->casCasList[CASTYPE_MSIRELATT] \
                            : (C)->casCasList[CASTYPE_NISRELATT] \
                              ? (C)->casCasList[CASTYPE_NISRELATT] \
                              : (C)->casCasList[CASTYPE_XGRSRELATT] \
                                ? (C)->casCasList[CASTYPE_XGRSRELATT] \
                                : (CAS *) NULL) )

    /* loop between a DS40 start & end */

    while ( !selReq || cas1) {/* loop once if selReq=NULL or while there is */
    CAS *ds40Start, *ds40End;                  /* a topNext that has a DS40 */

      if ( selReq) {

        /* break out of above while loop ...
         * - if this is the same selReq as last time through outer loop
         * - if top parent is not a REQ
         */
        if ( iSelCas>0) if ( selReq == selCasPtrs[iSelCas-1]) break;
        if ( !isREQ( *selReq)) break;

        /* starting at current cas1, look for next CAS in selReq with DS40
         * put it into cas0, break out if none found
         */
        for ( cas0=cas1; cas0; cas0=cas0->topNext) {
          if ( (ds40Start=SUBDS40START(cas0)) ) break;
        }
        if ( !cas0) break;

        /* find next CAS after cas0 in selReq that has a DS40 or RELATT,
         * break out if none found
         */
        for ( cas1=cas0->topNext; cas1; cas1=cas1->topNext) {
          if ( (ds40End=SUBDS40END(cas1)) ) break;
        }
        if ( !cas1) break;

        /* use start times of start & end DS40/RELATT's to set 
         * exeCas->_et & ->_duration
         * - correct duration for end DS40 slew duration if it is greater than 0
         */
        exeCas->_et = CASSTART( ds40Start);
        exeCas->_duration = CASSTART( ds40End) - exeCas->_et;

        if isANYDS40( *ds40End) 
          if ( ds40End->ds40SlewDuration > 0) 
            exeCas->_duration -= ds40End->ds40SlewDuration;

      } /* if selReq */

      scOrbit->_agen._EtStart = exeCas->_et;
      scOrbit->_agen._MaxTime = exeCas->_duration;

      /* (7) generate overlap limits */

      olapLimits = orbit_genOlapLimits( scOrbit
                                      , *testInterval
                                      , seqDefCas->_ctlVar
                                      , seqDefCas->_ctlLims);

      /* - print limits out */

      fprintf( stdout, "\n");
      for ( ii=0; ii<2; ++ii) {
        for ( i=0; i<*olapLimits; ++i) {
          fprintf( stdout, "%ld %ld\n"
                         , *testInterval*i, olapLimits[1+ii+(2*i)]);
        }
        fprintf( stdout, "\n");
      }

      /* set up m & b, which determines time for one scan with this 
       * particular seqDefCas
       */

      switch ( seqDefCas->_type) {

      case CASTYPE_MSISEQDEF:
        extraDelay = 0;
        m = 1;
        b = 0;
        break;

      case CASTYPE_NISSEQDEF:
        extraDelay = 60;
        m = seqDefCas->nisNumObs;
        b = (seqDefCas->nisSecBtwObs * (seqDefCas->nisNumObs - 1))
          + seqDefCas->nisSecBtwScan;
        break;
      }

      /* create 1 REQ */

      req = orbit_CAS_new( CASTYPE_REQ);
      req->top = req->next = req->prev = req->topNext = (CAS *) NULL;
      req->_et = exeCas->_et;
      req->_miscPtr = seqDefCas->_miscPtr;
      strcpy( req->_savename, req->_name);
      orbitgui_CAS_addToMiscPtr( req);

      /* move through times in olapLimits,
       * - print out corners of iVal vs Time plot
       * - then for each (nScans by *ctlVar), create 1 CAS, CASDEF,
       *   & 1 each nisex/shoot/..., & *seqdef
       */
      lastDelayStart = extraDelay;            /* incremental CAS time offsets */
      startTime = lastDelayStart;
      iVal = 0;
      while ( startTime < exeCas->_duration ) {
      int doBreak;

        iVal = orbit_bestFitScan( olapLimits, *testInterval, startTime, m, b);
        if ( iVal == -1) break;

        scanLen = m * iVal + b;
        nScans = orbit_fitScans( olapLimits, *testInterval
                               , startTime, iVal, scanLen);

        fprintf( stdout, "%ld %ld\n", startTime, iVal);
        startTime += (nScans*scanLen) + extraDelay;
        fprintf( stdout, "%ld %ld\n", startTime, iVal);

        doBreak = 0;

        while ( startTime >= exeCas->_duration && nScans > 0) {
          nScans--;
          startTime -= scanLen;
          doBreak = 1;
        }

        if ( nScans<1) break;

#       ifdef NEWCAS_SETVALS
#       undef NEWCAS_SETVALS
#       endif

#       define NEWCAS( CASVAR, TYPE, TOP, DELAY) \
        CASVAR = orbit_CAS_new( TYPE); \
        if ( isCAS(*TOP) && !isANYTOP(*CASVAR) ) { \
          TOP->casCasList[TYPE] = CASVAR; \
        } \
        NEWCAS_SETVALS  /* set TYPE-specific values before updateTop() call */ \
        CASVAR->top = TOP; \
        orbit_CAS_updateTop( CASVAR); \
        strcpy( CASVAR->_savename, CASVAR->_name); \
        CASVAR->_miscPtr = seqDefCas->_miscPtr; \
        CASVAR->_delayStart = DELAY; \
        orbitgui_CAS_addToMiscPtr( CASVAR)


#       define NEWCAS_SETVALS  /* nothing to set needs for casDefCas & casCas */

        switch( seqDefCas->_type) {
        case CASTYPE_MSISEQDEF:
          NEWCAS( casCas, CASTYPE_CAS, req, lastDelayStart);           /* CAS */
          break;
        case CASTYPE_NISSEQDEF:
          /* schedule NISSEQDEF CAS 2 minutes before CAS that executes it */
          NEWCAS( casDefCas, CASTYPE_CASDEF, req, lastDelayStart-120);/*CASDEF*/
          NEWCAS( casCas, CASTYPE_CAS, req, 120);                      /* CAS */
          break;
        }
#       undef NEWCAS_SETVALS

        /* - *SEQDEF &/or NISEX/MSISR
         *   - copy bulk of data from ->_data. */

        switch( seqDefCas->_type) {

        case CASTYPE_MSISEQDEF:                        /* MSISR/SHOOT only */
          switch ( nScans) {
          /* for nScans = 1 (i.e. 1 image), use SHOOT because SR #Iter must
           * be > 1
           */
          case 1:
#           define msiShootCas msiRepCas
#           define NEWCAS_SETVALS \
            msiShootCas->msiShootCount = 1; \
            msiShootCas->msiShootSeq[0] = exeCas->msiRepSeq[0]; \
            msiShootCas->msiShootDel[0] = 0; \
            msiShootCas->msiShootImageTypeDbl[0] = exeCas->msiRepImageTypeDbl;

            NEWCAS( msiShootCas, CASTYPE_SHOOT, casCas, 0);
#           undef NEWCAS_SETVALS
#           undef msiShootCas

            break;

          /* for nScans > 1, use MSISR */
          default:
#           define NEWCAS_SETVALS \
            *msiRepCas->_data._msiRep = *exeCas->_data._msiRep; \
            msiRepCas->msiRepIter[0] = nScans; \
            msiRepCas->msiRepDel[0] = iVal;

            NEWCAS( msiRepCas, CASTYPE_MSISR, casCas, 0);
#           undef NEWCAS_SETVALS

            break;
          }

          break;

        case CASTYPE_NISSEQDEF:

#         define NEWCAS_SETVALS \
          *nisSeqDefCas->_data._nisSeqDef = *seqDefCas->_data._nisSeqDef; \
          nisSeqDefCas->nisIdNum = exeCas->nisRepSeq[0]; \
          nisSeqDefCas->nisNumScans = nScans; \
          nisSeqDefCas->nisSecPerObs = iVal; \
          \
          /* limit s/obs to legal value (16), move time into secBtwObs */ \
          \
          if ( nisSeqDefCas->nisSecPerObs > 16) { \
            nisSeqDefCas->nisSecBtwObs += (nisSeqDefCas->nisSecPerObs - 16); \
            nisSeqDefCas->nisSecPerObs = 16; \
          } /* if nisSecPerObs > 16 */ \
          \
          /* if stretching, reduce secBtwObs & secBtwScan to add numObs */ \
          /* then put as much as possible of it back into nisNumObs, */ \
          /* saving change in nisNumObs and decrease starting mirror posn */ \
          addlObsPerScan = 0; \
          if ( doStretch) { \
            \
            /* move time from secBtwObs to secBtwScan, leave secBtwObs = 1 */ \
            \
            if ( nisSeqDefCas->nisSecBtwObs > 1) { \
              nisSeqDefCas->nisSecBtwScan += ((nisSeqDefCas->nisSecBtwObs-1) \
                                             * (nisSeqDefCas->nisNumObs-1)); \
              nisSeqDefCas->nisSecBtwObs = 1; \
            } /* if nisSecBtwObs > 1 */ \
            \
            /* move time from secBtwScan to numObs, leave secBtwScan >= 1,  */ \
            /* save delta numObs to subtract from start mirror posn, */ \
            /* don't let delta numObs make start mirror posn go below 0 */ \
            \
            secPerBtwObs = nisSeqDefCas->nisSecBtwObs \
                         + nisSeqDefCas->nisSecPerObs; \
            while ( nisSeqDefCas->nisSecBtwScan > secPerBtwObs \
                 && (exeCas->nisRepMirrorPosn[0]/nisSeqDefCas->nisStepMirror) \
                    > addlObsPerScan ) { \
              nisSeqDefCas->nisSecBtwScan -= secPerBtwObs; \
              nisSeqDefCas->nisNumObs++; \
              addlObsPerScan++; \
            } /* while */ \
          } /* if doStretch */

          NEWCAS( nisSeqDefCas, CASTYPE_NISSEQDEF, casDefCas, 0);
#         undef NEWCAS_SETVALS

#         define NEWCAS_SETVALS \
          *nisExCas->_data._nisRep = *exeCas->_data._nisRep; \
          nisExCas->nisRepMirrorPosn[0] /* add stretch to start mirror posn */ \
            -= (addlObsPerScan * nisSeqDefCas->nisStepMirror);

          NEWCAS( nisExCas, CASTYPE_NISEX, casCas, 0);
#         undef NEWCAS_SETVALS

          /* switch sequence id # for next pass */
          toggleSeqNum = exeCas->nisRepSeq[0];
          exeCas->nisRepSeq[0] = exeCas->nisRepSeq[1];
          exeCas->nisRepSeq[1] = toggleSeqNum;

          break;
        } /* switch seqDefCas->_type */

        lastDelayStart = nScans * scanLen + extraDelay;

        if ( doBreak) break;

      } /* while startTime < exeCas->_duration */

      if ( !selReq) break;    /* break out on first pass if no selected CASes */

    } /* while ( !selReq || cas1) - topNext loop */

    if ( !selReq) break;      /* break out on first pass if no selected CASes */
    if ( !selCasPtrs[++iSelCas]) break;    /* break if no more selected CASes */

  } /* while ( 1) - main loop through selCasPtrs */

  FILLCLEANUP;
  return 1;
} /* orbitgui_createCASFILL_fldMenu_ok_CB( CAS *cas, FLDMENU *fldMenu) { */

/***************************************/
void
orbitgui_createCASFILL_fldMenu( CAS **selCasPtrs, int opt, OCWS *CASWStuff
                              , Widget w) {
CAS *exeCas = (CAS *) NULL;
CAS *seqDefCas = (CAS *) NULL;
int numflds, i, icount, ii, mtl, iBit, numBits;
char **lcllbl0, *txt0, *txtFn;
FLDMENU *fldMenu, *fm;
CAS *lclcas;
static long casNum;
UTC utc;
long lclAddlMalloc, gblAddlMalloc, addlMalloc;
void *lclPtr, *gblPtr;

#ifdef FREE
#undef FREE
#endif
#define FREE(A) if ( A) free(A)

#ifdef ERRRTN
#undef ERRRTN
#endif
#define ERRRTN { FREE(selCasPtrs); \
                 FREE(exeCas); FREE(seqDefCas); \
                 return; }

#define FMFILLLBL \
    "Sched pt (UTC)"     /* these first two may be skipped IF */ \
  , "Duration, s "       /* DS40's are selected in CAS menu */ \
  , "Test Interval, s" \
  , "Overlap Limits (0-1)" \
  , "Ctl Var Limits, s"

static char *msiFillFmLbl[] = {
    FMFILLLBL
  , "MSI Sequence ID (3-30)"
  /* , "MSI Delta btw Seq 1 executes, s"      = control variable */
  /* , "MSI # Iterations"                     = set by olap calcs */
  , "MSI Image Type"
  , (char *) NULL
  };

static char *nisFillFmLbl[] = {
    FMFILLLBL
  , "NIS Start Mirror Pos"
  , "NIS Aperture\0IN\0OUT\0SHTR\0SLIT\0"
  , "NIS Gain\0 10X\0 1X\0"
  , "NIS Sequence ID (3-15)"
  /* , "NISSEQDEF Sequence ID (3-15)"  = 99 */
  /* , "NISSEQDEF # of scans"          = 2 */
  /* , "NISSEQDEF Seconds btw scans"   = (#obs/scan + 3) / 4 Hz */
  , "NISSEQDEF # obs / scan"
  , "NISSEQDEF Cal interval"
  /* , "NISSEQDEF # Spectra/obs"       = control variable (ctl var) */
  , "NISSEQDEF # of rests"
  , "NISSEQDEF # of darks"
  /* , "NISSEQDEF Mirror steps/obs"    = 2 for OUT, else 1 */
  /* , "NISSEQDEF Seconds btw obs"     = 1 */
  , "Stretch obs/scan to fill time?\0Yes\0No\0"
  , (char *) NULL
  };

  /* start code.  get utc */

  orbitgui_return_utc_ci( cur_item, &utc);

  /* allocate new seq def & msisr/nisexecute/... CASes for each type of fill
   * most of the stuff is in the seq def, the shoot/execute/... CASes 
   * hold additional info e.g. start time, starting mirror position (NIS), &c
   */

  switch( opt) {
  case MSIFILL_BUTTON:

    lcllbl0 = msiFillFmLbl;

    seqDefCas = orbit_CAS_new( CASTYPE_MSISEQDEF);
    if ( !seqDefCas) ERRRTN
    exeCas = orbit_CAS_new( CASTYPE_MSISR);
    if ( !exeCas) ERRRTN

    lclAddlMalloc = 0;

    mtl = FLD_MAXTXTLENPERDBL;    /* length-of-text of longest field excl UTC */

    /* ERRRTN; */

    break;

  case NISFILL_BUTTON:

    lcllbl0 = nisFillFmLbl;

    seqDefCas = orbit_CAS_new( CASTYPE_NISSEQDEF);
    if ( !seqDefCas) ERRRTN
    exeCas = orbit_CAS_new( CASTYPE_NISEX);
    if ( !exeCas) ERRRTN

    /* additional memory for fields not in NISEX or NISSEQDEF to malloc at 
     * end of fldMenu structs
     */
    lclAddlMalloc = sizeof( long); /* (long) bitfield for NISFILL_BIT*STRETCH */

    mtl = 2 * FLD_MAXTXTLENPERDBL;/* length-of-text of longest field excl UTC */
    break;

  default:
    break;
  }


  exeCas->prev = exeCas->next = seqDefCas;
  seqDefCas->prev = seqDefCas->next = exeCas;

  seqDefCas->_miscPtr = exeCas->_miscPtr = (void *) CASWStuff;

  /* skip first two fields (UTC & duration) if there is at least one 
   * non-null selected items
   */
  if ( selCasPtrs) if ( *selCasPtrs) lcllbl0 += 2;

  /* count labels - i.e. non-null fields */
  for ( numflds=0; lcllbl0[numflds]; numflds++) ;
  numflds++;    /* FLD_END */

  /* allocate field menu structures
   *  plus space for fld_txt0 fields (each mtl chars wide)
   *  plus space for additional malloc'ed stuff
   */

  gblAddlMalloc = sizeof(long);                          /* for Test Interval */
  gblAddlMalloc += UTCLEN+1;                         /* Sched Pt - UTC string */

  addlMalloc = gblAddlMalloc + lclAddlMalloc;   /* total space after fld_txt0 */

  fldMenu = (FLDMENU *) malloc((numflds * (sizeof(FLDMENU)+mtl)) + addlMalloc);

  txt0 = (char *) (fldMenu + numflds);     /* fld_txt0 fields after fldMenu's */

  /* lclPtr & gblPtr point to end of ->fld_txt0 fields, contains 
   * - space in lclAddlMalloc
   * - Test Interval, s             (part of gblAddlMalloc)
   * - UTC string (Sched Pt)        (part of gblAddlMalloc)
   */

  lclPtr = (void *) (txt0 + (numflds * mtl));             /* after ->fld_txt0 */
  gblPtr = lclPtr + lclAddlMalloc;                            /* after lclPtr */

  /* setup fldMenu structure pointers to labels & to text strings */

  for ( fm=fldMenu, i=0; i<numflds; NEXTFM) {
    fm->lbl_txt = lcllbl0[i];
    fm->fld_txt0 = txt0 + (i * mtl);
  }

  fm = fldMenu;
  i = 0;

#  ifdef NEXTFM
#  undef NEXTFM
#  endif
#  define NEXTFM fm++, i++

  /* use first two fields only if no cas items selected 
   * i.e. null selCasPtrs or null *selCasPtrs
   */
  if ( !selCasPtrs ? 1 : !*selCasPtrs ? 1 : 0) {

    /* "Sched pt (UTC)" */
    /* ->_et - scheduling point, as UTC text
     *   - use extra string at end of gblAddlMalloc area, after test interval
     *   - get UTC text from UTC struct member _utcstr
     *   - ok callback will convert UTC string back to ->_et
     */
    fm->type = FLD_TXT; fm->fld_txt = (char *) (gblPtr + sizeof(long));
    strncpy( fm->fld_txt, utc._utcstr, UTCLEN+1);
    fm->fld_txt[UTCLEN] = '\0';
    strncpy( fm->fld_txt0, fm->fld_txt, UTCLEN+1);
    fm->fld_txt_maxlen = UTCLEN;
    NEXTFM;

    /* "Duration, s" - ->_duration */
    fm->type = FLD_LNG; fm->fld_lng = &exeCas->_duration;
    *fm->fld_lng = 3600;
    NEXTFM;
  }

  /* "Test Interval, s" - long variable in gblAddlMalloc area */

  fm->type = FLD_LNG; fm->fld_lng = (long *) gblPtr;
  *fm->fld_lng = 600;
  NEXTFM;

  /* "Overlap Limits (0-1)" */
  fm->type = FLD_DBL8; fm->fld_dbl = seqDefCas->_olapLims;
  fm->fld_dbl[0] = 0.1;
  fm->fld_dbl[1] = 0.4;
  fm->subtype = fm->fld_txt_maxlen = 2;
  NEXTFM;

  /* "Ctl Var Limits, s" */
  fm->type = FLD_LNG8; fm->fld_lng = seqDefCas->_ctlLims;
  /* fm->fld_lng[0] & [1] will be set below */
  fm->subtype = fm->fld_txt_maxlen = 2;
  NEXTFM;

  switch ( opt) { 

  case MSIFILL_BUTTON:

    exeCas->msiRepSeq[0] = 30;

    /* minimum interval between sequences is 
     * (1s imaging + 1s to move filter) per image
     */
    seqDefCas->_ctlLims[0] = 2;
    seqDefCas->_ctlLims[1] = 9999;

    fm->type = FLD_LNG; fm->fld_lng = exeCas->msiRepSeq;   /* "MSI Seq. ID ...*/
    NEXTFM;

    /* MSI Image Type */
    fm->type = FLD_DBL; fm->fld_dbl = &exeCas->msiRepImageTypeDbl;
    NEXTFM;

    break;

  case NISFILL_BUTTON:

    exeCas->nisRepSetup = 20;   /* from maureen & karl, 19991004 */

    exeCas->nisRepSeq[0] = 15;
    exeCas->nisRepSeq[1] = 14;

    seqDefCas->_ctlLims[0] = 1;
    seqDefCas->_ctlLims[1] = 129;

    lclcas = exeCas;           /* set for SETFRMBIT & MKFLD* macros to work */

    SETFRMBIT( nisRepBits[0], NISREPBITallAper, nisRepAperture[0]
             , NISREPI000Aper
             , nisRepAperArray, NISREPIslitAper)

    SETFRMBIT( nisRepBits[0], NISREPBITallGain, nisRepGain[0]
             , NISREPI000Gain
             , nisRepGainArray, NISREPI000Gain)

    MKFLDLNGP( nisRepMirrorPosn);                   /* "NIS Start Mirror Pos" */
    MKFLDBIT( nisRepBits[0], NISREPI000Aper);            /* "NIS Aperture ... */
    MKFLDBIT( nisRepBits[0], NISREPI000Gain);                /* "NIS Gain ... */
    fm->type = FLD_LNG8; fm->fld_lng = exeCas->nisRepSeq;/* "NIS Seq. ID ...*/
    fm->subtype = fm->fld_txt_maxlen = 2;
    NEXTFM;

    lclcas = seqDefCas;

    seqDefCas->nisIdNum = 99;               /* dummy NISSEQDEF Sequence ID */
    seqDefCas->nisNumScans = 2;              /* "NISSEQDEF # of scans" = 2 */
    /* nisSecBtwScan to be set later            "NISSEQDEF Seconds btw scans" */
    MKFLDLNG( nisNumObs);                         /* "NISSEQDEF # obs / scan" */
    MKFLDLNG( nisCalInterval);                    /* "NISSEQDEF Cal interval" */
    /* nisSecPerObs is the control variable:        "NISSEQDEF # Spectra/obs" */
    MKFLDLNG( nisNumRests);                         /* "NISSEQDEF # of rests" */
    MKFLDLNG( nisNumDarks);                         /* "NISSEQDEF # of darks" */
    /* nisStepMirror to be set later             "NISSEQDEF Mirror Steps/obs" */
    seqDefCas->nisSecBtwObs = 1;                     /* NISSEQDEF Sec Btw Obs */

    /* "Stretch obs/scan to fill time?\0Yes\0No\0" */
    MKFLDANYBIT( fm, ((long *)lclPtr)[0], NISFILL_I000STRETCH);
    *fm->fld_lng = NISFILL_BITdoSTRETCH;
    NEXTFM;

    break;

  } /* switch opt */

  /* last fm struct:  add cancel callback & filetype */
  fm->type = FLD_END;
  fm->client_call = orbitgui_createCASFILL_fldMenu_cancel_CB;
  strcpy( fm->fld_txt0, "*.");
  strcat( fm->fld_txt0, WHOAMIFILL(opt));

  /* add ok callback & CAS pointer to first fm struct */
  fldMenu->client_call = orbitgui_createCASFILL_fldMenu_ok_CB;
  fldMenu->client_data = WFILL( opt, seqDefCas
                                   , seqDefCas
                                   , (CAS *) NULL);

  (fldMenu+1)->client_data = selCasPtrs;    /* save selected CAS pointer list */

  orbitgui_create_fldmenu_dialog( buttonForm, WHOAMIFILL(opt), fldMenu);

  return;
} /* orbitgui_createCASFILL_fldMenu() */

/***************************************/

void 
orbitgui_CASmenu_widget_CB(Widget w,XtPointer client_data,XtPointer call_data) {
int option = (long) client_data;
int savOption = option;
OCWS *CASWStuff;
Widget wtop = orbitgui_findWMS( w);
Widget form = XtParent( w); /* to be set insensitive to disable other buttons */
CAS *foundCAS;
CAS **foundCASes = (CAS **) NULL;
CAS *cas, *frag;
int i, nCASes;
CAS *lastRunREQ, *lastRunCAS;

  FINDOCWS( wtop)
  if ( !CASWStuff) {
    fprintf( stderr, "orbitgui_CASmenu_widget_CB: %s\n"
           , "Can't find OCWS structure matching a topshell; closing it");
    fflush( stderr);
    XtDestroyWidget( wtop);
  }

#define BREAKIFNOSELECTEDCAS \
  if ( !(foundCAS=orbitgui_CAS_getSelectedCAS( CASWStuff))) break

#define GETSELECTEDCASES \
  foundCASes = orbitgui_CAS_getSelectedCASes( CASWStuff); \
  if ( !foundCASes) nCASes = -1; \
  else for ( nCASes=0; foundCASes[nCASes]; ++nCASes) /* count selected CASes */

#define GETSELECTEDTOPCASES /* convert all children to top level parent */ \
  GETSELECTEDCASES; \
  for ( i=0; i<nCASes; ++i) { \
    if ( foundCASes[i]->top)  \
      foundCASes[i] = foundCASes[i]->top->top ? foundCASes[i]->top->top \
                                              : foundCASes[i]->top; \
  }

#define BREAKIFNOSELECTEDCASES \
  foundCASes = orbitgui_CAS_getSelectedCASes( CASWStuff); \
  if ( !foundCASes) break; \
  if ( !foundCASes[0]) break; \
  for ( nCASes=1; foundCASes[nCASes]; ++nCASes) /* count selected CASes */

  /* option saved in savOption; 
   * - option may = (CASTYPE_COUNT+(CASTYPE_*)) for CASTYPE_CASOPNAV, 
   *   if so, modify option for switch statement
   */

  if ( option >= CASTYPE_COUNT && option < ((CASTYPE_COUNT)+(OPNAVTYPE_COUNT))){
    option = CASTYPE_CASOPNAV;
  }

  switch( option) {

  /* auto-rename, optionally auto-save, REQs */

  case AUTOSAVEREQ_BUTTON:
  case AUTONAMEREQ_BUTTON:

    BREAKIFNOSELECTEDCASES;
    XmListDeselectAllItems( listw);            /* deselect to avoid linux bug */
    orbit_CAS_autonameReqs( foundCASes, orbitgui_CAS_addToMiscPtr);

    if ( option == AUTOSAVEREQ_BUTTON) {
      AUTOSAVEREQ_CB( w, (XtPointer) foundCASes, call_data);
      foundCASes = (CAS **) NULL;    /* orbit_CAS_autoSaveReqs will free this */
    }

    break;

  /* load cas */

  case LOADSASF_BUTTON:
    LOADSASF_CB( w, (XtPointer) CASWStuff, call_data);
    break;

  /* load cas */

  case LOADCAS_BUTTON:
    LOADCAS_CB( w, (XtPointer) CASWStuff, call_data);
    break;

  /* load all cas */

  case LOADALLREQ_BUTTON:
    LOADALLREQ_CB( w, (XtPointer) CASWStuff, call_data);
    break;

  /* load all cas */

  case LOADNRUNALLREQ_BUTTON:
    LOADNRUNALLREQ_CB( w, (XtPointer) CASWStuff, call_data);
    break;

  /* save cas */

  case SAVECAS_BUTTON:
    BREAKIFNOSELECTEDCAS;
    SAVECAS_CB( w, (XtPointer) foundCAS, call_data);

    break;

  /* save selected REQ to SASF file */

  case SAVESASFONEREQ_BUTTON:
    BREAKIFNOSELECTEDCAS;
    SAVESASFONEREQ_CB( w, (XtPointer) foundCAS, call_data);

    break;

  /* save All REQs to SASF file */

  case SAVESASFALLREQ_BUTTON:
    if ( CASWStuff->_cas) {
      SAVESASFALLREQ_CB( w, (XtPointer) CASWStuff->_cas, call_data);
    }
    break;

  /* Run All top level frags 
   * - RUNCAS/orbit_CAS_runCas will handle children
   * - generate frames per repeats' instructions
   */
  case RUNALLCAS_BUTTON:
    for ( foundCAS=CASWStuff->_cas; foundCAS; foundCAS = foundCAS->next) {
      if ( !foundCAS->top) RUNCAS( foundCAS);
    }
    break;

  /* Run Selected item(s) - (1) run every frag under a selected REQ
   *                      - (2) run every frag under a selected CAS or CASOPNAV
   *                      - (3) run single selected repeat frag
   *                      - ***N.B. don't run if parent CAS or REQ already run
   *                        - tests assume foundCASes in same order as listw
   */
  case RUNREPEATCAS_BUTTON:
    BREAKIFNOSELECTEDCASES;

    lastRunREQ = lastRunCAS = (CAS *) NULL; /* clear last run parent REQ/CAS* */

    for ( i=0; i<nCASes; ++i) {   /* step through selected fragments IN ORDER */
    CAS *parentCAS, *parentREQ;

      foundCAS = foundCASes[i];

      /* climb ladder to parent CAS*
       * - if foundCAS is a CAS* OR foundCAS has no parent, 
       *   parentCAS should be set to NULL 
       */
      if ( lastRunCAS ) {
        for ( parentCAS=foundCAS->top; parentCAS; parentCAS=parentCAS->top) {
          if ( !isREQ( *parentCAS)) if ( isANYTOP( *parentCAS)) break;
        }
      }
      /* same check for parent REQ
       */
      if ( lastRunREQ) {
        for ( parentREQ=foundCAS->top; parentREQ; parentREQ=parentREQ->top) {
          if ( isREQ( *parentREQ)) break;
        }
      }

      /* run foundCAS if any & all parents not already run 
       * - if foundCAS is REQ or CAS*, save in lastRun*
       */
      if ( (!lastRunCAS || (parentCAS!=lastRunCAS))
        && (!lastRunREQ || (parentREQ!=lastRunREQ)) ) {

        /* fprintf( stderr, "Running '%s'\n", foundCAS->_name); */
        RUNCAS( foundCAS);

        if ( isREQ(*foundCAS)) lastRunREQ = foundCAS;
        else if ( isANYTOP(*foundCAS)) lastRunCAS = foundCAS;
      } 
        /* else /**/
        /* fprintf( stderr, "Skipping '%s'\n", foundCAS->_name); /**/
      /* fflush( stderr); /**/

    } /* for i<nCASes */

    break; /* case RUNREPEATCAS_BUTTON: */

  /* Modify - find selected position, convert to CAS, startup CAS field menu */

  case MODIFYCAS_BUTTON:
    BREAKIFNOSELECTEDCAS;
    /* pass parent of button i.e. form widget */
    orbitgui_create_CAS_fldMenu( foundCAS, 0, CASWStuff, form);

    break;

  /* Remove - find selected position, find previous CAS, 
   *          remove CAS + listw entry, adjust CAS linked list
   */
  case REMOVECAS_BUTTON:
    BREAKIFNOSELECTEDCASES;

    /* bug in Linux during XmListDeletePos -> XmLstReallocSelectedItems 
     * - try to avoid the bug by deselecting all items first
     *   - won't affect the result because they will be deleted anyway
     */
    XmListDeselectAllItems( listw);

    /* delete from the bottom of the sorted list up */

    while ( --nCASes > -1) {
      orbitgui_CAS_deleteCAS( foundCASes[nCASes]);
    }

    break;

  case CASTYPE_REQ:
  case CASTYPE_CAS:
  case CASTYPE_CASDEF:
  case CASTYPE_CASOPNAV:
  case CASTYPE_NISSU2:
  case CASTYPE_NISSU1:
  case CASTYPE_NISSR:
  case CASTYPE_NISDR:
  case CASTYPE_NISEX:
  case CASTYPE_NISSEQDEF:
  case CASTYPE_MSISEQDEF:
  case CASTYPE_SHOOT:
  case CASTYPE_MSITR:
  case CASTYPE_MSISR:
  case CASTYPE_MSIDR:
  case CASTYPE_MSIDSR:
  case CASTYPE_DS40XGRS:
  case CASTYPE_DS40:
  case CASTYPE_DS40FULL:
  case CASTYPE_DS56:
  case CASTYPE_AUTOEXPOSE:
  case CASTYPE_LOADFILT:
  case CASTYPE_MSICONFIG:
  case CASTYPE_NISCONFIG:
  case CASTYPE_XGRSCONFIG:
  case CASES_NOPARAMS:
    orbitgui_create_CAS_fldMenu( (CAS *) 0, savOption, CASWStuff, form);
    break;

  case MSIFILL_BUTTON:
  case NISFILL_BUTTON:
    GETSELECTEDCASES;
    orbitgui_createCASFILL_fldMenu( foundCASes, option, CASWStuff, form);
    foundCASes = (CAS **) NULL;    /* CASFILL_fldMenu routines will free this */
    break;

  case CLOSECASLIST_BUTTON:
    XtDestroyWidget( wtop);
    break;
  }

  if ( foundCASes) free( (void *) foundCASes);                   /* cleanup */
  foundCASes = (CAS **) NULL;

  return;

} /* orbitgui_CASmenu_widget_CB() */

/***********************************************************/
/* OPNAVTYPE_* Simple Option Menu callback
 * - simple wrapper for orbit_CASmenu_widget_CB() above
 *   - OPNAVTYPE_* is in client_data, add to CASTYPE_COUNT to 
 *     get proper client_data for *_widget_CB()
 */

void
orbitgui_CASmenu_opnavSOM_CB( Widget w,XtPointer client_data
                            , XtPointer call_data) {

long modClientData = CASTYPE_COUNT + ((long) client_data);

  orbitgui_CASmenu_widget_CB( w, (XtPointer) modClientData, call_data);
  return;
} /* orbitgui_CASmenu_opnavSOM_CB() */


/******************************************/
/* return OCWS that has a target cur_item */

static OCWS *
orbitgui_getOCWSFromCurItem( void *cur_item_target) {
OCWS *CASWStuff = firstCASWStuff;
OCWS **newCASWStuffPtr;

  while ( CASWStuff ) {/* look for existing (OCSW *) that has cur_item_target */
    if ( cur_item == cur_item_target) break;
    else CASWStuff = CASWStuff->next;
  }

  /* if no match found, make new OCWS structure to hold cur_item_target */

  if ( !CASWStuff) {
    if ( (CASWStuff = (OCWS *) malloc( sizeof( OCWS))) ) {

      for ( newCASWStuffPtr = &firstCASWStuff     /* find where new OCWS goes */
          ; *newCASWStuffPtr
          ; newCASWStuffPtr = &(*newCASWStuffPtr)->next) ;

      *newCASWStuffPtr = CASWStuff;                   /* ... and put it there */

      cur_item = cur_item_target;                      /* fill in OCSW struct */
      CASWStuff->_cas = (CAS *) 0;
      CASWStuff->next = (OCWS *) 0;

    } /*if CASWStuff=malloc ... */
  } /* if !CASWStuff ... */
  return CASWStuff;
}

/*********************************************************/
/* return first CAS from OCWS that has a target cur_item */

CAS *
orbitgui_get1stCASFromCurItem( void *cur_item_target) {
OCWS *CASWStuff = orbitgui_getOCWSFromCurItem( (void *) cur_item_target);

  if ( CASWStuff ) return CASWStuff->_cas;
  return( (CAS *) 0);
}

/************************************************************/
/* CAS Menu Callback - bring up CAS Menu                    */

void 
orbitgui_CASmenu_CB( Widget w, XtPointer client_data, XtPointer call_data) {
Widget toplevel = orbitgui_findWMS( w);
Widget main_w, pb, topw, scrollw, sepw, pbbelow;
OCWS *CASWStuff;
OCWS *ocws;
XmString *xmss;
CAS *cas;
long ncas, icas;

  /* find OCWS with cur_item that matches client_data ... */

  CASWStuff = orbitgui_getOCWSFromCurItem( (void *) client_data);

  topshell = XtVaCreatePopupShell( "CAS/Fragment Menu"
    , topLevelShellWidgetClass, toplevel
    , XmNallowShellResize, True /**/
    , XmNdeleteResponse, XmDESTROY /**/
    , XmNresizePolicy, XmRESIZE_GROW /**/
    , NULL);
  orbitgui_setSensitivity( w, topshell);

  /* Create a MainWindow to contain everything */
  main_w = XtVaCreateWidget( "main_w", xmFormWidgetClass, topshell
      , XmNautoUnmanage, True /**/
      , XmNresizable, True/**/
      , NULL);

  /* create a form to hold the buttons at the bottom */
  buttonForm = XtVaCreateManagedWidget( "buttonForm", xmFormWidgetClass, main_w
          , XmNleftAttachment,    XmATTACH_FORM
          , XmNbottomAttachment,     XmATTACH_FORM
          , XmNresizable, True
          , NULL);

#define LEFTEDGE( TXT, CDATA) \
  pb = XtVaCreateManagedWidget ( TXT \
     , xmPushButtonGadgetClass, buttonForm \
     , XmNleftAttachment, XmATTACH_FORM \
     , BOTTOMATTACHFLDS \
     , NULL); \
  XtAddCallback( pb, XmNactivateCallback, orbitgui_CASmenu_widget_CB \
               , (XtPointer) CDATA)

#define NOTLEFTEDGE( TXT, CDATA) \
  pb = XtVaCreateManagedWidget ( TXT \
     , xmPushButtonGadgetClass, buttonForm \
     , XmNleftAttachment, XmATTACH_WIDGET \
     , XmNleftWidget, pb \
     , BOTTOMATTACHFLDS \
     , NULL); \
  XtAddCallback( pb, XmNactivateCallback, orbitgui_CASmenu_widget_CB \
               , (XtPointer) CDATA)

#define BOTTOMATTACHFLDS XmNbottomAttachment, XmATTACH_FORM

  /* bottom row:  req, cas, casdef, casopnav (simple option menu) load */

  LEFTEDGE( "REQuest", CASTYPE_REQ);
  NOTLEFTEDGE( "CAS", CASTYPE_CAS);
  NOTLEFTEDGE( "CASDEF", CASTYPE_CASDEF);

  {
  XmString s[OPNAVTYPE_COUNT], *sptr;
  XmString lbl;
  int i;

    lbl = XmStringCreateLocalized("Add:");
    for ( (sptr=s), i=0; i<OPNAVTYPE_COUNT; ++i, ++sptr) {
      *sptr = XmStringCreateLocalized( opnavNames[i]);
    }

#   define XMVPB(I,K) XmVaPUSHBUTTON, s[I], K, NULL, NULL

    pbbelow =
    pb = XmVaCreateSimpleOptionMenu( buttonForm, "opnavButtons"
      , lbl, 'N', 0, orbitgui_CASmenu_opnavSOM_CB
      , XMVPB(OPNAVTYPE_A,'A')
      , XMVPB(OPNAVTYPE_B,'B')
      , XMVPB(OPNAVTYPE_BP,'b')
      , XMVPB(OPNAVTYPE_C,'C')
      , XMVPB(OPNAVTYPE_CACRUISE,'U')
      , XMVPB(OPNAVTYPE_CE,'c')
      , XMVPB(OPNAVTYPE_CT,'T')
      , XMVPB(OPNAVTYPE_D,'D')
      , XMVPB(OPNAVTYPE_E,'E')
      , XMVPB(OPNAVTYPE_E2,'2')
      , XMVPB(OPNAVTYPE_E3,'3')
      , XMVPB(OPNAVTYPE_E4,'4')
      , XMVPB(OPNAVTYPE_F,'F')
      , XMVPB(OPNAVTYPE_G,'G')
      , XMVPB(OPNAVTYPE_H,'H')
      , XMVPB(OPNAVTYPE_I,'I')
      , XMVPB(OPNAVTYPE_J,'J')
      , XMVPB(OPNAVTYPE_K,'K')
      , XMVPB(OPNAVTYPE_DOUBLE_K,'k')
      , XmNleftAttachment, XmATTACH_WIDGET
      , XmNleftWidget, pb
      , BOTTOMATTACHFLDS
      , NULL);

    for ( (sptr=s), i=0; i<OPNAVTYPE_COUNT; ++i, ++sptr) { XmSF( *sptr); }
    XmSF( lbl);
    XtManageChild( pbbelow);
  }

  NOTLEFTEDGE( "Load", LOADCAS_BUTTON);
  NOTLEFTEDGE( "Load All .REQ", LOADALLREQ_BUTTON);
  NOTLEFTEDGE( "Load&Run *.REQ", LOADNRUNALLREQ_BUTTON);
  NOTLEFTEDGE( "Load SASF", LOADSASF_BUTTON);
  NOTLEFTEDGE( "Write SASF", SAVESASFONEREQ_BUTTON);
  /* pbbelow = is set in the simple option menu above */
  NOTLEFTEDGE( "Write ALL to SASF", SAVESASFALLREQ_BUTTON);

#undef BOTTOMATTACHFLDS
#define BOTTOMATTACHFLDS XmNbottomAttachment, XmATTACH_WIDGET \
                       , XmNbottomWidget, pbbelow

  /* next row:  MSI/NISPARK, NISCAL, NISBUFFLUSH, MSI/NIS/XGRSRELATT
   *            - no NAVRELATT
   */
  LEFTEDGE( "MSIPARK", CASTYPE_MSIPARK);
  NOTLEFTEDGE( "NISCAL", CASTYPE_NISCAL);
  NOTLEFTEDGE( "NISBUFFLUSH", CASTYPE_NISBUFFLUSH);
  NOTLEFTEDGE( "NISPARK", CASTYPE_NISPARK);
  NOTLEFTEDGE( "MSIRELATT", CASTYPE_MSIRELATT);
  NOTLEFTEDGE( "NISRELATT", CASTYPE_NISRELATT);
  pbbelow =
  NOTLEFTEDGE( "XGRSRELATT", CASTYPE_XGRSRELATT);

  /* next row:  LOADFILT, AUTOEXPOSE, MSICONFIG, NISCONFIG */

  LEFTEDGE( "LOADFILT", CASTYPE_LOADFILT);
  NOTLEFTEDGE( "AUTOEXPOSE", CASTYPE_AUTOEXPOSE);
  NOTLEFTEDGE( "MSICONFIG", CASTYPE_MSICONFIG);
  NOTLEFTEDGE( "NISCONFIG", CASTYPE_NISCONFIG);
  NOTLEFTEDGE( "DS40", CASTYPE_DS40);
  NOTLEFTEDGE( "DS40FULL", CASTYPE_DS40FULL);
  pbbelow =
  NOTLEFTEDGE( "DS56", CASTYPE_DS56);

  /* next row:  nisex, nissr, nisdr, nisseqdef, ds40, ds40full, ds56 */ 

  LEFTEDGE( "NISEX", CASTYPE_NISEX);
  NOTLEFTEDGE( "NISSR", CASTYPE_NISSR);
  NOTLEFTEDGE( "NISDR", CASTYPE_NISDR);
  NOTLEFTEDGE( "NISSEQDEF", CASTYPE_NISSEQDEF);
  NOTLEFTEDGE( "NISSU2", CASTYPE_NISSU2);
  NOTLEFTEDGE( "NISSU1", CASTYPE_NISSU1);
  pbbelow =
  NOTLEFTEDGE( "NIS Fill", NISFILL_BUTTON);

  /* next row:  shoot, missr, msidr, msidsr, msiseqdef */

  LEFTEDGE( "SHOOT", CASTYPE_SHOOT);
  NOTLEFTEDGE( "MSISR", CASTYPE_MSISR);
  NOTLEFTEDGE( "MSIDR", CASTYPE_MSIDR);
  NOTLEFTEDGE( "MSIDSR", CASTYPE_MSIDSR);
  NOTLEFTEDGE( "MSITR", CASTYPE_MSITR);
  NOTLEFTEDGE( "MSISEQDEF", CASTYPE_MSISEQDEF);
  pbbelow =
  NOTLEFTEDGE( "MSI Fill", MSIFILL_BUTTON);

  /* put Remove button on the right to keep it away from the others */
  pb = XtVaCreateManagedWidget ( "Remove"
     , xmPushButtonGadgetClass, buttonForm
     , XmNrightAttachment, XmATTACH_FORM
     , XmNbottomAttachment, XmATTACH_WIDGET
     , XmNbottomWidget, pbbelow
     , NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitgui_CASmenu_widget_CB
               , (XtPointer) REMOVECAS_BUTTON);

  /* top row: modify, run, save, modify, autorename, close */
  LEFTEDGE( "   Run   ", RUNREPEATCAS_BUTTON);
  NOTLEFTEDGE( "Run All", RUNALLCAS_BUTTON);
  NOTLEFTEDGE( "Modify", MODIFYCAS_BUTTON);
  NOTLEFTEDGE( "Save", SAVECAS_BUTTON);
  NOTLEFTEDGE( "AutoName", AUTONAMEREQ_BUTTON);
  NOTLEFTEDGE( "AutoSave", AUTOSAVEREQ_BUTTON);
  pbbelow = 
  NOTLEFTEDGE( "Close", CLOSECASLIST_BUTTON);

  sepw = XtVaCreateManagedWidget( "sepw"
       , xmSeparatorWidgetClass, main_w
       , XmNleftAttachment, XmATTACH_FORM
       , XmNrightAttachment, XmATTACH_FORM 
       , XmNbottomAttachment, XmATTACH_WIDGET
       , XmNbottomWidget, buttonForm
       , NULL);

  /* create widget for top half */

   topw = XtVaCreateManagedWidget( "topw", xmFormWidgetClass, main_w
        , XmNleftAttachment, XmATTACH_FORM
        , XmNresizable, True 
        , XmNbottomAttachment, XmATTACH_WIDGET 
        , XmNbottomWidget, sepw
        , NULL);

  /* create scrolled list using convenience routine */

  listw = XmCreateScrolledList( topw, "orbitgui_CASmenu", NULL, 0);
  XtVaSetValues( listw
        , XmNvisibleItemCount, 10 /**/
        /* , XmNvisibleItemCount, 4 /* for testing */
        , XmNresizable, True /**/
        /* , XmNselectionPolicy, XmSINGLE_SELECT /**/
        /* , XmNselectionPolicy, XmBROWSE_SELECT   /**/
        /* , XmNselectionPolicy, XmMULTIPLE_SELECT   /**/
        , XmNselectionPolicy, XmEXTENDED_SELECT   /**/
        , XmNleftAttachment, XmATTACH_FORM /**/
        , XmNbottomAttachment, XmATTACH_WIDGET /**/
        , XmNbottomWidget, sepw
        , NULL);
  XtAddCallback( listw, XmNdefaultActionCallback, orbitgui_CASmenu_widget_CB
               , (XtPointer) MODIFYCAS_BUTTON); /**/

  scrollw = XtParent( listw);
  XtVaSetValues( scrollw
        , XmNresizable, True /**/
        /* , XmNscrollingPolicy, XmAUTOMATIC /**/
        /* , XmNscrollBarDisplayPolicy, XmAS_NEEDED /**/
        , NULL);

  /* add cas names to list;
   *  _CAS_addToListW() will ensure no duplicates by modifying _name if nec.
   */

  for ( cas=CASWStuff->_cas, ncas=0; cas; cas=cas->next, ncas++) 
    orbitgui_CAS_addToListW( cas, -1);

  /* add a dummy list item so list sizes big enough when it becomes managed */
  {
  char cdumy[IDLEN+MAXPFXLEN];
  XmString xdumy;
  int i;
    for ( i=0; i<(IDLEN+1); ++i) cdumy[i] = ' ';
    cdumy[i] = '\0';
    xdumy = XmStringCreateLtoR( cdumy, CS);
    XmListAddItem( listw, xdumy, 0);
    XmSF( xdumy);
  }

  XtManageChild( listw);   /* convenience routine leaves widget unmanaged */
  /* XtManageChild( scrollw); /* convenience routine leaves widget unmanaged? */

  XtManageChild( main_w); /**/

  /* remove dummy list item */
  XmListDeletePos( listw, 0);

  XtPopup( topshell, XtGrabNone);

  return;
} /* orbitgui_CASmenu_CB() */
