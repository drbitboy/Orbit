/* orbdb.c - orbit dbm routines
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define _ORBDB_PRIVATE_H_
#include "orbdb.h"

#ifdef _ORBDB_SIMPLETEST_               /* use malloc debugger / leak checker */
void *orbdbTestMalloc( size_t);
void orbdbTestFree( void *);
void orbdbTestRealFree( void *);
#define malloc orbdbTestMalloc
#define free orbdbTestFree
#endif

/******************************************************************/
/* load metKEY structure i.e. observation i.e. MET & instrument
 */
void 
orbdb_loadMetKey( metKEY *metKey, double met, long instr) {
  memset( metKey, 0, sizeof( metKEY));
  metKey->_metKeyVal = TRUNCMET_NEAR(met);
  metKey->_instr = instr;
  return;
}

/******************************************************************/
/* load long plate number into pltKEY structure
 */
void 
orbdb_loadPltKey( pltKEY *pltKey, long plt) {
  *((long *)pltKey) = 0; /* memset( pltKey, 0, sizeof( pltKEY)); */
  *((long *)pltKey) = plt;                                  /* pltKEY is long */
  return;
}

/******************************************************************/
/* unload pltKEY structure into plate number
 */
long
orbdb_unloadPltKey( const pltKEY *pltKey) {
  return *((long *)pltKey);                                 /* pltKEY is long */
}

/************************************************************/
/* comparison function for qsort'ing long's in ascending order
*/
int 
orbdb_compareLongs( const void *v1, const void *v2) {
long *l1 = (long *) v1;
long *l2 = (long *) v2;
  if ( *l1 < *l2) return -1;
  if ( *l1 > *l2) return 1;
  return 0;
} /* orbdb_compareLongs */

/************************************************************/
/* comparison function for qsort'ing pltKEY's in ascending order
*/
int 
orbdb_comparePltKeys( const void *v1, const void *v2) {
pltKEY *l1 = (pltKEY *) v1;
pltKEY *l2 = (pltKEY *) v2;
  return orbdb_compareLongs( v1, v2);
} /* orbdb_comparePltKeys */

/******************************************************************/
/* compare metKEY structures ***N.B. usable as comparison function for qsort(3)
 * - compares ->_instr, then ->_metKeyVal
 * - returns +1 if metKey1 > metKey2
 * - returns 0 if metKey1 == metKey2
 * - returns -1 if metKey1 < metKey2
 * - if used with qsort, will sort into ascending order
 */
int 
orbdb_compareMetKeys( const void *v1, const void *v2) {
metKEY *metKey1 = (metKEY *) v1;
metKEY *metKey2 = (metKEY *) v2;

  if ( metKey1->_instr > metKey2->_instr) return 1;
  if ( metKey1->_instr < metKey2->_instr) return -1;

  /* to here, ->_instr's are equal, test ->_metKeyVal's */

  if ( metKey1->_metKeyVal > metKey2->_metKeyVal) return 1;
  if ( metKey1->_metKeyVal < metKey2->_metKeyVal) return -1;

  return 0;
}

/******************************/
/* sort list into unique list */
size_t
orbdb_qsortUnique( void *base, size_t nmemb, size_t membSize
                , int(*compar)(const void *, const void *)) {
void *curPtr, *startPtr, *endPtr;
size_t newNMemb;

  qsort( base, nmemb, membSize, compar);

  endPtr = base + (nmemb * membSize);           /* set endPtr after last item */

  /* start curPtr & startPtr at last item in list
   * decrement curPtr but maintain status quo (sq) i.e. (*curPtr == *startPtr)
   * - when can no longer decrement curPtr and maintain sq
   *   - if (curPtr<startPtr)
   *     - move (endPtr-startPtr) bytes from startPtr to curPtr (if cur!=start)
   *     - adjust endPtr
   *   - set startPtr membSize less than curPtr, decrement curPtr
   */
  for ( startPtr=curPtr=endPtr-membSize; curPtr > base; curPtr-=membSize) {
    if ( compar( curPtr-membSize, startPtr)) {/* will next curPtr-- break sq? */
      if ( curPtr < startPtr) { 
        memmove( curPtr, startPtr, endPtr-startPtr);
        endPtr -= (startPtr - curPtr);
      }
      startPtr = curPtr-membSize;     /* set startPtr to where curPtr will be */
    }
  }
  if ( curPtr < startPtr) { 
    memmove( curPtr, startPtr, endPtr-startPtr);
    endPtr -= (startPtr - curPtr);
  }
  newNMemb = ((size_t)(endPtr - base)) / membSize;
  return newNMemb;
}
/************************************************************************/
/* function FUNC to malloc DB-ready STRTYP structure. "DB-ready" means 
 * structure memory is followed by contiguous room for N items of type TYP
 * - allocate space for higher of 1.5*N or 10 items
 *   - allows for some expansion without changing dSize
 * - N is also STRTYP member ->N => count of those items
 * - STRTYP member ->MBRPTR is pointer to those items
 * - STRTYP member ->_dSize is total size
 *
 * function LOADDATAFUNC to load ->MBRPTR data
 */
#define MALLOCLOADSTR( MALLOCFUNC, STRTYP, N, MBRTYP, MBRPTR, LOADDATAFUNC) \
STRTYP * \
MALLOCFUNC( const STRTYP *strSrc, long N){ \
long dSize; \
STRTYP *str; \
  \
  dSize=( sizeof(STRTYP)+(((N>7)?(3*N)/2:10) * sizeof(MBRTYP)) ); \
  str = (STRTYP *) malloc( dSize); \
  if ( str) { \
    memset( str, 0, sizeof( STRTYP)); \
    if ( strSrc) *str = *strSrc;  /* non-dSize/N/MBRTYP/MBRPTR-related info */ \
    str->_dSize = dSize; \
    str->N = N; \
    FIXCONTENTPTR(STRTYP,str,MBRTYP,MBRPTR); \
  } \
  return str; \
} \
\
/****************************************/ \
/* load data function                   */ \
void \
LOADDATAFUNC( STRTYP *str, const MBRTYP *dataPtr) { \
  if ( str) { \
    FIXCONTENTPTR(STRTYP,str,MBRTYP,MBRPTR); \
    if ( dataPtr) { \
      memcpy( str->MBRPTR, dataPtr, str->N * sizeof(MBRTYP)); \
    } \
  } \
  return; \
}

/************************************************************/

MALLOCLOADSTR( orbdb_mallocMetContent, metCONTENT, _nVisPlates
             , pltKEY, _visPlates
             , orbdb_loadMetData)

MALLOCLOADSTR( orbdb_mallocPltContent, pltCONTENT, _nMetKeys
             , metKEY, _metKeys
             , orbdb_loadPltData)

#if 1
/******************************************************************/
/* copy from non-DB-ready metCONTENT metSrc into DB-ready metDest
 * - use pltKEY *visPlates for plate keys
 *   - visPlates[] should be uniquely sorted i.e. no duplicates
 * - assumes _nVisPlates already set correctly in metDest
 */
void
orbdb_loadMetContent( metCONTENT *metDest, const metCONTENT *metSrc
                    , const pltKEY *visPlates) {
  if ( metDest) {
  long dSize = metDest->_dSize;              /* save self-descriptive info */
  long nVisPlates = metDest->_nVisPlates;    /*  " */
  pltKEY *visPlates = metDest->_visPlates;   /*  " */
    *metDest = *metSrc;                      /* copy ancilliary data */
    metDest->_dSize = dSize;                 /* restore self-descriptive info */
    metDest->_nVisPlates = nVisPlates;       /*  " */
    metDest->_visPlates = visPlates;         /*  " */
    if ( visPlates) {
      orbdb_loadMetData( metDest, visPlates);  /* copy pltKEY's */
    }
  }
  return;
}
#endif

/******************************************************************/
/* allocate & load met-specific content structure
 * - metSrc is used for non-pltKEY array values (e.g. met, resolution, &c)
 * - arguments nVisPlates & visPlates are used to determine size of 
 *   DB-ready return value
 *   - see orbdb_loadMetContent() comments above
 */
metCONTENT *
orbdb_newMetContent( const metCONTENT *metSrc
                   , const pltKEY *visPlates
                   , long nVisPlates) {
metCONTENT *metNew;

  metNew = orbdb_mallocMetContent( metSrc, nVisPlates);
  orbdb_loadMetData( metNew, visPlates);                     /* copy pltKEY's */

# if 0
  orbdb_loadMetContent( metNew, metSrc, visPlates);
# endif
  return metNew;
} /* orbdb_newMetContent */

/**********************************/

#define LOADDATUMAUTO( DATUM, PTR) \
  LOADDATUM2( DATUM, PTR, (PTR)->_dSize)

#define LOADDATUMSTR( DATUM, STRPTR) \
  LOADDATUM2( DATUM, STRPTR, sizeof *(STRPTR))

#define LOADDATUM2( DATUM, PTR, SIZE) \
  DATUM.dptr = (char *) (PTR); \
  DATUM.dsize = (SIZE)

/****************************************************************/
/* orbdb_fetchMetContentByKey */
metCONTENT *
orbdb_fetchMetContentByKey( const ORBDB_FILES *orbdb_files, const metKEY *key) {
metCONTENT *content;
datum keyDatum, contentDatum;
  if ( !orbdb_files) return (metCONTENT *) NULL;
  LOADDATUMSTR( keyDatum, key);
  contentDatum = gdbm_fetch( gMF, keyDatum);
  content = (metCONTENT *) contentDatum.dptr;

  if ( content) { 

#   ifdef _ORBDB_SIMPLETEST_
    metCONTENT *savContent = content;
    content = (metCONTENT *) malloc( contentDatum.dsize);
    memcpy( content, savContent, contentDatum.dsize);
    orbdbTestRealFree( savContent);
#   endif

    FIXVISPLATESPTR( content);
  }          /* fix _visPlates ptr */
  return content;
}

/****************************************************************/
/* orbdb_fetchPltContentByKey */
pltCONTENT *
orbdb_fetchPltContentByKey( const ORBDB_FILES *orbdb_files, const pltKEY *key) {
pltCONTENT *content;
datum keyDatum, contentDatum;
  if ( !orbdb_files) return (pltCONTENT *) NULL;
  LOADDATUMSTR( keyDatum, key);
  contentDatum = gdbm_fetch( gPF, keyDatum);
  content = (pltCONTENT *) contentDatum.dptr;

  if ( content) {

#   ifdef _ORBDB_SIMPLETEST_
    pltCONTENT *savContent = content;
    content = (pltCONTENT *) malloc( contentDatum.dsize);
    memcpy( content, savContent, contentDatum.dsize);
    orbdbTestRealFree( savContent);
#   endif

    FIXMETKEYSPTR( content);               /* fix _metKeys ptr */
  }
  return content;
} /* orbdb_fetchMetContentByKey */

/****************************************************************/
/* orbdb_fetchMetContent */
metCONTENT *
orbdb_fetchMetContent( const ORBDB_FILES *orbdb_files, double met, long instr) {
metCONTENT *content;
metKEY key;
  if ( !orbdb_files) return (metCONTENT *) NULL;
  orbdb_loadMetKey( &key, met, instr);
  return orbdb_fetchMetContentByKey( orbdb_files, &key);
}

/****************************************************************/
/* orbdb_fetchPltContent */
pltCONTENT *
orbdb_fetchPltContent( const ORBDB_FILES *orbdb_files, long plateNum) {
pltCONTENT *content;
pltKEY key;
datum keyDatum, contentDatum;
  if ( !orbdb_files) return (pltCONTENT *) NULL;
  orbdb_loadPltKey( &key, plateNum);
  return orbdb_fetchPltContentByKey( orbdb_files, &key);
}

/******************************************************************/
/* remove met/instrument key from plate in plate database
 * - return pointer to malloc'ed & updated pltCONTENT
 * - return NULL if no update required
 */
pltCONTENT *
orbdb_removeMetKeyFromPlate( const ORBDB_FILES *orbdb_files
                           , const metKEY *metKey, const pltKEY *pltKeyPtr) {
pltCONTENT *oldPltContent;
metKEY *curMetKey, *endMetKey;

  if ( !orbdb_files) return (pltCONTENT *) NULL;

  oldPltContent = orbdb_fetchPltContentByKey( orbdb_files, pltKeyPtr);

  if ( oldPltContent) {
  long oldN = oldPltContent->_nMetKeys;

    curMetKey = oldPltContent->_metKeys;
    endMetKey = curMetKey + oldPltContent->_nMetKeys;

    if ( curMetKey < endMetKey) 
    while ( orbdb_compareMetKeys( curMetKey, metKey) != 0) { /* work forwards */
                                           /* until key matching target found */
      if ( ++curMetKey == endMetKey) break;           /* or out of ->_metKeys */
    }

    if ( curMetKey == endMetKey) {                 /* check if no match found */
      free( oldPltContent);
      oldPltContent = (pltCONTENT *) NULL;

    } else {                          /* found target metKey in oldPltContent */

      while ( ++curMetKey < endMetKey) {
        curMetKey[-1] = *curMetKey;
      }
      oldPltContent->_nMetKeys--;
    }
  }
  return oldPltContent;

} /* orbdb_removeMetKeyFromPlate() */

/***************************************************/
/* add met/instrument key to plate in plate database
 * - return pointer to malloc'ed & updated pltCONTENT
 *   ***N.B. does not actually update database
 * - return NULL if no update required
 */
pltCONTENT *
orbdb_addMetKeyToPlate( const ORBDB_FILES *orbdb_files
                      , const metKEY *metKey, const pltKEY *pltKeyPtr) {
datum pltKeyDatum;
datum pltContentDatum;
pltCONTENT *newPltContent, *oldPltContent;
metKEY *oldMetKey, *newMetKey;

  if ( !orbdb_files) return (pltCONTENT *) NULL;

  oldPltContent = orbdb_fetchPltContentByKey( orbdb_files, pltKeyPtr);

  if ( oldPltContent) {
  long oldN = oldPltContent->_nMetKeys;
  long oldMaxN = (oldPltContent->_dSize - sizeof(pltCONTENT)) / sizeof(metKEY);

    if ( oldMaxN > oldN) {         /* if there is room in existing pltContent */
      newPltContent = oldPltContent;                                /* use it */
      newPltContent->_nMetKeys++;
    } else {                                                          /* else */
      newPltContent = orbdb_mallocPltContent( (pltCONTENT *) NULL
                                            , oldN+1);      /* malloc new one */
    }
    oldMetKey = oldPltContent->_metKeys + oldN;   /* start at end of _metKeys */
    newMetKey = newPltContent->_metKeys + oldN + 1;

    while ( --oldMetKey >= oldPltContent->_metKeys) {     /* work backwards;  */
      if ( orbdb_compareMetKeys( metKey, oldMetKey) >= 0){/* until find one   */
        break;                                            /* old metKey <=    */
      }                                                   /* target,          */
      *(--newMetKey) = *(oldMetKey);                      /* shift metKeys up */
    }

    *(--newMetKey) = *metKey;                         /* insert target metKey */

    if ( oldMetKey >= oldPltContent->_metKeys) {

      /* check if target metKey already in this plate's db record,
       * - should not be required, but do it anyway
       */
      if (!orbdb_compareMetKeys( metKey, oldMetKey)) {
        if ( newPltContent != oldPltContent) free( newPltContent);
        newPltContent = (pltCONTENT *) NULL;

      } else {                  /* target metKey not already in oldPltContent */

        *(--newMetKey) = *oldMetKey;                  /* copy next old metKey */
        if ( newMetKey != oldMetKey) {    /* if the rest don't already match, */
          while ( --oldMetKey >= oldPltContent->_metKeys) { /* copy them also */
            *(--newMetKey) = *oldMetKey;
          }
        }
      }
      /* free oldPltContent if it is different than newPltContent
       * ***N.B. this includes case above where newPltContent was set to NULL
       */
      if ( newPltContent != oldPltContent) free( oldPltContent);
    }

  } else {                                             /* no old plate found, */
    newPltContent = orbdb_mallocPltContent( (pltCONTENT *) NULL
                                          , 1);               /* make new one */
    orbdb_loadPltData( newPltContent, metKey);    /* ... & load single metKey */
  }
  return newPltContent;

} /* orbdb_addMetKeyToPlate() */

/**************************************************************************/
/* add metCONTENT & plates to met & plate db's
 * - all of metSrc contents used (i.e. put in db's) except _dSize & _visPlates
 *   - (long *) visPlatesLongs converted to (pltKEY *)
 */
int
orbdb_addMet( const ORBDB_FILES *orbdb_files    /* met db file, plate db file */
            , const metCONTENT *metSrc
            , const long *visPlatesLongs) {
datum metKeyDatum, pltKeyDatum;
datum oldMetContentDatum, newMetContentDatum;
metKEY metKey;
metKEY *metKeyPtr;
metCONTENT *newMetContent, *oldMetContent;
pltKEY *newVisPlates, *endNewVisPlates;
pltKEY *oldVisPlates, *endOldVisPlates;
datum *newPltData0, *newPltData;
datum *oldPltData0, *oldPltData;
pltKEY *visPlates0;
long i, sortedNVisPlates;

  if ( !orbdb_files) return -1;

  orbdb_loadMetKey( &metKey, metSrc->_met, metSrc->_instr);

  /* put visPlatesLongs into uniquely sorted (pltKEY *) array */

  visPlates0 = 
    (pltKEY *) malloc( (metSrc->_nVisPlates?metSrc->_nVisPlates:1)
                     * sizeof(pltKEY)
                     );
  for ( i=0; i<metSrc->_nVisPlates; ++i) {
    orbdb_loadPltKey( visPlates0+i, visPlatesLongs[i]);
  }
  sortedNVisPlates = orbdb_qsortUnique( visPlates0
                                      , metSrc->_nVisPlates
                                      , sizeof(pltKEY)
                                      , orbdb_comparePltKeys
                                      );

  newMetContent = orbdb_newMetContent( metSrc
                                     , visPlates0
                                     , sortedNVisPlates);
  free( visPlates0);

  if ( !newMetContent) return -1;

  LOADDATUMAUTO( newMetContentDatum, newMetContent);

  /* look at existing entry for this key
   * - may need to update plate database
   */

  /* oldMetContentDatum = gdbm_fetch( gMF, metKeyDatum);
  /* oldMetContent = (metCONTENT *) oldMetContentDatum.dptr;
  /**/
  oldMetContent = orbdb_fetchMetContentByKey( orbdb_files, &metKey);

  if ( oldMetContent) {                  /* may need to update plate database */
    /*XVISPLATESPTR( oldMetContent);                   /* fix _visPlates ptr */
    oldVisPlates = oldMetContent->_visPlates;
    endOldVisPlates = oldVisPlates + oldMetContent->_nVisPlates;

    /* allocate datum structure for each plate in old met content so plate db
     * can be updated (metKey removed from oldMetContent->_visPlates[N] plate
     * content if newMetContent does not have that plate in it)
     */
    oldPltData0 = (datum *) malloc( oldMetContent->_nVisPlates * sizeof(datum));
  } else {
    oldVisPlates = endOldVisPlates = (pltKEY *) NULL;
    oldPltData0 = (datum *) NULL;
  } /* if oldMetContent  */

  newVisPlates = newMetContent->_visPlates;
  endNewVisPlates = newVisPlates + newMetContent->_nVisPlates;

  if ( newVisPlates < endNewVisPlates) {
    /* allocate datum structure for each plate in new met content so plate 
     * db can be updated (metKey added to newMetContent->_visPlates[N] plate
     * content if oldMetContent does not have that plate in it)
     */
    newPltData0 = (datum *) malloc( newMetContent->_nVisPlates * sizeof(datum));
  } else {
    newPltData0 = (datum *) NULL;
  }

  /* macros to add & remove metKey as necessary */

# define ADDNEW /* add metKey to this plate's content */ \
  newPltData->dptr = \
    (char *) orbdb_addMetKeyToPlate( orbdb_files, &metKey, newVisPlates); \
  newPltData++; \
  newVisPlates++

# define REMOVEOLD /* remove metKey from this plates's content */ \
  oldPltData->dptr = \
    (char *) orbdb_removeMetKeyFromPlate( orbdb_files, &metKey, oldVisPlates); \
  oldPltData++; \
  oldVisPlates++

  /* assuming sorted _visPlates, see if plate database needs changing
   * before end of either new or old plates
   */
  oldPltData = oldPltData0;
  newPltData = newPltData0;

  while ( newVisPlates < endNewVisPlates && oldVisPlates < endOldVisPlates) {
  int comparisonVal = orbdb_comparePltKeys( newVisPlates, oldVisPlates);

    if ( comparisonVal < 0) {             /* in new, not in old => add metKey */
      ADDNEW;
    } else 
    if ( comparisonVal > 0) {          /* in old, not in new => remove metKey */
      REMOVEOLD;
    } else {                                   /* plate is in both, no action */
      oldPltData->dptr = newPltData->dptr = (char *) NULL;
      oldPltData++;
      newPltData++;
      newVisPlates++;
      oldVisPlates++;
    }
  }

  /* new plates left after last of old */

  while ( newVisPlates < endNewVisPlates) { ADDNEW; }

  /* ... or old plates left after end of new */

  while ( oldVisPlates < endOldVisPlates) { REMOVEOLD; }

  /* updated plate content structures are complete in arrays 
   * newPltData & oldPltData (where ->dptr is not NULL)
   * now update the plate database
   * - free ((datum *) [new|old]PltData)->dptr after each update
   * - free (datum *) [new|old]PltData0 & [new|old]MetContent after all updates
   * - put it in a macro (using lclPltData, lclVisPlates & endLclVisPlates)
   *   so it can be coded easily for both new & old
   */
# define UPDATEDB( PLTDATA0, METCONTENT) \
  if ( METCONTENT && PLTDATA0) { \
  datum *lclPltData; \
  pltKEY *lclVisPlates, *endLclVisPlates; \
    lclPltData = PLTDATA0; \
    lclVisPlates = METCONTENT->_visPlates; \
    endLclVisPlates = lclVisPlates + METCONTENT->_nVisPlates; \
    while ( lclVisPlates < endLclVisPlates) { \
      if ( lclPltData->dptr) { \
        LOADDATUM2( pltKeyDatum, lclVisPlates, sizeof(pltKEY)); \
        lclPltData->dsize = ((pltCONTENT *)(lclPltData->dptr))->_dSize; \
        gdbm_store( gPF, pltKeyDatum, *lclPltData, GDBM_REPLACE); \
        free( lclPltData->dptr); \
      } \
      lclPltData++; \
      lclVisPlates++; \
    } \
    if ( PLTDATA0) free( PLTDATA0); \
    free( METCONTENT); \
  }

  /* update the met database ... */

  LOADDATUMSTR( metKeyDatum, &metKey);
  gdbm_store( gMF, metKeyDatum, newMetContentDatum, GDBM_REPLACE);

  /* and the plate database */

  UPDATEDB( newPltData0, newMetContent) /* updated contents have metKey added */
  UPDATEDB( oldPltData0, oldMetContent)   /* updated contents have it removed */

  gdbm_sync( gMF); gdbm_sync( gPF);

  return 0;
} /* orbdb_addMet */

/********************************************************************/
/* remove met/instr key pair & associated plates from met & plate db's
 * - get met & instr from structure *met2Del
 */
void
orbdb_delMet( const ORBDB_FILES *orbdb_files  /* met db file, plate db file */
            , const metCONTENT *met2Del) {
datum metKeyDatum, pltKeyDatum, oldMetContentDatum;
metKEY metKey;
metKEY *metKeyPtr;
metCONTENT *oldMetContent;
pltKEY *oldVisPlates, *endOldVisPlates;
datum *oldPltData0, *oldPltData;

  if ( !orbdb_files || !met2Del) return;

  orbdb_loadMetKey( &metKey, met2Del->_met, met2Del->_instr);

  /* look at existing entry for this key, may need to update plate database
   */

  /* oldMetContentDatum = gdbm_fetch( gMF, metKeyDatum);
  /* oldMetContent = (metCONTENT *) oldMetContentDatum.dptr;
  /**/
  oldMetContent = orbdb_fetchMetContentByKey( orbdb_files, &metKey);

  if ( !oldMetContent) return;

  /*XVISPLATESPTR( oldMetContent);                   /* fix _visPlates ptr */
  oldVisPlates = oldMetContent->_visPlates;
  endOldVisPlates = oldVisPlates + oldMetContent->_nVisPlates;

  /* allocate datum structure for each plate in old met content so plate db
   * can be updated (metKey removed from oldMetContent->_visPlates[N] plate
   * content)
   */
  if ( oldMetContent->_nVisPlates > 0) {
    oldPltData0 = (datum *) malloc( oldMetContent->_nVisPlates * sizeof(datum));
  } else {
    oldPltData0 = (datum *) NULL;
  }
  oldPltData = oldPltData0;

  /* get updated plate content structures & update plate database
   * - REMOVEOLD & UPDATDB macros defined in orbdb_addMet above
   */
  while ( oldVisPlates < endOldVisPlates) { REMOVEOLD; }

  UPDATEDB( oldPltData0, oldMetContent)

  /* and update the met database by removing metKey */

  LOADDATUMSTR( metKeyDatum, &metKey); /**/
  gdbm_delete( gMF, metKeyDatum);

  gdbm_sync( gMF); gdbm_sync( gPF);

  return;
} /* orbdb_delMet */

/***************************************************************/
/* save number of plates under plate key in macro NUMPLATESKEY */

#define NUMPLATESKEY "NUMPLATES"

void orbdb_setNumPlates( const ORBDB_FILES *orbdb_files, long numPlates) {
datum kDatum, cDatum;
  if ( !orbdb_files) return;
  kDatum.dsize = strlen(NUMPLATESKEY);
  kDatum.dptr = NUMPLATESKEY;
  cDatum.dsize = sizeof(long);
  cDatum.dptr = (char *) &numPlates;
  gdbm_store( gPF, kDatum, cDatum, GDBM_INSERT);          /* DO NOT OVERWRITE */
  return;
}

/************************/
/* get number of plates 
 * - return number of plates
 * - return 0 if # of plates not in plt DB
 * - return -1 for other error/inconsistency
 */
long orbdb_getNumPlates( const ORBDB_FILES *orbdb_files) {
datum kDatum, cDatum;
long numPlates;

  if ( !orbdb_files) return -1;
  kDatum.dsize = strlen(NUMPLATESKEY);
  kDatum.dptr = NUMPLATESKEY;
  cDatum = gdbm_fetch( gPF, kDatum);
  if ( !cDatum.dptr) return 0;
  if ( cDatum.dsize != sizeof(long)) {
    numPlates = -1;
  } else {
    numPlates = *((long *) cDatum.dptr);
  }

# ifdef _ORBDB_SIMPLETEST_
  {
  char *savPtr = cDatum.dptr;
    cDatum.dptr = malloc( cDatum.dsize);
    memcpy( cDatum.dptr, savPtr, cDatum.dsize);
    orbdbTestRealFree( savPtr);
  }
# endif

  free( cDatum.dptr);
  return numPlates;
}

/********************************************************************/
void orbdb_clearState( ORBDB_STATE *gState) {
  if ( !gState) return;
  if ( !*gState) return;
  free( *gState);
  *gState = (ORBDB_STATE) NULL;
  return;
}

/********************************************************************/
/* procedure to step through keys, saving state in **gState
 * - if input *gState is NULL, allocate *gState & init **gState as necessary
 * - when keys are done, free *gState
 * - ***N.B. the returned key is referred to by **gState (e.g. (**gState).dptr
 *           for gdbm), and it is the users responsibility to free it, but only 
 *           AFTER the next use of gState via Nextkey or Clearkey
 */
char *orbdb_genericNextKey( const GDBM_FILE gGF, ORBDB_STATE *gState){

  if ( !gState) return (char *) NULL;
  if ( !(*gState)) {
    *gState = (ORBDB_STATE) malloc( sizeof(datum));
    if ( !(*gState) ) return (char *) NULL;
    *((datum *)*gState) = gdbm_firstkey( gGF);
  } else {
    *((datum *)*gState) = gdbm_nextkey( gGF, *((datum *)*gState) );
  }

  /* if this is key whose datum contains the number of plates */

  if ( (((datum *)*gState)->dsize == strlen(NUMPLATESKEY))
    && ((datum *)*gState)->dptr) {
    if ( !strncmp( NUMPLATESKEY, ((datum *)*gState)->dptr
                 , strlen(NUMPLATESKEY))
       ) {
      *((datum *)*gState) = gdbm_nextkey( gGF, *((datum *)*gState) );
    }
  }

  if ( !((datum *)*gState)->dptr) {
    orbdb_clearState( gState);
    return (char *) NULL;
  }
  return ((datum *)*gState)->dptr;
}

/*******************************************************/
#define KEYFUNCS( GNEXTKEY, GFIRSTKEY, GKEY, GFILE) \
\
GKEY *GNEXTKEY( const ORBDB_FILES *orbdb_files \
                            , ORBDB_STATE *gState) { \
  if ( !orbdb_files) return (GKEY *) NULL; \
  return (GKEY *) orbdb_genericNextKey( GFILE, gState); \
} \
\
GKEY *GFIRSTKEY( const ORBDB_FILES *orbdb_files \
                             , ORBDB_STATE *mState) { \
  if ( !mState) return (GKEY *) NULL; \
  *mState = (ORBDB_STATE) NULL; \
  return (GKEY *) orbdb_genericNextKey( GFILE, mState); \
}

KEYFUNCS( orbdb_metNextkey, orbdb_metFirstkey, metKEY, gMF)
KEYFUNCS( orbdb_pltNextkey, orbdb_pltFirstkey, pltKEY, gPF)
  
/*****************************************************/

#define NEWMODE S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH

/****************************************************/
/* orbdb_close */
void
orbdb_close( ORBDB_FILES *orbdb_files) {
  if ( !orbdb_files) return;
  gdbm_close( gMF);
  gdbm_close( gPF);
  if ( orbdb_files_real->_iAmMallocedByOrbdb) free(orbdb_files);
  return;
} /* orbdb_close( ORBDB_FILES *orbdb_files) { */

/****************************************************/
/* orbdb_open
 *   nameSpec    - file name of met or plate orbdb file, or prefix of either
 *                 - met & plt suffices are in orbdb.h; they are 
 *                   "_orbdbMet.db" & "_orbdPlt.db", as of 04.Nov,1999
 *   rw          - read_write per gdbm_open(); see gdbm.h
 *   orbdb_files - pointer to structure store GDBM_FILE pointers
 *                 - normally user specifies NULL to have orbdb routines
 *                   malloc on open and free on close
 *                 - user sees (void *), #define _ORBDB_PRIVATE_H_ before
 *                   #include "orbdb.h" to see typedef of ORBDB_FILES_REAL
 *
 *   RETURN VALUE
 *     - (ORBDB_FILES *) pointer to structure with GDBM_FILE pointers
 *     - NULL for failure
 */
ORBDB_FILES *
orbdb_open( const char *nameSpec, int rw, ORBDB_FILES *orbdb_files) {
#define MAXFULLNAMELEN 2048
char fullName[MAXFULLNAMELEN];
char *sfxPtr, *endPtr;
GDBM_FILE metFile, pltFile;

  /* open files:  path must end in _orbdbMet.db or _orbdbPlt.db
   *              - the input argument string nameSpec may satisfy either 
   *                of those, otherwise it is assumed to be a prefix
   */
  strncpy( fullName, nameSpec, MAXFULLNAMELEN);
  fullName[MAXFULLNAMELEN-1] = '\0';
  endPtr = fullName + strlen(fullName);/* set suffix pointer to end of string */

  /* put sfxPtr at start of met- or plt- db file suffix: */

  sfxPtr = endPtr - strlen(ORBDB_MET_DBFILE_SUFFIX);          /* setup for    */
  if ( sfxPtr < fullName) sfxPtr = fullName;                  /* met sfx test */
  if ( strcmp( sfxPtr, ORBDB_MET_DBFILE_SUFFIX)) {     /* if true, no met sfx */
    sfxPtr =  endPtr - strlen(ORBDB_PLT_DBFILE_SUFFIX);       /* setup for    */
    if ( sfxPtr < fullName) sfxPtr = fullName;                /* plt sfx test */
    if ( strcmp( sfxPtr, ORBDB_PLT_DBFILE_SUFFIX)) {   /* if true, no plt sfx */
      sfxPtr = endPtr;        /* neither suffix found, append suffices at end */
    }
  }
  /* append met sfx */
  strncpy( sfxPtr, ORBDB_MET_DBFILE_SUFFIX, MAXFULLNAMELEN-(sfxPtr-fullName));
  fullName[MAXFULLNAMELEN-1] = '\0';
  metFile = gdbm_open( fullName, 512, rw, NEWMODE, 0);           /* open file */

  /* append plt sfx */
  strncpy( sfxPtr, ORBDB_PLT_DBFILE_SUFFIX, MAXFULLNAMELEN-(sfxPtr-fullName));
  fullName[MAXFULLNAMELEN-1] = '\0';
  pltFile = gdbm_open( fullName, 512, rw, NEWMODE, 0);           /* open file */

  /* malloc orbdb_files if required & set malloced-by-me flag */

  if ( metFile && pltFile && !orbdb_files) {
    if ( (orbdb_files=(ORBDB_FILES *)malloc(sizeof(ORBDB_FILES_REAL))) ) {
      orbdb_files_real->_iAmMallocedByOrbdb = 1;
    }
  } else if ( orbdb_files) {           /* otherwise clear malloced-by-me flag */
    orbdb_files_real->_iAmMallocedByOrbdb = 0;
  }

  if ( metFile && pltFile && orbdb_files)   { /* load structure if successful */
    gMF = metFile;
    gPF = pltFile;

  } else {                                           /* cleanup after failure */
    if ( metFile) gdbm_close( metFile);
    if ( pltFile) gdbm_close( pltFile);
    orbdb_files = (ORBDB_FILES *) NULL;            /* should not have to free */
  }

  return orbdb_files;
} /* orbdb_open( const char *nameSpec, int rw, ORBDB_FILES *orbdb_files){*/

/*************************/

/* ORBDB_FILES *
/* orbdb_openNew( const char *nameSpec, ORBDB_FILES *orbdb_files) {
/* ORBDB_FILES *ofs = orbdb_open( nameSpec, GDBM_NEWDB, orbdb_files);
  /* return ofs;
/* }
/* */

#define OPENFUNC(NAME,RW) \
ORBDB_FILES *\
NAME( const char *nameSpec, ORBDB_FILES *orbdb_files) { \
  return orbdb_open( nameSpec, RW, orbdb_files); \
}

OPENFUNC( orbdb_openNew, GDBM_NEWDB|GDBM_FAST)
OPENFUNC( orbdb_openForWrite, GDBM_WRCREAT|GDBM_FAST)
OPENFUNC( orbdb_openForRead, GDBM_READER)

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/* test code for orbdb */

#ifdef _ORBDB_SIMPLETEST_

#define FP(A) fprintf A; fflush(stdout)
#define FP1(S) FP((stdout, "%s", S))
#define FPDOT FP1(".")
#define FPOK FP1(" ok\n")
#define FPERR( S) FP((stdout, "\n***%s\n")); return -1

/**********************************/
/* malloc debugger / leak checker */

#undef malloc
#undef free

#define LEAKCHK(N) \
FP((stdout, "  %ld memory leak%s\n", numMallocs-N, ((numMallocs-N)==1)?"":"s"))

static void **testPtrs;
static long nTestPtrs, numMallocs;

void orbdbBumpTestMalloc(void) {
long newNTestPtrs = nTestPtrs ? (2*nTestPtrs) : 2048;
  testPtrs = (void **) realloc( testPtrs, sizeof(void *) * newNTestPtrs);
  if ( !nTestPtrs) numMallocs = 0;
  while ( nTestPtrs < newNTestPtrs) { testPtrs[nTestPtrs++] = (void *) NULL; }
  return;
}

void *orbdbTestMalloc( size_t size) {
long i;
  for ( i=0; i<nTestPtrs; ++i) { if ( !testPtrs[i]) break; }
  if ( i==nTestPtrs) orbdbBumpTestMalloc();
  if ( (testPtrs[i]=malloc( size)) ) {
    /* FP1("<"); */
    numMallocs++;
  }
  return testPtrs[i];
}

void orbdbTestFree(void *ptr) {
long i;
  for ( i=0; i<nTestPtrs; ++i) { if ( ptr == testPtrs[i]) break; }
  if ( i < nTestPtrs) {
    testPtrs[i] = (void *) NULL;
    /* FP1(">"); */
    numMallocs--;
  } else {
    /* there are other routines that use the real malloc */
    FP(( stdout, "***Unknown pointer: orbdbTestFree(ptr=%08lxx)\n",(long)ptr));
    /**/
  }
  free( ptr);
}

void orbdbTestRealFree( void *ptr) { free( ptr); }

/********************************/

int
main(int argc, char **argv) {
#ifndef TESTCOUNT
#define TESTCOUNT 50L
#endif
double metArr[TESTCOUNT];
long instrArr[TESTCOUNT];
long instrParArr[TESTCOUNT];
long nVisPlatesArr[TESTCOUNT];
long visPlatesLongsArr[TESTCOUNT][TESTCOUNT];
long skipIArr[TESTCOUNT];

long visPlatesUnique[TESTCOUNT*TESTCOUNT];
long nUniquePlates;

#define ofs orbdb_files
ORBDB_FILES *ofs;

/* datum mKD, mCD; */
ORBDB_STATE mState, pState;
metCONTENT metCTmp;
metCONTENT *mC;
metKEY *mK, *mK2;

/* datum pKD; */
pltCONTENT *pC;
pltKEY *pK, *pK2;

long i, j;
long nPltKeys;
long iUP;

double offset = 0.0;
long testCount = TESTCOUNT;
char orbdbName[200];
char *cPtr;

  *orbdbName = orbdbName[99] = '\0';
  while ( --argc ) {
    argv++;
    if (sscanf( *argv,"-o%lf",&offset) ) FP((stdout,"offset:%s\n", *argv));
    if (sscanf( *argv,"-t%ld",&testCount)){FP((stdout,"testCount:%s\n",*argv));}
    if (sscanf( *argv,"-n%99s",&orbdbName)){FP((stdout,"name:%s\n",*argv));}
  }
  cPtr = orbdbName + strlen(orbdbName);

  nUniquePlates = 0;

  for ( i=0; i<testCount; ++i) {
    metArr[i] = offset + (1010.0 * i);
    instrArr[i] = (long)metArr[i] + 1;
    instrParArr[i] = instrArr[i] + 1;
    nVisPlatesArr[i] = ((9*i)/10) + 4;
    skipIArr[i] = 0;
    for ( j=0; j<nVisPlatesArr[i]; ++j) { 
    long plateNum;
      plateNum = j * i;
      visPlatesUnique[nUniquePlates] = visPlatesLongsArr[i][j] = plateNum;
      for ( iUP=0; iUP<nUniquePlates; ++iUP) {
        if ( visPlatesUnique[iUP] == plateNum) break;
      }
      if ( iUP == nUniquePlates) nUniquePlates++;
    }
  }
  FP((stdout, "testCount = %ld; # of unique plates = %ld\n"
    , testCount, nUniquePlates));


  /***********************************************/
  FP1("Testing orbdb file open & close routines\n  ");

  strcpy( cPtr,ORBDB_MET_DBFILE_SUFFIX);
  unlink( orbdbName);
  strcpy( cPtr,ORBDB_PLT_DBFILE_SUFFIX);
  unlink( orbdbName);
  *cPtr = '\0';
  if ( (ofs=orbdb_openNew( orbdbName, (ORBDB_FILES *) NULL)) ) {
    FPDOT;
    orbdb_close( ofs);
  } else {
    FPERR( "PROBLEM CREATING/OPENING NEW FILES");
  }

  if ( (ofs=orbdb_openNew( orbdbName, (ORBDB_FILES *) NULL)) ) {
    FPDOT;
    orbdb_close( ofs);
  } else {
    FPERR( "PROBLEM CREATING/OPENING NEW FILES OVER OLD FILES");
  }

  strcpy( cPtr,ORBDB_MET_DBFILE_SUFFIX);
  unlink( orbdbName);
  strcpy( cPtr,ORBDB_PLT_DBFILE_SUFFIX);
  unlink( orbdbName);
  *cPtr = '\0';
  if ( (ofs=orbdb_openForWrite( orbdbName, (ORBDB_FILES *) NULL)) ) {
    FPDOT;
    orbdb_close( ofs);
  } else {
    FPERR( "PROBLEM OPENING NEW FILES");
  }

  if ( (ofs=orbdb_openForRead( orbdbName, (ORBDB_FILES *) NULL)) ) {
    FPDOT;
    orbdb_close( ofs);
  } else { 
    FPERR( "PROBLEM OPENING OLD FILES");
  }

  strcpy( cPtr,ORBDB_MET_DBFILE_SUFFIX);
  ofs = orbdb_openForRead( orbdbName, (ORBDB_FILES *) NULL);
  if ( ofs) {
    FPDOT;
    orbdb_close( ofs);
  } else { 
    FPERR( "PROBLEM OPENING OLD FILES USING MET SUFFIX");
  }

  strcpy( cPtr, ORBDB_PLT_DBFILE_SUFFIX);
  ofs = orbdb_openForRead( orbdbName, (ORBDB_FILES *) NULL);
  if ( ofs) {
    FPDOT;
    orbdb_close( ofs);
  } else { 
    FPERR( "PROBLEM OPENING OLD FILES USING PLT SUFFIX");
  }

  if ( (ofs=orbdb_openForWrite( orbdbName, (ORBDB_FILES *) NULL)) ) {
    FPDOT;
    orbdb_close( ofs);
  } else {
    FPERR( "PROBLEM OPENING NEW FILES");
  }
  FPOK;

  LEAKCHK(0);

  /***********************************/
  FP1("Testing orbdb addMet routine\n  ");

  if ( (ofs=orbdb_openNew( orbdbName, (ORBDB_FILES *) NULL)) ) {
    FP1("Open ");
  } else {
    FPERR( "PROBLEM CREATING/OPENING NEW FILES OVER OLD FILES");
  }

  if ( !orbdb_getNumPlates( ofs)) {                        /* should return 0 */
    FP1( "N");
  } else {
    FPERR( "PROBLEM FINDING NON-EXISTANT NUMBER OF PLATES");
  }

  orbdb_setNumPlates( ofs, (long)(testCount*testCount));   /* set # of plates */
  if ( ((long)(testCount*testCount)) == orbdb_getNumPlates( ofs)) {
    FP1( "P");
  } else {
    FPERR( "PROBLEM SETTING NUMBER OF PLATES");
  }

  orbdb_setNumPlates( ofs, (long) testCount);    /* this should not update DB */
  if ( ((long)(testCount*testCount)) != orbdb_getNumPlates( ofs)) {
    FPERR( "PROBLEM BEING ABLE TO OVERRIDE NUMBER OF PLATES");
  }

# define INITI(I) for ( I=0; I<testCount; ++I) if ( !skipIArr[I]) break
# define INCRI(I)  while ( ++I<testCount) if ( !skipIArr[I]) break

  metCTmp._scVec[0] = metCTmp._scVec[1] = metCTmp._scVec[2] =
  metCTmp._sunVec[0] = metCTmp._sunVec[1] = metCTmp._sunVec[2] =
  metCTmp._eulCam2Abf[0] = metCTmp._eulCam2Abf[1] = metCTmp._eulCam2Abf[2] =
  metCTmp._miscVals[0] = metCTmp._miscVals[1] = metCTmp._miscVals[2] =
  metCTmp._miscVals[3] = metCTmp._miscVals[4] = -999.0;

  INITI(i);
  while ( i<testCount) {
    metCTmp._met = metArr[i];
    metCTmp._instr = instrArr[i];
    metCTmp._instrPar = instrParArr[i];
    metCTmp._nVisPlates = nVisPlatesArr[i];
/* #   define RANGE1(A) (((double)(A)) / (3.0 * (double) testCount)) /**/
#   define RANGE1(A) ( srand(2*(A)+1), (((double)rand()) / RAND_MAX) )
    metCTmp._mu0 = RANGE1(testCount-i);
    metCTmp._mu = RANGE1(2 * i);
    metCTmp._alpha = RANGE1(3 * i);
    metCTmp._resolution = 4 * i;
    j = orbdb_addMet( ofs, &metCTmp, visPlatesLongsArr[i]);

    if ( !j) {
      /* 
      nVisPlatesArr[i]
      = orbdb_qsortUnique( visPlatesLongsArr[i], nVisPlatesArr[i], sizeof(long)
                         , orbdb_compareLongs);
      */
      FPDOT;
    } else {
      FPERR( "PROBLEM W/addMet() ROUTINE"); 
    }
    INCRI(i);
  }
  orbdb_close( ofs);
  FPOK;
  LEAKCHK(0);

  /***************************************************/
  FP1("Testing all keys in MET database\n  ");

# define OPENREAD \
  ofs = orbdb_openForRead( orbdbName, (ORBDB_FILES *) NULL); \
  if ( ofs) { \
    FP1("Open "); \
  } else { \
    FPERR( "PROBLEM OPENING OLD FILES FOR READ"); \
  } \

  OPENREAD

  i=0;
  mK = orbdb_metFirstkey( ofs, &mState);
  while ( mK) {
    FPDOT;
    i++;
    mK2 = mK;
    mK = orbdb_metNextkey( ofs, &mState);
    free( mK2);
  }
  FP((stdout, " %ld keys found;", i));
  orbdb_close( ofs);
  FPOK;
  LEAKCHK(0);

  /***************************************************/
  FP1("Testing all keys in PLT database\n  ");

  OPENREAD

  nPltKeys = 0;
  pK = orbdb_pltFirstkey( ofs, &pState);
  while ( pK) {
    FPDOT;
    nPltKeys++;
    pK2 = pK;
    pK = orbdb_pltNextkey( ofs, &pState);
    free( pK2);
  }
  FP((stdout, " %ld keys found;", nPltKeys));
  orbdb_close( ofs);
  FPOK;
  LEAKCHK(0);

  /***************************************************/
  FP1("Testing orbdb fetchMetContent[ByKey] routines & MET db consistency\n  ");

  OPENREAD

/* # define NESET(VAL,MBR,BIT) if ( (VAL) != mC->MBR) notEqual |= (BIT) /**/
/* # define NESET(VAL,MBR,BIT) \
  if ( ((double)(VAL)) != ((double)mC->MBR)) notEqual |= (BIT) /**/
# define NESET(VAL,MBR,BIT) if ( fabs((VAL)-mC->MBR) > 1e-14) notEqual |= (BIT) /**/
# define NESETARR(ARR,MBR,BIT) NESET( ARR[i],MBR, BIT);

# define METCONSISTENCYCHECK( ERRMSG) \
  for ( j=3; j<=(testCount*3); j+=3) {\
  long notEqual;\
  long jj, dbJ;\
    i = j % testCount;\
    mC = orbdb_fetchMetContent( ofs, metArr[i], instrArr[i]);\
    notEqual = 0;\
    if ( mC && skipIArr[i]) notEqual = -2;\
    else if ( mC) {\
      NESETARR( metArr, _met, 1);\
      NESETARR( instrArr, _instr, 2);\
      NESETARR( instrParArr, _instrPar, 4);\
      NESET( RANGE1(testCount-i), _mu0, 8);\
      NESET( RANGE1(2*i), _mu, 16);\
      NESET( RANGE1(3*i), _alpha, 32);\
      NESET( 4*i, _resolution, 64);\
      if( nVisPlatesArr[i] < mC->_nVisPlates) notEqual |= 128;\
      if ( !notEqual ) {\
      long visPlate;\
        for ( dbJ=0; dbJ<mC->_nVisPlates; ++dbJ) {  /* is each plt in DB ... */\
          visPlate = orbdb_unloadPltKey( mC->_visPlates+dbJ);\
          for ( jj=0; jj<nVisPlatesArr[i]; ++jj) { /*... also in source data?*/\
            if ( visPlate == visPlatesLongsArr[i][jj]) break;\
          }\
          if ( jj == nVisPlatesArr[i]) { notEqual |= (128L<<(1+dbJ)); break; }\
        }\
        for ( jj=0; jj<nVisPlatesArr[i]; ++jj) {/*is each plt in src data ...*/\
          for ( dbJ=0; dbJ<mC->_nVisPlates; ++dbJ) {       /* ... also in DB?*/\
            visPlate = orbdb_unloadPltKey( mC->_visPlates+dbJ);\
            if ( visPlate == visPlatesLongsArr[i][jj]) break;\
          }\
          if ( dbJ == nVisPlatesArr[i]) { notEqual |= (128L<<(1+jj)); break; }\
        }\
      } /* if notEqual */\
    } else if ( !skipIArr[i]) notEqual = -1;\
    if ( notEqual) {\
      FP((stdout, "%08lxx=notEqual", notEqual));\
      FPERR( ERRMSG); \
    } else {\
      FPDOT;\
    }\
    if ( mC) orbdbTestFree( mC);\
  }\
  orbdb_close( ofs);\
  FPOK;\
  LEAKCHK(0)

  METCONSISTENCYCHECK( 
    "PROBLEM W/RETURNED DATA FROM fetchMetContent[ByKey] ROUTINES");

  /***************************************************/
  FP1("Testing orbdb fetchPltContent[ByKey] routines");
  FP1(" & MET<=>PLT DB consistency\n  ");

  OPENREAD

# define METPLTCONSISTENCYCHECK \
  for ( j=0; j<nUniquePlates; j++) {                /* for each unique plate */\
  long iMK, jj;\
    pC = orbdb_fetchPltContent( ofs, visPlatesUnique[j]);/*get plate contents*/\
    if ( !pC ) {\
      FPERR( "PROBLEM W/RETURNED DATA FROM fetchPltContent[ByKey] ROUTINES");\
    }\
    INITI(i);\
    while ( i<testCount) {                                   /* for each MET */\
    metKEY metKey;\
    long iTest;\
      iTest = 0;\
      orbdb_loadMetKey( &metKey, metArr[i], instrArr[i]);    /* load MET key */\
      for ( iMK=0; iMK<pC->_nMetKeys; ++iMK) { /*compare against pC->_metKeys*/\
        if ( !orbdb_compareMetKeys( pC->_metKeys+iMK, &metKey)) {\
          iTest = 1;                         /* MET is under PLT in plate db */\
          break;\
        }\
      }\
      for ( jj=0; jj<nVisPlatesArr[i]; ++jj) {      /* for this MET's plates */\
        if ( visPlatesUnique[j] == visPlatesLongsArr[i][jj]) {/*plt under MET*/\
          if ( !iTest) {             /* but MET is not under plate in PLT db */\
            FPERR( "PROBLEM TYPE MET:  METYES/PLTNO");\
          }\
          iTest = 0;\
          break;\
        }\
      }\
      if ( iTest) {         /* met is under plate in PLT db but plate is not */\
        FPERR( "PROBLEM TYPE PLATE:  METNO/PLTYES");  /* under MET in MET db */\
      }\
      INCRI(i);\
    }\
    if ( pC->_nMetKeys) { FPDOT; } else { FP1("x"); }\
    orbdbTestFree( pC);\
  }\
  orbdb_close( ofs);\
  FPOK;\
  LEAKCHK(0)

  METPLTCONSISTENCYCHECK;

  /***************************************************/
  FP1("Testing orbdb delMet routine; that it maintains DBs' consistencies\n  ");

# define OPENWRITE \
  ofs = orbdb_openForWrite( orbdbName, (ORBDB_FILES *) NULL); \
  if ( ofs) { \
    FP1("Open "); \
  } else { \
    FPERR( "PROBLEM OPENING OLD FILES FOR WRITE"); \
  } \

  OPENWRITE

# define DELMET( I) \
  metCTmp._met = metArr[I]; \
  metCTmp._instr = instrArr[I]; \
  orbdb_delMet( ofs, &metCTmp); \
  skipIArr[I] = 1;

  /* delete first ([0]), seventh ([6]) & last ([testCount-1]) met's */

  DELMET(0); DELMET(testCount/2); DELMET(testCount-1);

  /* repeat consistency check w/o *Arr[0], *Arr[6] & *Arr[testCount-1] */

  METPLTCONSISTENCYCHECK;

  /*********************************************/
  FP1("Testing orbdb addMet routine; that it maintains DBs' consistencies\n  ");

  OPENWRITE

  /* in last non-skipped MET of source data ... */

  for ( i=testCount-1; i>0; --i) if ( !skipIArr[i]) break;

  /* ... add 1 to each plate number, update visPlatesUnique, nUniquePlates */

  for ( j=0; j<nVisPlatesArr[i]; j++) {
  long visPlate;
    visPlate = visPlatesUnique[nUniquePlates] = ++visPlatesLongsArr[i][j];
    for ( iUP=0; iUP<nUniquePlates; ++iUP) {
      if ( visPlate == visPlatesUnique[iUP]) break;
    }
    if ( iUP == nUniquePlates) nUniquePlates++;
  }

  /* add this MET */

  metCTmp._met = metArr[i];
  metCTmp._instr = instrArr[i];
  metCTmp._instrPar = instrParArr[i];
  metCTmp._nVisPlates = nVisPlatesArr[i];
  metCTmp._mu0 = i;
  metCTmp._mu = 2 * i;
  metCTmp._alpha = 3 * i;
  metCTmp._resolution = 4 * i;
  j = orbdb_addMet( ofs, &metCTmp, visPlatesLongsArr[i]);
  if ( !j) { FP1("A"); } else { FPERR( "PROBLEM W/addMet() ROUTINE"); }

  METPLTCONSISTENCYCHECK;

  return 0;

} /* main */
#endif /* _ORBDB_SIMPLETEST_ */
