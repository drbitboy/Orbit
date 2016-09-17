/* orbdb.h */
#ifndef _ORBDB_H_
#define _ORBDB_H_

/******************************/
/* orbdb private              */

#ifdef _ORBDB_PRIVATE_H_

#include <gdbm.h>

typedef struct {
  GDBM_FILE _metFile;
  GDBM_FILE _pltFile;
  int _iAmMallocedByOrbdb;
  int _userInt;
} ORBDB_FILES_REAL;

typedef datum *ORBDB_STATE_REAL;

#define orbdb_files_real ((ORBDB_FILES_REAL *)orbdb_files)
#define gMF orbdb_files_real->_metFile
#define gPF orbdb_files_real->_pltFile
#define orbdb_state_real ((ORBDB_STATE_REAL *)orbdb_state)

#endif /* _ORBDB_PRIVATE_H_ */

/* end orbdb private          */
/******************************/

#define ORBDB_MET_DBFILE_SUFFIX "_met.orbdb"
#define ORBDB_PLT_DBFILE_SUFFIX "_plt.orbdb"

typedef void ORBDB_FILES;
typedef void *ORBDB_STATE;

typedef long pltKEY;     /* plate key is only the plate number */

typedef struct {
  double _metKeyVal;      /* MET truncated to some resolution */
  long _instr;            /* instrument */
} metKEY;

typedef struct {           /* MET content, pointer will go in ((datum)X).dptr */
  long _dSize;           /* will go in ((datum)X).dsize */
  long _instr;           /* instrument */
  double _met;           /* real SPICE MET */
  double _sunVec[3];     /* unit vector to sun */
  double _scVec[3];      /* space craft position wrt body center */
  double _eulCam2Abf[3]; /* euler angles, "camera" to ABF (SPICE), radians */
  double _resolution;    /* avg pixel/km */
  double _miscVals[5];   /* misc values, init to -999.0 */
  double _mu0, _mu;      /* avg cos(inc), avg cos(emi) */
  double _alpha;         /* boresight cos(phase) */
  long _instrPar;        /* instrument parameter e.g. mirror position, filter */
  long _nVisPlates;      /* # of plates visible in observation */
  pltKEY *_visPlates;    /* pointer to those plates' ids, MUST point to */
                         /*   (long *)(((metCONTENT *)(mcPtr))+1) */
} metCONTENT;

typedef struct {
  long _dSize;
  long _nMetKeys;                      /* # of metKEY's containing this plate */
  metKEY *_metKeys;               /* pointer to those metKEY's, MUST point to */
  double _dumyAlign;   /* force alignment of structure so metKEY's will be ok */
} pltCONTENT;                     /*   (metKEY *)(((pltCONTENT *)(pltPtr))+1) */

#define FIXCONTENTPTR(CTYP,C,TYP,LPTR) \
  ((CTYP *)(C))->LPTR = (TYP *) (((CTYP *)(C))+1)
#define FIXVISPLATESPTR(C) FIXCONTENTPTR(metCONTENT,C,pltKEY,_visPlates)
#define FIXMETKEYSPTR(C) FIXCONTENTPTR(pltCONTENT,C,metKEY,_metKeys)

/* convert near met to seconds, truncate to nearest 1 second */
#define TRUNCMET_NEAR( MET) ( ((MET)-(fmod(MET,1000.0))) / 1000.0 )
/* - was 2 seconds, changed it 09.dec'1999 */
/* #define TRUNCMET_NEAR( MET) ( ((MET)-(fmod(MET,2000.0))) / 1000.0 ) */

void orbdb_loadMetKey( metKEY *metKey, double met, long instr);
void orbdb_loadPltKey( pltKEY *pltKey, long plt);
int orbdb_comparePltKeys( const void *v1, const void *v2);
int orbdb_compareMetKeys( const void *v1, const void *v2);
size_t orbdb_qsortUnique( void *base, size_t nmemb, size_t membSize
                        , int(*compar)(const void *, const void *));

metCONTENT *orbdb_mallocMetContent( const metCONTENT *metSrc, long nVisPlates);
pltCONTENT *orbdb_mallocPltContent( const pltCONTENT *pltSrc, long nMetKeys);
void orbdb_loadMetData( metCONTENT *, const pltKEY *);
void orbdb_loadPltData( pltCONTENT *, const metKEY *);

void orbdb_loadMetContent( metCONTENT *metDest
                         , const metCONTENT *metSrc
                         , const pltKEY *visPlates
                         );
metCONTENT* orbdb_newMetContent( const metCONTENT *metSrc
                               , const pltKEY *visPlates
                               , long nVisPlates
                               );
pltCONTENT* orbdb_removeMetKeyFromPlate( const ORBDB_FILES *of
                                       , const metKEY *metKey
                                       , const pltKEY *pltKeyPtr);
pltCONTENT* orbdb_addMetKeyToPlate( const ORBDB_FILES *of
                                      , const metKEY *metKey
                                      , const pltKEY *pltKeyPtr);

/**************************/
/* user interfaces follow */
/**************************/

long orbdb_unloadPltKey( const pltKEY *pltKey);
int orbdb_addMet( const ORBDB_FILES *of
                , const metCONTENT *metSrc
                , const long *visPlatesLongs
                );
void orbdb_delMet( const ORBDB_FILES *of
                 , const metCONTENT *metContent
                 );
metCONTENT *orbdb_fetchMetContentByKey( const ORBDB_FILES *of
                                      , const metKEY *key
                                      );
pltCONTENT *orbdb_fetchPltContentByKey( const ORBDB_FILES *of
                                      , const pltKEY *key
                                      );
metCONTENT *orbdb_fetchMetContent( const ORBDB_FILES *of
                                 , double met, long instr
                                 );
pltCONTENT *orbdb_fetchPltContent( const ORBDB_FILES *of
                                 , long plateNum
                                 );

metKEY *orbdb_metFirstkey( const ORBDB_FILES *of
                         , ORBDB_STATE *mState
                         );
metKEY *orbdb_metNextkey( const ORBDB_FILES *of
                        , ORBDB_STATE *mState
                        );
pltKEY *orbdb_pltFirstkey( const ORBDB_FILES *of
                         , ORBDB_STATE *pState
                         );
pltKEY *orbdb_pltNextkey( const ORBDB_FILES *of
                        , ORBDB_STATE *pState
                        );

long orbdb_getNumPlates( const ORBDB_FILES *of);
void orbdb_setNumPlates( const ORBDB_FILES *of, long numPlates);

void orbdb_close( ORBDB_FILES *of
                     );
ORBDB_FILES *orbdb_open( const char *nmSpc
                            , int rw, ORBDB_FILES *of
                            );
ORBDB_FILES *orbdb_openNew( const char *nmSpec
                               , ORBDB_FILES *of
                               );
ORBDB_FILES *orbdb_openForWrite( const char *nmSpec
                                    , ORBDB_FILES *of
                                    );
ORBDB_FILES *orbdb_openForRead( const char *nmSpec
                                   , ORBDB_FILES *of
                                   );

#endif /* _ORBDB_H_ */ /* end orbdb.h */
