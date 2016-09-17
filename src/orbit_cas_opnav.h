/* orbit_cas_opnav.h */

#ifndef _ORBIT_CAS_OPNAV_H_

/* opnav types */

enum {
  OPNAVTYPE_A, OPNAVTYPE_B, OPNAVTYPE_BP
, OPNAVTYPE_C, OPNAVTYPE_CACRUISE, OPNAVTYPE_CE, OPNAVTYPE_CT
, OPNAVTYPE_D
, OPNAVTYPE_E, OPNAVTYPE_E2, OPNAVTYPE_E3, OPNAVTYPE_E4
, OPNAVTYPE_F, OPNAVTYPE_G, OPNAVTYPE_H, OPNAVTYPE_I, OPNAVTYPE_J
, OPNAVTYPE_K
, OPNAVTYPE_DOUBLE_K
, OPNAVTYPE_COUNT };

static char *opnavNames[] = {
  "CAS_OPNAV_A", "CAS_OPNAV_B", "CAS_OPNAV_BP"
, "CAS_OPNAV_C", "CAS_OPNAV_C_A_CRUISE", "CAS_OPNAV_CE"
, "CAS_OPNAV_CT"
, "CAS_OPNAV_D"
, "CAS_OPNAV_E", "CAS_OPNAV_E_2", "CAS_OPNAV_E_3", "CAS_OPNAV_E_4"
, "CAS_OPNAV_F", "CAS_OPNAV_G", "CAS_OPNAV_H", "CAS_OPNAV_I"
, "CAS_OPNAV_J", "CAS_OPNAV_K"
, "CAS_OPNAV_DOUBLE_K"
};

enum {
  OPNAVARG_PRIO
,     OPNAVARG_SLEWDUR
,         OPNAVARG_STARTNTHSLEW
,             OPNAVARG_DS40COORDSYS
,                 OPNAVARG_DS40AIMXYZ
,                     OPNAVARG_DS40AIMSHORT
,                         OPNAVARG_DS40BOREXYZ
,                             OPNAVARG_DS56
,                                 OPNAVARG_FRAMES
,                                     OPNAVARG_IM_TYPE
,                                         OPNAVARG_IM_DELTA
,                                             OPNAVARG_MISCINT
#                                             define OPNAVARG_COUNT \
                                                     (OPNAVARG_MISCINT+1)
,                                                 OPNAVFRAG_SHOOT
,                                                     OPNAVFRAG_MSISR
,                                                         OPNAVFRAG_DS40
,                                                             OPNAVFRAG_DS56
,                                                                 OPNAVFRAG_ATT
, OPNAVARGFRAG_COUNT };

static long opnavArgFragCount[OPNAVTYPE_COUNT][OPNAVARGFRAG_COUNT] = {
  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1 /* A */
, 1,  1,  0,  1,  1,  0,  1,  0,  0,  1,  0,  0,  1,  0,  1,  0,  1 /* B */
, 1,  1,  0,  1,  1,  0,  1,  0,  0,  1,  0,  0,  1,  0,  1,  0,  1 /* BP */
, 1,  2,  1,  2,  2,  2,  2,  0,  0,  5,  0,  0,  1,  0,  2,  1,  1 /* C */
, 0,  2,  1,  2,  2,  2,  2,  0,  0,  5,  0,  0,  1,  1,  3,  2,  1 /* C_A... */
, 1,  2,  1,  2,  2,  2,  2,  0,  0,  5,  0,  0,  1,  0,  2,  1,  1 /* C_E */
, 1,  2,  1,  2,  2,  2,  2,  0,  0,  5,  0,  0,  1,  0,  2,  1,  1 /* C_T */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  4,  0,  0,  1,  0,  1,  1,  1 /* D */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  1,  0,  0,  1,  0,  1,  0,  1 /* E */
, 1,  2,  0,  2,  2,  2,  2,  0,  0,  1,  0,  0,  1,  0,  2,  0,  1 /* E_2 */
, 1,  3,  0,  3,  3,  3,  3,  0,  0,  1,  0,  0,  1,  0,  3,  0,  1 /* E_3 */
, 1,  4,  0,  4,  4,  4,  4,  0,  0,  1,  0,  0,  1,  0,  4,  0,  1 /* E_4 */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  6,  0,  0,  1,  0,  1,  1,  1 /* F */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  6,  0,  0,  1,  0,  1,  1,  1 /* G */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  9,  0,  0,  1,  0,  1,  1,  1 /* H */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  2,  0,  0,  1,  0,  1,  1,  1 /* I */
, 1,  1,  0,  1,  1,  1,  1,  0,  0,  2,  0,  0,  1,  0,  1,  1,  1 /* J */
, 1,  1,  0,  1,  1,  1,  1,  1,  1,  12, 12, 0,  1,  0,  1,  1,  1 /* K */
, 1,  2,  0,  2,  2,  2,  2,  2,  2,  2,  24, 1,  2,  0,  2,  2,  1 /*DOUBLE_K*/
/*PRI SLW SSL SYS AIM SHR BOR D56 FRM IMT IMD MSC SHO SR  D40 D56 ATT */
};

/* set limits on number of arguments in SATF & on numbers of fragments in 
 * SATF (SATF parameters OR number of fragments in STEPS)
 */
#define OPNAVMAX_SLEWDUR 4
#define OPNAVMAX_PRIO 1
#define OPNAVMAX_STARTNTHSLEW 1
#define OPNAVMAX_MISCINT 1
#define OPNAVMAX_SHOOT 2    /* each w/up to 12 img exe's incl. types & deltas */
#define OPNAVMAX_MSISR 1
#define OPNAVMAX_DS40 4
#define OPNAVMAX_DS56 2             /* one max in params, but 2 in some STEPS */
#define OPNAVMAX_NAVRELATT 1

typedef struct CASOPNAVstr {      /* struct to hold OPNAV-related CASes/FRAGs */
  long _subType;                                  /* _A, _B, _BP, _C, _C_A... */
  long _slewDur[OPNAVMAX_SLEWDUR];
  long _prio[OPNAVMAX_PRIO];
  long _bits[OPNAVMAX_PRIO];
  long _startNthSlew[OPNAVMAX_STARTNTHSLEW];
  long _miscInt[OPNAVMAX_MISCINT];
  long _imDeltas[24];           /* buffer for OPNAV_*K (SHOOT *)->msiShootDel */
  long _letMyPeopleGo;                         /* if 1, ok to delete children */
  struct CASstr *_shoot[OPNAVMAX_SHOOT];/* dim [1] so if there are more added */
  struct CASstr *_msiSR[OPNAVMAX_MSISR]; /* later, we will not have to change */
  struct CASstr *_ds40[OPNAVMAX_DS40];   /* the syntax of existing statements */
  struct CASstr *_ds56[OPNAVMAX_DS56];
  struct CASstr *_navRelAtt[OPNAVMAX_NAVRELATT];
} CASOPNAV;

#define opnavSubType _data._opnavCAS->_subType
#define opnavSlewDur _data._opnavCAS->_slewDur
#define opnavPrio _data._opnavCAS->_prio
#define opnavBits _data._opnavCAS->_bits
#define opnavStartNthSlew _data._opnavCAS->_startNthSlew
#define opnavMiscInt _data._opnavCAS->_miscInt
#define opnavImDeltas _data._opnavCAS->_imDeltas
#define opnavLetMyPeopleGo _data._opnavCAS->_letMyPeopleGo
#define opnavShoot _data._opnavCAS->_shoot
#define opnavMsiSR _data._opnavCAS->_msiSR
#define opnavDs40 _data._opnavCAS->_ds40
#define opnavDs56 _data._opnavCAS->_ds56
#define opnavNavRelAtt _data._opnavCAS->_navRelAtt

#define CASOPNAV_SCHED_POINT "CASOPNAV_SCHED_POINT"
#define CASOPNAV_START_DELAY "CASOPNAV_START_DELAY"

/* PRIORITY:  FALSE/TRUE => 0/1
 * - duplicate 1/0 for backward compatibility
 */
static long opnavPrioArray[] = { 0L, 1L, 0L, 1L, -1L };
static char *opnavPrioNames[] = { "FALSE", "TRUE", "0", "1", (char *) 0 };

/* macro to combine opnavSubType with type for OPNAV CASes */

#define CASTYPE(CAS) \
 (isCASOPNAV(*(CAS)) ? (CAS)->opnavSubType+CASTYPE_COUNT : (CAS)->_type)

/*************************************************/
/* opnav satf/sasf argument order lists & tables */

/* OPNAVARGLIST - argument list for a single OPNAVTYPE */

typedef struct { 
  long _argId;    /* OPNAVARG_*, -1 to terminate */
  long _argCount; /* ***N.B. ONE-BASED */
                  /* >0 => 1 item, use <array>[._argCount-1] */
                  /* <0 => multiple items, use [0] through [(-._argCount)-1] */
} OPNAVARGLIST;

/* OPNAVARGTABLE - argument list with an identifying OPNAVTYPE
 * - code will work with an array of these i.e. (OPNAVARGTABLE *)
 */
typedef struct {
  long _opnavType;        /* OPNAVTYPE_*; -1 to terminate */
  OPNAVARGLIST *_argList;
} OPNAVARGTABLE;

static OPNAVARGLIST opnavArgList_A[] = { 
  OPNAVARG_SLEWDUR, 1
, OPNAVARG_PRIO, 1
, -1, -1 };

static OPNAVARGLIST opnavArgList_B[] = { 
  OPNAVARG_SLEWDUR, 1
, OPNAVARG_PRIO, 1
, OPNAVARG_IM_TYPE, 1
, OPNAVARG_DS40COORDSYS, 1, OPNAVARG_DS40AIMXYZ, 1
, OPNAVARG_DS40BOREXYZ, 1
, -1, -1 };

#define opnavArgList_BP opnavArgList_B  /* _B & _BP have the same arg list */

#define OPNAVARGLISTDS40MACRO( I) \
  OPNAVARG_SLEWDUR, I \
, OPNAVARG_DS40COORDSYS, I, OPNAVARG_DS40AIMXYZ, I \
, OPNAVARG_DS40AIMSHORT, I, OPNAVARG_DS40BOREXYZ, I

static OPNAVARGLIST opnavArgList_C[] = { 
  OPNAVARG_PRIO, 1
, OPNAVARGLISTDS40MACRO( 1)
, OPNAVARG_STARTNTHSLEW, 1
, OPNAVARGLISTDS40MACRO( 2)
, OPNAVARG_IM_TYPE, -5
, -1, -1 };

/* _C* opnavs have similar argument lists */

#define opnavArgList_CACRUISE (opnavArgList_C+1)   /* same as _C w/o priority */
#define opnavArgList_CE opnavArgList_C
#define opnavArgList_CT opnavArgList_CE

#define OPNAVARGLIST_DFGHIJMACRO( N)           /* prio, slew, ds40, imtyp*N */ \
  OPNAVARG_PRIO, 1, OPNAVARGLISTDS40MACRO( 1), OPNAVARG_IM_TYPE, -(N)

static OPNAVARGLIST opnavArgList_D[] = { 
  OPNAVARGLIST_DFGHIJMACRO( 4)
, -1, -1 };

static OPNAVARGLIST opnavArgList_E[] = { 
  OPNAVARG_PRIO, 1, OPNAVARG_IM_TYPE, 1
, OPNAVARGLISTDS40MACRO( 1)
, -1, -1 };

static OPNAVARGLIST opnavArgList_E2[] = { 
  OPNAVARG_PRIO, 1, OPNAVARG_IM_TYPE, 1
, OPNAVARGLISTDS40MACRO( 1)
, OPNAVARGLISTDS40MACRO( 2)
, -1, -1 };

static OPNAVARGLIST opnavArgList_E3[] = { 
  OPNAVARG_PRIO, 1, OPNAVARG_IM_TYPE, 1
, OPNAVARGLISTDS40MACRO( 1)
, OPNAVARGLISTDS40MACRO( 2)
, OPNAVARGLISTDS40MACRO( 3)
, -1, -1 };

static OPNAVARGLIST opnavArgList_E4[] = { 
  OPNAVARG_PRIO, 1, OPNAVARG_IM_TYPE, 1
, OPNAVARGLISTDS40MACRO( 1)
, OPNAVARGLISTDS40MACRO( 2)
, OPNAVARGLISTDS40MACRO( 3)
, OPNAVARGLISTDS40MACRO( 4)
, -1, -1 };

static OPNAVARGLIST opnavArgList_F[] = {
  OPNAVARGLIST_DFGHIJMACRO( 6)
, -1, -1 };

static OPNAVARGLIST opnavArgList_G[] = { 
  OPNAVARGLIST_DFGHIJMACRO( 6)
, -1, -1 };

static OPNAVARGLIST opnavArgList_H[] = { 
  OPNAVARGLIST_DFGHIJMACRO( 9)
, -1, -1 };

static OPNAVARGLIST opnavArgList_I[] = { 
  OPNAVARGLIST_DFGHIJMACRO( 2)
, -1, -1 };

static OPNAVARGLIST opnavArgList_J[] = { 
  OPNAVARGLIST_DFGHIJMACRO( 2)
, -1, -1 };

/* macro to im_delta_N, im_type_N */

#define OPNAVARGKMACRO( I) OPNAVARG_IM_DELTA, (I), OPNAVARG_IM_TYPE, (I)

/* then  triple that */

#define OPNAVARGKMACRO3( I) \
  OPNAVARGKMACRO( I), OPNAVARGKMACRO( (I)+1), OPNAVARGKMACRO( (I)+2)

static OPNAVARGLIST opnavArgList_K[] = { 
  OPNAVARG_PRIO, 1
, OPNAVARGLISTDS40MACRO( 1)
, OPNAVARG_DS56, 1
, OPNAVARG_FRAMES, 1
, OPNAVARGKMACRO3(  1)    /*  1- 3 */
, OPNAVARGKMACRO3(  4)    /*  4- 6 */
, OPNAVARGKMACRO3(  7)    /*  7- 9 */
, OPNAVARGKMACRO3( 10)    /* 10-12 */
, -1, -1 };

/* triple IM_DELTA */

#define OPNAVARGDOUBLEKMACRO3( I) \
  OPNAVARG_IM_DELTA, (I), OPNAVARG_IM_DELTA, ((I)+1), OPNAVARG_IM_DELTA, ((I)+2)

static OPNAVARGLIST opnavArgList_DOUBLE_K[] = { 
  OPNAVARG_PRIO, 1
, OPNAVARGLISTDS40MACRO( 1)
, OPNAVARG_DS56, 1
, OPNAVARG_FRAMES, 1
, OPNAVARG_IM_TYPE, 1
, OPNAVARGDOUBLEKMACRO3(  1)    /*  1- 3 */
, OPNAVARGDOUBLEKMACRO3(  4)    /*  4- 6 */
, OPNAVARGDOUBLEKMACRO3(  7)    /*  7- 9 */
, OPNAVARGDOUBLEKMACRO3( 10)    /* 10-12 */
, OPNAVARG_MISCINT, 1
, OPNAVARGLISTDS40MACRO( 2)
, OPNAVARG_DS56, 2
, OPNAVARG_FRAMES, 2
, OPNAVARG_IM_TYPE, 2
, OPNAVARGDOUBLEKMACRO3( 13)    /*  1- 3 */
, OPNAVARGDOUBLEKMACRO3( 16)    /*  4- 6 */
, OPNAVARGDOUBLEKMACRO3( 19)    /*  7- 9 */
, OPNAVARGDOUBLEKMACRO3( 22)    /* 10-12 */
, -1, -1 };

static OPNAVARGTABLE opnavArgTable[] = {

/*_opnavType   _argList */

  OPNAVTYPE_A, opnavArgList_A
, OPNAVTYPE_B, opnavArgList_B
, OPNAVTYPE_BP, opnavArgList_BP
, OPNAVTYPE_C, opnavArgList_C
, OPNAVTYPE_CACRUISE, opnavArgList_CACRUISE
, OPNAVTYPE_CE, opnavArgList_CE
, OPNAVTYPE_CT, opnavArgList_CT
, OPNAVTYPE_D, opnavArgList_D
, OPNAVTYPE_E, opnavArgList_E
, OPNAVTYPE_E2, opnavArgList_E2
, OPNAVTYPE_E3, opnavArgList_E3
, OPNAVTYPE_E4, opnavArgList_E4
, OPNAVTYPE_F, opnavArgList_F
, OPNAVTYPE_G, opnavArgList_G
, OPNAVTYPE_H, opnavArgList_H
, OPNAVTYPE_I, opnavArgList_I
, OPNAVTYPE_J, opnavArgList_J
, OPNAVTYPE_K, opnavArgList_K
, OPNAVTYPE_DOUBLE_K, opnavArgList_DOUBLE_K
, -1, (OPNAVARGLIST *) NULL };

/***************************/
/* function to get arglist */

static OPNAVARGLIST *
getOpnavArgList( long opnavType) {
OPNAVARGLIST *opnavArgListPtr = (OPNAVARGLIST *) NULL;
OPNAVARGTABLE *opnavArgTablePtr = opnavArgTable;

  for ( ; opnavArgTablePtr->_opnavType != -1; ++opnavArgTablePtr) {
    if ( opnavType == opnavArgTablePtr->_opnavType) {
      opnavArgListPtr = opnavArgTablePtr->_argList;
      break;
    }
  }
  return opnavArgListPtr;
}

#endif /* _ORBIT_CAS_OPNAV_H_ */ /* end orbit_cas_opnav.h */
