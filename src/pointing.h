/* pointing.h Pointing structures */

#ifndef _POINTING_H_
#define _POINTING_H_

/* #include <X11/Intrinsic.h>	/* so we get Widget */
#include "orbit3d.h"

/* Aimpoint structure (boresight aimpt & reference vectors) */

enum { Ij2k=0, Ieci, Isci, Iaci, Inadir, Iabf, Isbf} ;
#define Iaim Ij2k
#define Irol Ij2k
typedef struct {
  void *menu;
  void *abf;
  void *aci;
  void *nadir;
  void *j2k;
  void *eci;
  void *sci;
  int type;
  VEC vec;
} Aimpt;

/* S/C vector structure (boresight & roll vectors) */

enum { Iinstr=0, Ipanel, Ix, Iy, Iz, Iuser} ;
typedef struct {
  void *menu;
  void *instr;
  void *panel;
  void *x;
  void *y;
  void *z;
  void *user;
  int type;
  VEC vec;
} Scvec;

/* Orbit structure - keep widgets */

enum { Iorbit, Ispice } ;
typedef struct {
  void *elts_menu;
  void *elts_aster;
  void *elts_earth;
  void *elts_sc;
  void *spice_menu;
  void *spice_aster;
  void *spice_earth;
  void *spice_sc;
  int type;
  double params[10];
} Orbit;

typedef
struct DS40str {      /* static pointing:  vbore=aimpt; roll vec to roll ref */
  long _bits;                   /* to hold coordinate system bits in fldMenu */
  long _aimptFrmType;                          /* aimpoint Coordinate System */
  VEC _aimptVec;                                                 /* aimpoint */
  VEC _scRollVec;                                  /* spacecraft roll vector */
  long _aimptSelect;                                   /* aimpoint selection */
  VEC _vbVec;                                           /* virtual boresight */
  VEC _rollRefVec;           /* roll reference vector if _rollFrmType == j2k */
  long _rollFrmType;                                       /* roll selection */
  long _slewDuration;                                       /* slew_duration */
} DS40;

/* ds40->_bits - bit positions */
enum { DS40Ij2kSys=0, DS40IeciSys, DS40IsciSys              /* _aimptFrmType */
     , DS40IaciSys, DS40InadSys, DS40IabfSys                /*  "            */
     , DS40IaimSel, DS40IsciSel, DS40IeciSel, DS40InadSel   /* _aimptSelect  */
     , DS40Ij2kSysRoll, DS40IsciSysRoll                     /* _rollFrmType  */
     , DS40IeciSysRoll, DS40InadSysRoll                     /*  "            */
     };

/* - rightmost bits for each section */
#define DS40I000Sys DS40Ij2kSys
#define DS40I000Sel DS40IaimSel
#define DS40I000SysRoll DS40Ij2kSysRoll

/* - bits for _aimptFrmType */
#define DS40BITj2kSys (1L<<DS40Ij2kSys)
#define DS40BITeciSys (1L<<DS40IeciSys)
#define DS40BITsciSys (1L<<DS40IsciSys)
#define DS40BITaciSys (1L<<DS40IaciSys)
#define DS40BITnadSys (1L<<DS40InadSys)
#define DS40BITabfSys (1L<<DS40IabfSys)
#define DS40BITallSys (DS40BITj2kSys|DS40BITeciSys|DS40BITsciSys| \
                       DS40BITaciSys|DS40BITnadSys|DS40BITabfSys)
/*   - array to convert from bits to Ixxx */
static long ds40SysArray[] = { Ij2k, Ieci, Isci, Iaci, Inadir, Iabf, -1L};

/* - bits for _aimptSelect */
#define DS40BITaimSel (1L<<DS40IaimSel)
#define DS40BITsciSel (1L<<DS40IsciSel)
#define DS40BITeciSel (1L<<DS40IeciSel)
#define DS40BITnadSel (1L<<DS40InadSel)
#define DS40BITallSel (DS40BITaimSel|DS40BITsciSel| \
                       DS40BITeciSel|DS40BITnadSel)
/*   - array ... */
static long ds40SelArray[] = { Iaim, Isci, Ieci, Inadir, -1L };

/* - bits for _rollFrmType */
#define DS40BITj2kSysRoll (1L<<DS40Ij2kSysRoll)
#define DS40BITsciSysRoll (1L<<DS40IsciSysRoll)
#define DS40BITeciSysRoll (1L<<DS40IeciSysRoll)
#define DS40BITnadSysRoll (1L<<DS40InadSysRoll)
#define DS40BITallSysRoll (DS40BITj2kSysRoll|DS40BITsciSysRoll| \
                           DS40BITeciSysRoll|DS40BITnadSysRoll)
/*   - array ... */
/*     - same as ds40SelArray */
#define ds40SysRollArray ds40SelArray

/* orbit_getFrmByBit() - convert from bits to element in ...Array
 * - returns -1 on error
 * e.g.
 *  Ixxx = orbit_getFrmByBit( ds40->_bits, DS40BIT000Sys, ds40SysArray);
 *
 * ***N.B. ASSUMES LAST VALID ELEMENT IN frmArray IS FOLLOWED BY -1L
 */
static 
long orbit_getFrmByBit( long inBits, long zeroBitPosn, long *frmArray)
{
long lclBits = (inBits >> zeroBitPosn);
long i;
  for ( i=0; frmArray[i] != -1; ++i, lclBits >>= 1) {
    if ( (lclBits & 1L)) return frmArray[i];
  }
  return -1L;
}

/* orbit_getBitByFrm() - convert from Frm to bit
 * - returns 0 on error
 * e.g. 
 * bits |= orbit_getBitByFrm( ds40->_AimptFrmType, DS40BIT000Sys, ds40SysArray);
 * 
 * ***N.B. SEE ASSUMPTION ABOVE
 */
static 
long orbit_getBitByFrm( long inFrm, long zeroBitPosn, long *frmArray)
{
long lclBit = (1L<<zeroBitPosn);
long i;
  for ( i=0; frmArray[i] != -1; ++i, lclBit <<= 1) {
    if ( inFrm == frmArray[i]) return lclBit;
  }
  return 0L;
}
  

typedef
struct DS56str {     /* dynamic pointing - vary boresight aimpoint with time */
  long _frmType;                                        /* Coordinate System */
  long _reUse;                                           /* RE_USE_PREV_SCAN */
  long _bits;      /* which of _ratevec are moving when & how, + closed loop */
                             /* - I/BITxyzPAUSE - 1=>initially moving        */
                             /* - I/BITxyzCHG - Reverse Direction each Rate  */
                                     /* - I/BITclosedLOOP - closed loop flag */
                                /* - I/BIT*Sys - frmType bits for field menu */
                           /* - I/BITreUSE* - reuse prev scan for field menu */
  VEC _ratePauseDur[2];                            /* Rate & Pause durations */
  VEC _rateVec;               /* initial aimpoint vector rates, units/second */
} DS56;

/* ds56->_bits - bit positions */
enum { DS56IxPAUSE = 0L, DS56IyPAUSE, DS56IzPAUSE
     , DS56IxCHG, DS56IyCHG, DS56IzCHG 
     , DS56IclosedLOOP
     , DS56Ij2kSys, DS56InadSys, DS56IabfSys, DS56IaciSys, DS56IsbfSys
     , DS56IreUSE, DS56IreUSE_not
     };
#define DS56I000Sys DS56Ij2kSys                  /* low bit of coord sys bits */
#define DS56I000reUSE DS56IreUSE                /* low bit of reuse prev scan */

#define BITxPAUSE (1L<<DS56IxPAUSE)
#define BITyPAUSE (1L<<DS56IyPAUSE)
#define BITzPAUSE (1L<<DS56IzPAUSE)
#define BITxCHG (1L<<DS56IxCHG)
#define BITyCHG (1L<<DS56IyCHG)
#define BITzCHG (1L<<DS56IzCHG)
#define BITclosedLOOP (1L<<DS56IclosedLOOP)
#define DS56BITj2kSys (1L<<DS56Ij2kSys)
#define DS56BITnadSys (1L<<DS56InadSys)
#define DS56BITabfSys (1L<<DS56IabfSys)
#define DS56BITaciSys (1L<<DS56IaciSys)
#define DS56BITsbfSys (1L<<DS56IsbfSys)
#define DS56BITreUSE (1L<<DS56IreUSE)
#define DS56BITreUSE_not (1L<<DS56IreUSE_not)

#define DS56BITallSys (DS56BITj2kSys|DS56BITnadSys|DS56BITabfSys| \
                       DS56BITaciSys|DS56BITsbfSys)
#define DS56BITallREUSE (DS56BITreUSE|DS56BITreUSE_not)

static long ds56SysArray[] = { Ij2k, Inadir, Iabf, Iaci, Isbf, -1L} ;

static long ds56ReUseArray[] = { DS56IreUSE, DS56IreUSE_not, -1L } ;
static char *ds56ReUseNames[] = { "TRUE", "FALSE", (char *)0 } ;

typedef struct {
  Aimpt aimpt;
  Scvec scvec;
} POINTING;

/* time structure */

typedef struct {
  double _et;
  int
    _year
  , _doy
  , _hour
  , _minute
  , _second
  ;
  char _utcstr[UTCLEN];
} UTC ;

#define UTCOUT(UU) "%04d-%03dT%02d:%02d:%02d" \
  , (UU)._year, (UU)._doy, (UU)._hour, (UU)._minute, (UU)._second

#define UTCLOAD(UU) sprintf( (UU)._utcstr, UTCOUT(UU) )

void orbitgui_return_boreroll( SC *, POINTING **, POINTING **);
int pointing_solve( char *, double *, SC *, POINTING *, POINTING *);

#endif

