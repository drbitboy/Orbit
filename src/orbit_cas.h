/* orbit_cas.h - structures for interpreting CAS's (Command Activity Sequences)
 */

#ifndef _ORBIT_CAS_H_
#define _ORBIT_CAS_H_

#include "orbit3d.h"
#include "pointing.h"
#include "orbit_params.h"
#include "orbit_cas_opnav.h"

/* To add CAS's:
 *
 *   1) add CASTYPE_... enumeration id's for new fragments in orbit_cas.h (here)
 *      - order is important for frags that are combined into CAS's that 
 *          orbit simulates 
 *          - see casCASFmLbl[] in orbitgui_create_CAS_fldMenu() in 
 *            orbitgui_cas.c, coordinate with Group/position in enum
 *            in orbit_cas.h
 *   1A) add corresponding CASBITS_... in orbit_cas.h
 *   2) add CAS description to casDescr for any SASF CASes in orbit_cas.h
 *   3) add, if necessary, typedef struct that is to be sub-struct in 
 *      struct CASstr in orbit_cas.h e.g. like MSISEQDEFstr
 *      - it is likely that the GENERICFRAG would be adequate for most,
 *        so #define NEWFRAG GENERICFRAG may be enough e.g. see MSICONFIGFRAG
 *   4) add typedef from (3) to CASstr in orbit_cas.h
 *   5) add new item to WCAS, WHOAMI in orbit_cas.h
 *   6) add new isNEWFRAG macro in orbit_cas.h
 *      - check isANY* macros also
 *   7) add macros for members of sub-struct in orbit_cas.h 
 *      e.g. #define msiAutoTImDelta _data._auto->_longs[0]
 *   8) if there are any bit-type fields, add enumerated bit positions 
 *      NEWFRAGIxyzAbc & bits NEWFRAGBITxyzAbc in orbit_cas.h
 *      e.g. XGRSCONFIalPosn & XGRSCONFBITalPosn
 *      - add NEWFRAGBITall_Abc macro(s) which has OR's all bits in a field
 *      - add long array(s) & name array(s) newFragPosnArray & newFragPosnNames
 *        e.g. static long xgrsConfPosnArray[] = { ..., -1L };
 *           & static char *xgrsConfPosnNames[] = { ..., (char *) 0};
 *      - add NEWFRAGI000Abc macro(s) which is/are the first bit position in 
 *        each field e.g. XGRSCONFI000Posn
 *   9) add MKCAS line to orbit_CAS_new() in orbit_cas.c
 *  10) add "case CASTYPE_NEWFRAG: ..." default values code to orbit_CAS_new() 
 *      in orbit_cas.c
 *  11) add "case CASTYPE_NEWFRAG: ..." code to RWARGMACRO macro in orbit_cas.c
 *  12) add static char *newFragFmLbl = ..." to orbitgui_create_CAS_fldMenu() 
 *      in orbitgui_cas.c
 *      - if no user parameters, add CASTYPE_NEWFRAG: to CASE_NOPARAMS macro
 *  13) add to casCASFmLbl in orbitgui_create_CAS_fldMenu() in orbitgui_cas.c, 
 *      - adjust NCASCASFMLBL as necessary
 *      - adjust/add MKFLDBITS under case CASTYPE_CAS:
 *  14) add "case CASTYPE_NEWFRAG: ..." code in orbitgui_cas.c:
 *      - to set lcllbl0 in orbitgui_create_CAS_fldMenu()
 *      - to handle user parameters in orbitgui_create_CAS_fldMenu()
 *      - to handle user parameters in orbitgui_CAS_fldMenu_ok_CB()
 *      ***N.B. only done if not CASTYPE_NEWFRAGS is not in CASES_NOPARAMS macro
 *  15) add "case CASTYPE_NEWFRAG:" before orbitgui_create_CAS_fldMenu call in
 *      orbitgui_CASmenu_widget_CB() if CASTYPE_NEWFRAG if not in 
 *      CASES_NOPARAMS macro
 *  16) add buttons if necessary using (NOT)LEFTEDGE macros in 
 *      orbitgui_CASmenu_CB() in orbitgui_cas.c
 *  17) if new CASTYPE is a CAS* (e.g. CASOPNAV), add CAS*_START_DELAY &
 *      CAS*_SCHED_POINT to orbit_cas.h (or to orbit_cas_*.h)
 *  18) "grep -n '[^n]isCAS' *.c *.h", add/modify such lines as needed
 *  18A) "grep -n 'CAS[A-Z]*:' *.c *.h", add/modify such lines as needed
 *  19) modify CASSTART() in orbit_cas.h if necessary (e.g. see CASOPNAV))
 */

typedef
struct MSISEQDEFstr {                             /* MSI Sequence Definition */
  long _idNum;                                         /* slot to use (1-30) */
  long _numImages;          /* number of images to take per sequence, 0 to 8 */
  long _interval;                      /* seconds between images in sequence */
  long _bits;                        /* repeat flag - 0=>NONE                */
                                     /* compression table - NONE, TABLE[1-7] */
                                     /* flag dpcm - ON, OFF                  */
                                     /* mode - AUTO, MAN                     */
  long _cmpTbl;         /* compression table (NONE, TABLE1-7), also in _bits */
  long _cmpAlg;   /* compression algorithm (NONE, FAST, RICE), also in _bits */
  long _dpcm;                          /* flag dpcm (ON, OFF), also in _bits */
  long _mode;                    /* exposure mode (AUTO, MAN), also in _bits */
  long _msecExp[8];        /* exposure at each filter, ms; 0 for no exposure */
  long _filt[8];                                           /* filters to use */
  long _pixels;                                              /* bits/pixel ? */
  int _topIndex;                              /* index into CASDEF CAS lists */
} MSISEQDEF;

enum { 
       MSISEQDEF_CMPALG_NONE = 0L
     , MSISEQDEF_CMPALG_FAST
     , MSISEQDEF_CMPALG_RICE
     };

enum { MSISEQDEFIrptFlag = 0
     , MSISEQDEFInonCmpTbl
     , MSISEQDEFI001CmpTbl
     , MSISEQDEFI002CmpTbl
     , MSISEQDEFI003CmpTbl
     , MSISEQDEFI004CmpTbl
     , MSISEQDEFI005CmpTbl
     , MSISEQDEFI006CmpTbl
     , MSISEQDEFI007CmpTbl
     , MSISEQDEFIonDpcm
     , MSISEQDEFIoffDpcm
     , MSISEQDEFIautMode
     , MSISEQDEFImanMode
     , MSISEQDEFInonCmpAlg
     , MSISEQDEFIfstCmpAlg
     , MSISEQDEFIricCmpAlg
     };

#define MSISEQDEFI000CmpTbl MSISEQDEFInonCmpTbl
#define MSISEQDEFI000CmpAlg MSISEQDEFInonCmpAlg
#define MSISEQDEFI000Dpcm MSISEQDEFIonDpcm
#define MSISEQDEFI000Mode MSISEQDEFIautMode

#define MSISEQDEFBITrptFlg (1L<<MSISEQDEFIrptFlg)
#define MSISEQDEFBITnonCmpTbl (1L<<MSISEQDEFInonCmpTbl)
#define MSISEQDEFBIT001CmpTbl (1L<<MSISEQDEFI001CmpTbl)
#define MSISEQDEFBIT002CmpTbl (1L<<MSISEQDEFI002CmpTbl)
#define MSISEQDEFBIT003CmpTbl (1L<<MSISEQDEFI003CmpTbl)
#define MSISEQDEFBIT004CmpTbl (1L<<MSISEQDEFI004CmpTbl)
#define MSISEQDEFBIT005CmpTbl (1L<<MSISEQDEFI005CmpTbl)
#define MSISEQDEFBIT006CmpTbl (1L<<MSISEQDEFI006CmpTbl)
#define MSISEQDEFBIT007CmpTbl (1L<<MSISEQDEFI007CmpTbl)
#define MSISEQDEFBITonDpcm (1L<<MSISEQDEFIonDpcm)
#define MSISEQDEFBIToffDpcm (1L<<MSISEQDEFIoffDpcm)
#define MSISEQDEFBITautMode (1L<<MSISEQDEFIautMode)
#define MSISEQDEFBITmanMode (1L<<MSISEQDEFImanMode)
#define MSISEQDEFBITnonCmpAlg (1L<<MSISEQDEFInonCmpAlg)
#define MSISEQDEFBITfstCmpAlg (1L<<MSISEQDEFIfstCmpAlg)
#define MSISEQDEFBITricCmpAlg (1L<<MSISEQDEFIricCmpAlg)

#define MSISEQDEFBITallCmpTbl (MSISEQDEFBITnonCmpTbl|MSISEQDEFBIT001CmpTbl| \
                               MSISEQDEFBIT002CmpTbl|MSISEQDEFBIT003CmpTbl| \
                               MSISEQDEFBIT004CmpTbl|MSISEQDEFBIT005CmpTbl| \
                               MSISEQDEFBIT006CmpTbl|MSISEQDEFBIT007CmpTbl)

static long msiSeqDefCmpTblArray[] = { 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, -1L};
static char *msiSeqDefCmpTblNames[] = 
          { "NONE", "TABLE1", "TABLE2", "TABLE3"
          , "TABLE4", "TABLE5", "TABLE6", "TABLE7", (char *) 0 };

#define MSISEQDEFBITallDpcm (MSISEQDEFBITonDpcm | MSISEQDEFBIToffDpcm)
static long msiSeqDefDpcmArray[] = { 0L, 1L, -1L };
static char *msiSeqDefDpcmNames[] = { "ON", "OFF", (char *) 0 };

#define MSISEQDEFBITallMode (MSISEQDEFBITautMode | MSISEQDEFBITmanMode)
static long msiSeqDefModeArray[] = { 0L, 1L, -1L };
static char *msiSeqDefModeNames[] = { "AUTO", "MAN", (char *) 0 };

#define MSISEQDEFBITallCmpAlg (MSISEQDEFBITnonCmpAlg|MSISEQDEFBITfstCmpAlg| \
                               MSISEQDEFBITricCmpAlg)
static long msiSeqDefCmpAlgArray[] = { MSISEQDEF_CMPALG_NONE
                                     , MSISEQDEF_CMPALG_FAST
                                     , MSISEQDEF_CMPALG_RICE
                                     , -1L };
static char *msiSeqDefCmpAlgNames[] = { "NONE", "FAST", "RICE", (char *) 0 };

/***********************************/

typedef
struct NISSEQDEFstr {                             /* NIS Sequence Definition */
  long _idNum;
  long _numScans;
  long _stepMirror;
  long _timings[3];       /* s/spectra, s betw spectra in a scan, delay/scan */
  long _calInterval;
  long _numObs;
  long _numRests;
  long _numDarks;
  int _topIndex;                              /* index into CASDEF CAS lists */
} NISSEQDEF;

/************************/
/* NIS Repeat Fragments */

typedef
struct NISREPstr {                               /* NIS Single/Double Repeat */
  long _repSetup;                         /* setup time before seq 1 (SR/DR) */
  long _repMirrorPosnSetup;             /* mirror setup before seq 1 (SR/DR) */
  long _repApertureSetup;             /* aperture setup before seq 1 (SR/DR) */
  long _repGainSetup;                     /* gain setup before seq 1 (SR/DR) */
  long _repBitsSetup;                  /* bits to hold setup aperture & gain */
  long _seq[2];                              /* [0] 1st seq # to use (SR/DR) */
                                             /* [1] 2nd seq # to use (DR)    */
  long _mirrorPosn[2];             /* [0] starting mirror posn seq 1 (SR/DR) */
                                   /* [1] starting mirror posn seq 2 (DR)    */
  long _aperture[2];                           /* [0] aperture seq 1 (SR/DR) */
                                               /* [1] aperture seq 2 (DR)    */
  long _gain[2];                                   /* [0] gain seq 1 (SR/DR) */
                                                   /* [1] gain seq 2 (DR)    */
  long _del[2];                             /* [0] s btw two seq 1's (SR/DR) */
                                            /* [1] s btw two seq 2's (DR)    */
  long _repDelay;              /* START_SEQUENCING s offset to seq 1 (SR/DR) */
  long _iter;              /* # of times to execute seq 1 (SR) or 1 & 2 (DR) */
  long _bits[2];                             /* bits to hold aperture & gain */
} NISREP;

enum { NISREPIbothinAper = 0   /* BOTH_IN */
     , NISREPIbothoutAper      /* BOTH_OUT */
     , NISREPIshutterAper      /* SHUTTER */
     , NISREPIslitAper         /* SLIT */
     , NISREPInochangeAper     /* NOCHANGE - must be last Aper */
     , NISREPI10xGain          /* 10X */
     , NISREPI1xGain           /* 1X */
     , NISREPInochangeGain     /* NOCHANGE - must be last Gain */
     };

static long nisRepGainArray[] = { NISREPI10xGain, NISREPI1xGain, -1L };
static char *nisRepGainNames[] = { "10X", "1X", (char *) 0 };

static long nisRepGainNochArray[] = { NISREPI10xGain, NISREPI1xGain
                                , NISREPInochangeGain
                                , -1L };
static char *nisRepGainNochNames[] = { "10X", "1X", "NOCHANGE"
                                 , (char *) 0 };

static long nisRepAperArray[] = { NISREPIbothinAper, NISREPIbothoutAper
                                , NISREPIshutterAper, NISREPIslitAper
                                , -1L };
static char *nisRepAperNames[] = { "BOTH_IN", "BOTH_OUT", "SHUTTER", "SLIT"
                                 , (char *) 0 };

static long nisRepAperNochArray[] = { NISREPIbothinAper, NISREPIbothoutAper
                                , NISREPIshutterAper, NISREPIslitAper
                                , NISREPInochangeAper
                                , -1L };
static char *nisRepAperNochNames[] = { "BOTH_IN", "BOTH_OUT", "SHUTTER", "SLIT"
                                 , "NOCHANGE"
                                 , (char *) 0 };

#define NISREPBITbothinAper (1L<<NISREPIbothinAper)
#define NISREPBITbothoutAper (1L<<NISREPIbothoutAper)
#define NISREPBITshutterAper (1L<<NISREPIshutterAper)
#define NISREPBITslitAper (1L<<NISREPIslitAper)
#define NISREPBITnochangeAper (1L<<NISREPInochangeAper)
#define NISREPBITallAper (NISREPBITbothinAper|NISREPBITbothoutAper| \
                          NISREPBITshutterAper|NISREPBITslitAper)
#define NISREPBITallAperNoch (NISREPBITallAper|NISREPBITnochangeAper)

#define NISREPBIT10xGain (1L<<NISREPI10xGain)
#define NISREPBIT1xGain (1L<<NISREPI1xGain)
#define NISREPBITnochangeGain (1L<<NISREPInochangeGain)
#define NISREPBITallGain (NISREPBIT10xGain|NISREPBIT1xGain)
#define NISREPBITallGainNoch (NISREPBITallGain|NISREPBITnochangeGain)

#define NISREPI000Aper NISREPIbothinAper
#define NISREPI000Gain NISREPI10xGain

#define NISREPI000AperNoch NISREPI000Aper
#define NISREPI000GainNoch NISREPI000Gain

/*****************************/
/* Super Scan/Scan2 Fragment */

typedef
struct NISSUstr {                                            /* NIS SU & SU2 */
  long _repDelay;                                        /* start sequencing */
  long _seq;                                                 /* seq # to use */
  long _mirrorPosn;                                      /* init mirror posn */
  long _aperture;
  long _gain;
  long _ppss;                                    /* positions per super scan */
  long _stepDir;                    /* step dir (forward or backward)  (SU2) */
  long _noss;                                       /* # of super scans (SU) */
  long _bits;                       /* bits to hold aperture, gain & stepDir */
} NISSU;

enum { NISSUIforeDir = NISREPI1xGain + 1                          /* FORWARD */
     , NISSUIbackDir                                             /* BACKWARD */
     };

#define nisSuGainArray nisRepGainArray
#define nisSuGainNames nisRepGainNames

#define nisSuAperArray nisRepAperArray
#define nisSuAperNames nisRepAperNames

#define nisSuGainNochArray nisRepGainNochArray
#define nisSuGainNochNames nisRepGainNochNames

#define nisSuAperNochArray nisRepAperNochArray
#define nisSuAperNochNames nisRepAperNochNames

static long nisSuStepDirArray[] = { NISSUIforeDir, NISSUIbackDir
                                  , -1L };
static char *nisSuStepDirNames[] = { "FORWARD", "BACKWARD"
                                , (char *) 0 };

#define NISSUBITbothinAper NISREPBITbothinAper
#define NISSUBITbothoutAper NISREPBITbothoutAper
#define NISSUBITshutterAper NISREPBITshutterAper
#define NISSUBITslitAper NISREPBITslitAper
#define NISSUBITnochangeAper NISREPBITnochangeAper
#define NISSUBITallAper NISREPBITallAper
#define NISSUBITallAperNoch NISREPBITallAperNoch

#define NISSUBIT10xGain NISREPBIT10xGain
#define NISSUBIT1xGain NISREPBIT1xGain
#define NISSUBITnochangeGain NISREPBITnochangeGain
#define NISSUBITallGain NISREPBITallGain
#define NISSUBITallGainNoch NISREPBITallGainNoch

#define NISSUBITforeDir (1L<<NISSUIforeDir)
#define NISSUBITbackDir (1L<<NISSUIbackDir)
#define NISSUBITall_Dir (NISSUBITforeDir|NISSUBITbackDir)

#define NISSUIbothinAper NISREPIbothinAper
#define NISSUIbothoutAper NISREPIbothoutAper
#define NISSUIshutterAper NISREPIshutterAper
#define NISSUIslitAper NISREPIslitAper
#define NISSUInochangeAper NISREPInochangeAper
#define NISSUIallAper NISREPIallAper
#define NISSUIallAperNoch NISREPIallAperNoch

#define NISSUI10xGain NISREPI10xGain
#define NISSUI1xGain NISREPI1xGain
#define NISSUInochangeGain NISREPInochangeGain
#define NISSUIallGain NISREPIallGain
#define NISSUIallGainNoch NISREPIallGainNoch

#define NISSUI000Aper NISREPI000Aper
#define NISSUI000Gain NISREPI000Gain

#define NISSUI000AperNoch NISREPI000AperNoch
#define NISSUI000GainNoch NISREPI000GainNoch

#define NISSUI000Dir NISSUIforeDir

/********************/
/* Generic Fragment */

typedef
struct GENERICFRAGstr {                            /* to serve many purposes */
  long _longs[16];
  long _bits;
} GENERICFRAG;

#define AUTOEXPOSEFRAG GENERICFRAG                        /* MSI Auto Expose */
#define LOADFILTFRAG GENERICFRAG                          /* MSI Load Filter */
#define MSICONFIGFRAG GENERICFRAG                           /* MSI Configure */
#define PARKFRAG GENERICFRAG                                    /* Park Mode */
#define RELFRAG GENERICFRAG                              /* Release Attitude */
#define NISCAL GENERICFRAG                                        /* NIS Cal */
#define NISBUFFLUSH GENERICFRAG                          /* NIS Buffer Flush */
#define NISCONFIG GENERICFRAG                               /* NIS Configure */
#define XGRSCONFIG GENERICFRAG                             /* xgrs Configure */

/************************/
/* MSI Shoot Fragment */

typedef
struct SHOOTstr {                                               /* MSI Shoot */
  long _count;                                               /* num_executes */
  long _seq[12];                                   /* seq_num_1 - seq_num_12 */
  long _del[12];                                 /* im_delta_1 - im_delta_12 */
  double _imageTypeDbl[12];                        /* im_type_1 - im_type_12 */
} SHOOT;

/************************/
/* MSI Repeat Fragments */

typedef
struct MSIREPstr {              /* MSI Single/Double/Double Staggered Repeat */
  long _seq[3];                          /* [0] 1st seq # to use (SR/DR/DSR) */
                                         /* [1] 2nd seq # to use (D/DSR)     */
                                         /* [2] 3rd seq # to use (TR)        */
  long _del[3];                           /* [0] s btw two seq 1's (S/D/DSR) */
                                          /* [1] s btw two seq 2's (D/DSR)   */
  long _iter[2];                /* [0] # of times to execute seq 1 (S/D/DSR) */
                                /* [1] # of times to execute seq 2 (D/DSR)   */
  long _repDelay[2];        /* [0] START_SEQUENCING s offset to seq 1 (S/DR) */
                            /* [0] START_SEQ_1 s offset to seq 1 (DSR)       */
                            /* [1] START_SEQ_2 s offset to seq 2 (DSR)       */
  long _seq2Start;                      /* s btw 1st seq 1 & 1st seq 2 (DSR) */
  double _imageTypeDbl;                           /* image type (1.0 - 2.5)) */
  long _bits;                                             /* image type bits */
  double _imageTypeDblTR[3];                /* image types for triple repeat */
  long _bitsTR[3];                               /* image type bits for (TR) */
} MSIREP;

/* values for _imageType */

enum { MSIREPIblkType = 0   /* Black */
     , MSIREPIsmlType       /* Small */
     , MSIREPInrwType      /* Narrow */
     , MSIREPIfulType        /* Full */
     };

/* - bits for _imageType */

#define MSIREPBITblkType (1L<<MSIREPIblkType)
#define MSIREPBITsmlType (1L<<MSIREPIsmlType)
#define MSIREPBITnrwType (1L<<MSIREPInrwType)
#define MSIREPBITfulType (1L<<MSIREPIfulType)
#define MSIREPBITallType (MSIREPBITblkType|MSIREPBITsmlType| \
                          MSIREPBITnrwType|MSIREPBITfulType)

/*   - array to convert from bits to Ixxx */

static long msiRepTypeArray[] = 
     { MSIREPIblkType
     , MSIREPIsmlType
     , MSIREPInrwType
     , MSIREPIfulType
     , -1L
     };
static char *msiRepTypeNames[] = 
  { "BLACK", "SMALL", "NARROW", "FULL", (char *) 0 };
/* rightmost bit */

#define MSIREPI000Type MSIREPIblkType

/* end of MSI Repeat Fragments' descriptions */
/*********************************************/

/* CAS types
 * ***N.B. The order of groups of bits here must match that in casCASFmLbl[]
 *         in orbitgui_cas.c
 */
enum {                    /* Group#/position in field menu */
  CASTYPE_MSISEQDEF=0L
, CASTYPE_NISSEQDEF
, CASTYPE_SHOOT           /* G2/1 */
, CASTYPE_MSISR           /* G2/2 */
, CASTYPE_MSIDR           /* G2/3 */
, CASTYPE_MSIDSR          /* G2/4 */
, CASTYPE_MSITR           /* G2/5 - msi triple repeat */

, CASTYPE_NISEX           /* G3/1 - NIS execute use NISREP with 1 repeat */
, CASTYPE_NISSR           /* G3/2 */
, CASTYPE_NISDR           /* G3/3 */
, CASTYPE_NISSU2          /* G3/4 - nis super scan */
, CASTYPE_NISSU1          /* G3/5 - nis super, aka low phase super scan */

, CASTYPE_DS40            /* G1/1 */
, CASTYPE_DS40FULL        /* G1/2 */
, CASTYPE_DS40XGRS        /* G1/3 - xgrs pt quick - SlewDur,CoordSys,AimptXYZ */
, CASTYPE_DS56            /* G1/4 */
/* , CASTYPE_NADIR           /* G1/5 (not currently used) */

, CASTYPE_AUTOEXPOSE      /* G5/1 - msi auto expose */
, CASTYPE_LOADFILT        /* G5/2 - msi load filter table */
, CASTYPE_MSIRELATT       /* G5/3 - msi release attitude */
, CASTYPE_MSICONFIG       /* G5/4 - msi configure table */
, CASTYPE_MSIPARK         /* G5/5 */

, CASTYPE_NISCAL          /* G4/1 - nis calibrate */
, CASTYPE_NISBUFFLUSH     /* G4/2 - nis buffer flush */
, CASTYPE_NISRELATT       /* G4/3 - nis release attiitude */
, CASTYPE_NISCONFIG       /* G4/4 - nis configure */
, CASTYPE_NISPARK         /* G4/5 */

, CASTYPE_XGRSRELATT      /* G6/1 - xgrs release attiitude */
, CASTYPE_XGRSCONFIG      /* G6/2 - xgrs cal src pos */
, CASTYPE_XGRSPARK        /* G6/3 - xgrs park */
, CASTYPE_NAVRELATT
, CASTYPE_CASOPNAV

/*** last fragment above, no more than 32 allowed to fit in _bits ***/

, CASTYPE_CAS
, CASTYPE_CASDEF
, CASTYPE_REQ

, CASTYPE_COUNT};         /* this is last, used for loop count, not a FRAG */

#define CASBITS_MSISEQDEF (1L<<CASTYPE_MSISEQDEF)
#define CASBITS_NISSEQDEF (1L<<CASTYPE_NISSEQDEF)
#define CASBITS_SHOOT (1L<<CASTYPE_SHOOT)
#define CASBITS_MSISR (1L<<CASTYPE_MSISR)
#define CASBITS_MSIDR (1L<<CASTYPE_MSIDR)
#define CASBITS_MSIDSR (1L<<CASTYPE_MSIDSR)
#define CASBITS_MSITR (1L<<CASTYPE_MSITR)
#define CASBITS_NISEX (1L<<CASTYPE_NISEX)
#define CASBITS_NISSR (1L<<CASTYPE_NISSR)
#define CASBITS_NISDR (1L<<CASTYPE_NISDR)               /* 10 */
#define CASBITS_NISSU2 (1L<<CASTYPE_NISSU2)
#define CASBITS_NISSU1 (1L<<CASTYPE_NISSU1)
#define CASBITS_DS40 (1L<<CASTYPE_DS40)
#define CASBITS_DS40FULL (1L<<CASTYPE_DS40FULL)
#define CASBITS_DS40XGRS (1L<<CASTYPE_DS40XGRS)
#define CASBITS_DS56 (1L<<CASTYPE_DS56)
#define CASBITS_AUTOEXPOSE (1L<<CASTYPE_AUTOEXPOSE)
#define CASBITS_LOADFILT (1L<<CASTYPE_LOADFILT)
#define CASBITS_MSIRELATT (1L<<CASTYPE_MSIRELATT)
#define CASBITS_MSICONFIG (1L<<CASTYPE_MSICONFIG)       /* 20 */
#define CASBITS_MSIPARK (1L<<CASTYPE_MSIPARK)
#define CASBITS_NISCAL (1L<<CASTYPE_NISCAL)
#define CASBITS_NISBUFFLUSH (1L<<CASTYPE_NISBUFFLUSH)
#define CASBITS_NISRELATT (1L<<CASTYPE_NISRELATT)
#define CASBITS_NISCONFIG (1L<<CASTYPE_NISCONFIG)
#define CASBITS_NISPARK (1L<<CASTYPE_NISPARK)
#define CASBITS_XGRSRELATT (1L<<CASTYPE_XGRSRELATT)
#define CASBITS_XGRSCONFIG (1L<<CASTYPE_XGRSCONFIG)
#define CASBITS_XGRSPARK (1L<<CASTYPE_XGRSPARK)
#define CASBITS_NAVRELATT (1L<<CASTYPE_NAVRELATT)      /* 30 */
#define CASBITS_CASOPNAV (1L<<CASTYPE_CASOPNAV)

/* #define CASBITS_future (1L<<CASTYPE_future) */
/* #define CASBITS_NADIR (1L<<CASTYPE_NADIR) not used */

/*
#define CASBITS_CAS (1L<<CASTYPE_CAS)
#define CASBITS_CASDEF (1L<<CASTYPE_CASDEF)
#define CASBITS_REQ (1L<<CASTYPE_REQ)
*/
/* structure to describe CASes with specific sets of fragments */

typedef struct {
  char *_shortName;
  char *_sasfName;
  long _casBits;
} CASDESCR;

#define MAXCASKEYLEN 20  /* CAS requestor key strings are MAXCASKEYLEN+1 long */
typedef char CASKEY[MAXCASKEYLEN+1];
 
static CASDESCR casDescr[] = {
  { "MCONF", "CAS_MSI_CONFIG", CASBITS_MSICONFIG }
, { "MSEQ", "CAS_MSI_SEQ_DEF", CASBITS_MSISEQDEF }
, { "MAUTO", "CAS_MSI_AUTO_EXPOSE", CASBITS_AUTOEXPOSE }
, { "MFILT", "CAS_MSI_LOAD_FILTER_TABLE", CASBITS_LOADFILT }
, { "MSH", "CAS_MSI_SHOOT", CASBITS_SHOOT }
, { "MSR", "CAS_MSI_SINGLE_REPEAT", CASBITS_MSISR }
, { "MDR", "CAS_MSI_DOUBLE_REPEAT", CASBITS_MSIDR }
, { "MTR", "CAS_MSI_TRIPLE_REPEAT", CASBITS_MSITR }
, { "MDSR", "CAS_MSI_DOUBLE_STAGGERED_REPEAT", CASBITS_MSIDSR }
, { "MPARK", "CAS_MSI_PARK_MODE", CASBITS_MSIPARK }
, { "MREL", "CAS_MSI_RELEASE_ATTITUDE", CASBITS_MSIRELATT }
, { "MP", "CAS_MSI_POINT", CASBITS_DS40 }
, { "MFP", "CAS_MSI_POINT_FULL", CASBITS_DS40FULL }
, { "MPSH", "CAS_MSI_POINT_SHOOT", CASBITS_DS40|CASBITS_SHOOT }
, { "MPSR", "CAS_MSI_POINT_SINGLE_REPEAT", CASBITS_MSISR|CASBITS_DS40 }
, { "MPDR", "CAS_MSI_POINT_DOUBLE_REPEAT", CASBITS_MSIDR|CASBITS_DS40 }
, { "MPTR", "CAS_MSI_POINT_TRIPLE_REPEAT", CASBITS_MSITR|CASBITS_DS40 }
, { "MPDSR", "CAS_MSI_POINT_DOUBLE_STAGGERED_REPEAT", CASBITS_MSIDSR|CASBITS_DS40 }
, { "MSSR", "CAS_MSI_SCAN_SINGLE_REPEAT", CASBITS_MSISR|CASBITS_DS56 }
, { "MSDR", "CAS_MSI_SCAN_DOUBLE_REPEAT", CASBITS_MSIDR|CASBITS_DS56 }
, { "MSDSR", "CAS_MSI_SCAN_DOUBLE_STAGGERED_REPEAT", CASBITS_MSIDSR|CASBITS_DS56 }
, { "MPSSH", "CAS_MSI_POINT_SCAN_SHOOT", CASBITS_SHOOT|CASBITS_DS40|CASBITS_DS56 }
, { "MPSSR", "CAS_MSI_POINT_SCAN_SINGLE_REPEAT", CASBITS_MSISR|CASBITS_DS40|CASBITS_DS56 }
, { "MPSDR", "CAS_MSI_POINT_SCAN_DOUBLE_REPEAT", CASBITS_MSIDR|CASBITS_DS40|CASBITS_DS56 }
, { "MPSTR", "CAS_MSI_POINT_SCAN_TRIPLE_REPEAT", CASBITS_MSITR|CASBITS_DS40|CASBITS_DS56 }
, { "MPSDSR", "CAS_MSI_POINT_SCAN_DOUBLE_STAGGERED_REPEAT", CASBITS_MSIDSR|CASBITS_DS40|CASBITS_DS56 }
, { "NCAL", "CAS_NIS_CAL", CASBITS_NISCAL }
, { "NBUF", "CAS_NIS_BUFFER_FLUSH", CASBITS_NISBUFFLUSH }
, { "NCONF", "CAS_NIS_CONFIGURE", CASBITS_NISCONFIG }
, { "NSEQ", "CAS_NIS_LOAD_TRANS_SEQ_DEF", CASBITS_NISSEQDEF }
, { "NREL", "CAS_NIS_RELEASE_ATTITUDE", CASBITS_NISRELATT }
, { "NPARK", "CAS_NIS_PARK_MODE", CASBITS_NISPARK }
, { "NE", "CAS_NIS_EXECUTE", CASBITS_NISEX }
, { "NSR", "CAS_NIS_SINGLE_REPEAT", CASBITS_NISSR }
, { "NDR", "CAS_NIS_DOUBLE_REPEAT", CASBITS_NISDR }
, { "NPE", "CAS_NIS_POINT_EXECUTE", CASBITS_NISEX|CASBITS_DS40 }
, { "NPSE", "CAS_NIS_POINT_SCAN_EXECUTE", CASBITS_NISEX|CASBITS_DS40|CASBITS_DS56 }
, { "NPSR", "CAS_NIS_POINT_SINGLE_REPEAT", CASBITS_NISSR|CASBITS_DS40 }
, { "NPDR", "CAS_NIS_POINT_DOUBLE_REPEAT", CASBITS_NISDR|CASBITS_DS40 }
, { "NSSR", "CAS_NIS_SCAN_SINGLE_REPEAT", CASBITS_NISSR|CASBITS_DS56 }
, { "NSDR", "CAS_NIS_SCAN_DOUBLE_REPEAT", CASBITS_NISDR|CASBITS_DS56 }
, { "NPSSR", "CAS_NIS_POINT_SCAN_SINGLE_REPEAT", CASBITS_NISSR|CASBITS_DS40|CASBITS_DS56 }
, { "NPSDR", "CAS_NIS_POINT_SCAN_DOUBLE_REPEAT", CASBITS_NISSR|CASBITS_DS40|CASBITS_DS56 }
, { "NSU2", "CAS_NIS_SUPER_SCAN_2", CASBITS_NISSU2 }
, { "NSU1", "CAS_NIS_SUPERSCAN", CASBITS_NISSU1 }
, { "NFPE", "CAS_NIS_FULL_POINT_EXECUTE", CASBITS_NISEX|CASBITS_DS40FULL }
, { "NFPSE", "CAS_NIS_FULL_POINT_SCAN_EXECUTE", CASBITS_NISEX|CASBITS_DS40FULL|CASBITS_DS56 }
, { "NFPSSU1", "CAS_NIS_FULL_POINT_SCAN_SUPERSCAN_1", CASBITS_NISSU1|CASBITS_DS40FULL|CASBITS_DS56 }
, { "NFPSU1", "CAS_NIS_FULL_POINT_SUPERSCAN_1", CASBITS_NISSU1|CASBITS_DS40FULL }
, { "NFPSSR", "CAS_NIS_FULL_POINT_SCAN_SINGLE_REPEAT", CASBITS_NISSR|CASBITS_DS40FULL|CASBITS_DS56 }

/* XGRS stuff, 20.jul'1999 */

, { "XPQ", "CAS_xgrs_point_quick", CASBITS_DS40XGRS }
, { "XCONF", "CAS_xgrs_cal_src_pos", CASBITS_XGRSCONFIG }
, { "XREL", "CAS_xgrs_release_attitude", CASBITS_XGRSRELATT }
, { "XPARK", "CAS_xgrs_park_mode", CASBITS_XGRSPARK }

, { "OPNAV", "CAS_OPNAV", CASBITS_CASOPNAV }

#define OPNAVDESCR( OPNAVTYP) \
, { opnavNames[OPNAVTYP]+4, opnavNames[OPNAVTYP],CASBITS_CASOPNAV|(1L<<OPNAVTYP)

/*
OPNAVDESCR( OPNAVTYPE_A)
OPNAVDESCR( OPNAVTYPE_B)
OPNAVDESCR( OPNAVTYPE_C)
OPNAVDESCR( OPNAVTYPE_CACRUISE)
OPNAVDESCR( OPNAVTYPE_CE)
OPNAVDESCR( OPNAVTYPE_CT)
OPNAVDESCR( OPNAVTYPE_D)
OPNAVDESCR( OPNAVTYPE_E)
OPNAVDESCR( OPNAVTYPE_E2)
OPNAVDESCR( OPNAVTYPE_E3)
OPNAVDESCR( OPNAVTYPE_E4)
OPNAVDESCR( OPNAVTYPE_F)
OPNAVDESCR( OPNAVTYPE_G)
OPNAVDESCR( OPNAVTYPE_H)
OPNAVDESCR( OPNAVTYPE_I)
OPNAVDESCR( OPNAVTYPE_J)
OPNAVDESCR( OPNAVTYPE_K)
OPNAVDESCR( OPNAVTYPE_DOUBLE_K)
*/

, { (char *) 0, (char *) 0, 0L }
};

CASDESCR opnavDescr[OPNAVTYPE_COUNT];     /* contents filled in on the fly by */
                                          /* orbit_CAS_getCASDescr() */

#define CASTYPE_TOP CASTYPE_CAS
#define CASTYPE_TOPDEF CASTYPE_CASDEF

#define CASTOP_MAXARGSLEN 256

typedef
struct CASstr { /* container for any of the above */

  char _name[IDLEN];                                 /* ID of this structure */
  char _savename[IDLEN];     /* name on entering orbitgui_create_CAS_fldMenu */
  long _duration;                              /* duration, s (not used yet) */
  double _et;                     /* placeholder for start time, s after j2k */
  double *_ptrEt;               /* - _et only used if pointed at from _ptrEt */
  double _delayStart;                                   /* delay to start, s */
  double _met;                                            /* start time, MET */
  int _malloced;                                        /* 0 if not malloced */
  struct CASstr *next, *prev;                                 /* linked list */
  void *_miscPtr;                                      /* used for CASWStuff */
  int _type;                                             /* what is in union */
  long _enabled;                       /* =1, this CAS may be active, else 0 */
  long *_ctlVar;                  /* when optimising, this is the var to use */
  long _ctlLims[2];         /* when optimising, these are the control limits */
  double _olapLims[2];      /* when optimising, these are the overlap limits */
  long _noParams;                        /* set 1 if this frag has no params */
  struct CASstr *top;                                          /* parent CAS */
  struct CASstr *topNext;                  /* next CAS in list under REQuest */
  union {                                           /* - see CASTYPE_* above */
    AUTOEXPOSEFRAG *_auto;
    LOADFILTFRAG *_loadFilt;
    MSICONFIGFRAG *_msiConf;
    PARKFRAG *_park;
    RELFRAG *_rel;
    NISCAL *_nisCal;
    NISBUFFLUSH *_nisBufFlush;
    NISCONFIG *_nisConf;
    XGRSCONFIG *_xgrsConf;
    MSISEQDEF *_msiSeqDef;
    NISSEQDEF *_nisSeqDef;
    SHOOT *_msiShoot;
    MSIREP *_msiRep;
    NISREP *_nisRep;
    NISSU *_nisSu;
    DS56 *_ds56;
    DS40 *_ds40;

    CASOPNAV *_opnavCAS;                      /* defined in orbit_cas_opnav.h */

    struct CasCASstr {               /* start of list of CAS's under this one */
      struct CASstr *_casList[CASTYPE_COUNT];
      char *_argListPtrs[CASTYPE_COUNT];
      long _argListSize[CASTYPE_COUNT];
      long _bits;                         /* for creating/deleting sub-CAS's */
    } *_casCAS;

#define MAXNUMSEQDEF 60
    struct CasDefCASstr {    /* start of list of SEQDEF CAS's under this one */
      struct CASstr *_List[MAXNUMSEQDEF];
      char *_ListPtrs[MAXNUMSEQDEF];
      long _ListSize[MAXNUMSEQDEF];
      long _addMsi;                          /* for creating sub-CAS's */
      long _addNis;                          /* for creating sub-CAS's */
    } *_casDefCAS;

#define MAXLENFILNAM 256

    struct ReqCASstr {      /* start of list of top CAS[DEF]'s under this one */
      long _addTop;                                 /* for creating sub-CAS's */
      char _addCasTemplate[MAXLENFILNAM];
      long _addCasDef;                           /* for creating sub-CASDEF's */
      char _addCasDefTemplate[MAXLENFILNAM];
      long _addCasOpnavBits;                   /* for creating sub-CASOPNAV's */
      char _addCasOpnavTemplate[MAXLENFILNAM];
    } *_reqCAS;
  } _data;
} CAS;

typedef struct CasCASstr CASCAS;
typedef struct CasDefCASstr CASDEFCAS;
typedef struct ReqCASstr REQCAS;

/* where this cas came from (malloc'ed or not) */

enum { CAS_NOTMALLOC = 0
     , CAS_MALLOC
     , CAS_MALLOC_NEW
     };

/* macro to set value based on CAS */

#define WCAS( T, MSIitem, NISitem \
               , SHOOTitem, MSISRitem, MSIDRitem, MSIDSRitem \
               , NISSRitem, NISDRitem, NISEXitem \
               , DS56item, DS40item, DS40FULLitem \
               /* , NADIRitem not used */ \
               , MSITRitem \
               , AUTOEXPOSEitem \
               , LOADFILTitem \
               , MSICONFIGitem \
               , MSIPARKitem \
               , NISPARKitem \
               , MSIRELATTitem \
               , NISSU2item \
               , NISSU1item \
               , NISCALitem \
               , NISBUFFLUSHitem \
               , NISCONFIGitem \
               , NISRELATTitem \
               , DS40XGRSitem \
               , XGRSRELATTitem \
               , XGRSCONFIGitem \
               , XGRSPARKitem \
               , NAVRELATTitem, CASOPNAVitem \
               , CASitem, CASDEFitem, REQitem \
               , NULLitem) \
  ((T == CASTYPE_MSISEQDEF) ? MSIitem : \
  ((T == CASTYPE_NISSEQDEF) ? NISitem : \
  ((T == CASTYPE_SHOOT) ? SHOOTitem : \
  ((T == CASTYPE_MSISR) ? MSISRitem : \
  ((T == CASTYPE_MSIDR) ? MSIDRitem : \
  ((T == CASTYPE_MSIDSR) ? MSIDSRitem : \
  ((T == CASTYPE_NISSR) ? NISSRitem : \
  ((T == CASTYPE_NISDR) ? NISDRitem : \
  ((T == CASTYPE_NISEX) ? NISEXitem : \
  ((T == CASTYPE_DS56) ? DS56item : \
  ((T == CASTYPE_DS40) ? DS40item : \
  ((T == CASTYPE_DS40FULL) ? DS40FULLitem : \
  /* ((T == CASTYPE_NADIR) ? NADIRitem : */ \
  ((T == CASTYPE_MSITR) ? MSITRitem : \
  ((T == CASTYPE_AUTOEXPOSE) ? AUTOEXPOSEitem : \
  ((T == CASTYPE_LOADFILT) ? LOADFILTitem : \
  ((T == CASTYPE_MSICONFIG) ? MSICONFIGitem : \
  ((T == CASTYPE_MSIPARK) ? MSIPARKitem : \
  ((T == CASTYPE_NISPARK) ? NISPARKitem : \
  ((T == CASTYPE_MSIRELATT) ? MSIRELATTitem : \
  ((T == CASTYPE_NISSU2) ? NISSU2item : \
  ((T == CASTYPE_NISSU1) ? NISSU1item : \
  ((T == CASTYPE_NISCAL) ? NISCALitem : \
  ((T == CASTYPE_NISBUFFLUSH) ? NISBUFFLUSHitem : \
  ((T == CASTYPE_NISCONFIG) ? NISCONFIGitem : \
  ((T == CASTYPE_NISRELATT) ? NISRELATTitem : \
  ((T == CASTYPE_DS40XGRS) ? DS40XGRSitem : \
  ((T == CASTYPE_XGRSRELATT) ? XGRSRELATTitem : \
  ((T == CASTYPE_XGRSCONFIG) ? XGRSCONFIGitem : \
  ((T == CASTYPE_XGRSPARK) ? XGRSPARKitem : /**/ \
  ((T == CASTYPE_NAVRELATT) ? NAVRELATTitem : \
  ((T == CASTYPE_CASOPNAV) ? CASOPNAVitem : \
  ((T == CASTYPE_CAS) ? CASitem : \
  ((T == CASTYPE_CASDEF) ? CASDEFitem : \
  ((T == CASTYPE_REQ) ? REQitem : \
   NULLitem))))))))))) /* )nadir not used */ )))))))))))))))))))))))

/* macro to set CAS string based on CAS type - uses WCAS macro above */

#define WHOAMI(T) WCAS( T, "MSISEQDEF", "NISSEQDEF" \
                         , "SHOOT", "MSISR", "MSIDR", "MSIDSR" \
                         , "NISSR", "NISDR", "NISEX" \
                         , "DS56", "DS40", "DS40FULL" /* , "NADIR" */ \
                         , "MSITR" \
                         , "AUTOEXPOSE" \
                         , "LOADFILT" \
                         , "MSICONFIG" \
                         , "MSIPARK" \
                         , "NISPARK" \
                         , "MSIRELATT" \
                         , "NISSU2" \
                         , "NISSU1" \
                         , "NISCAL" \
                         , "NISBUFFLUSH" \
                         , "NISCONFIG" \
                         , "NISRELATT" \
                         , "DS40XGRS" \
                         , "XGRSRELATT" \
                         , "XGRSCONFIG" \
                         , "XGRSPARK" /**/\
                         , "NAVRELATT", "CASOPNAV" \
                         , "CAS", "CASDEF", "REQ" \
                         , "WSNBATGH-WHOAMI?")

/* CAS type tests */

#define isMSISEQDEF(C) ((C)._type == CASTYPE_MSISEQDEF)
#define isNISSEQDEF(C) ((C)._type == CASTYPE_NISSEQDEF)
#define isNISSR(C) ((C)._type == CASTYPE_NISSR)
#define isNISDR(C) ((C)._type == CASTYPE_NISDR)
#define isNISEX(C) ((C)._type == CASTYPE_NISEX)
#define isSHOOT(C) ((C)._type == CASTYPE_SHOOT)
#define isMSISR(C) ((C)._type == CASTYPE_MSISR)
#define isMSIDR(C) ((C)._type == CASTYPE_MSIDR)
#define isMSIDSR(C) ((C)._type == CASTYPE_MSIDSR)
#define isDS40(C) ((C)._type == CASTYPE_DS40)
#define isDS40FULL(C) ((C)._type == CASTYPE_DS40FULL)
#define isDS56(C) ((C)._type == CASTYPE_DS56)
#define isMSITR(C) ((C)._type == CASTYPE_MSITR)
#define isAUTOEXPOSE(C) ((C)._type == CASTYPE_AUTOEXPOSE)
#define isLOADFILT(C) ((C)._type == CASTYPE_LOADFILT)
#define isMSICONFIG(C) ((C)._type == CASTYPE_MSICONFIG)
#define isMSIPARK(C) ((C)._type == CASTYPE_MSIPARK)
#define isNISPARK(C) ((C)._type == CASTYPE_NISPARK)
#define isMSIRELATT(C) ((C)._type == CASTYPE_MSIRELATT)
#define isNISSU2(C) ((C)._type == CASTYPE_NISSU2)
#define isNISSU1(C) ((C)._type == CASTYPE_NISSU1)
#define isNISCAL(C) ((C)._type == CASTYPE_NISCAL)
#define isNISBUFFLUSH(C) ((C)._type == CASTYPE_NISBUFFLUSH)
#define isNISCONFIG(C) ((C)._type == CASTYPE_NISCONFIG)
#define isNISRELATT(C) ((C)._type == CASTYPE_NISRELATT)

/* xgrs 30.jul'1999 */

#define isXGRSPARK(C) ((C)._type == CASTYPE_XGRSPARK) /**/
#define isXGRSRELATT(C) ((C)._type == CASTYPE_XGRSRELATT)
#define isXGRSCONFIG(C) ((C)._type == CASTYPE_XGRSCONFIG)
#define isDS40XGRS(C) ((C)._type == CASTYPE_DS40XGRS)

#define isNAVRELATT(C) ((C)._type == CASTYPE_NAVRELATT)
#define isCASOPNAV(C) ((C)._type == CASTYPE_CASOPNAV)

#define isCASDEF(C) ((C)._type == CASTYPE_CASDEF)
#define isCAS(C) ((C)._type == CASTYPE_CAS)
#define isREQ(C) ((C)._type == CASTYPE_REQ)

#define isANYTOP(C) (isCASDEF(C) || isCAS(C) || isCASOPNAV(C) || isREQ(C))
#define isANYDS40(C) (isDS40(C) || isDS40FULL(C) || isDS40XGRS(C))
#define isRELATT(C) (isMSIRELATT(C) || isNISRELATT(C) || isXGRSRELATT(C) \
                   ||isNAVRELATT(C))

/***********************************************************/
/* calculate start time of a REQUEST, CAS[DEF] or FRAGMENT */
/* - method:  calculate base time, add local _delayStart at the end */

static double CASSTART( CAS *cas) {
double et = *cas->_ptrEt;
CAS *nextCAS;

  if ( !cas->top) return et + cas->_delayStart;       /* standalone structure */

  switch( cas->_type) {

  case CASTYPE_REQ:                  /* REQUEST - add _delayStart to et below */
    /* this path should never be followed 
     * - REQUESTs should have null top's
     */
    break;

  case CASTYPE_CAS:                                /* CAS, CASDEF or CASOPNAV */
  case CASTYPE_CASDEF:
  case CASTYPE_CASOPNAV:
    nextCAS = cas->top;
    while ( nextCAS != cas) {    /*  accumulate _delayStart's from parent REQ */
      et += nextCAS->_delayStart;
      nextCAS = nextCAS->topNext;
    }
    break;

  default:                     /* FRAGMENT - delay is relative to parent time */

    et = CASSTART(cas->top);              /* get et from parent CAS or CASDEF */
    break;
  }

  return et + cas->_delayStart;                       /* add local delayStart */
}

/*************************************************************/

/* CAS enabled test */

#define CASISENABLED( C) ((C)._enabled & 1L)

/* sub-structure/union macros */

/* XGRSCONFIG *_xgrsConf; */
#define xgrsConfPosn _data._xgrsConf->_longs[0]
#define xgrsConfDir _data._xgrsConf->_longs[1]
#define xgrsConfBits _data._xgrsConf->_bits

/* values, bits, long array & names array for xgrsConfPosn & xgrsConfDir */

enum { XGRSCONFIalPosn=0L    /* AL */
     , XGRSCONFIclearPosn    /* CLEAR */
     , XGRSCONFIhomePosn     /* HOME */
     , XGRSCONFImgPosn       /* MG */
     , XGRSCONFIforDir       /* FORWARD */
     , XGRSCONFIrevDir       /* REVERSE */
     };

#define XGRSCONFBITalPosn (1L<<XGRSCONFIalPosn)
#define XGRSCONFBITclearPosn (1L<<XGRSCONFIclearPosn)
#define XGRSCONFBIThomePosn (1L<<XGRSCONFIhomePosn)
#define XGRSCONFBITmgPosn (1L<<XGRSCONFImgPosn)
#define XGRSCONFBITforDir (1L<<XGRSCONFIforDir)
#define XGRSCONFBITrevDir (1L<<XGRSCONFIrevDir)

#define XGRSCONFBITall_Posn (XGRSCONFBITalPosn\
                            |XGRSCONFBITclearPosn\
                            |XGRSCONFBIThomePosn\
                            |XGRSCONFBITmgPosn)
#define XGRSCONFBITall_Dir (XGRSCONFBITforDir|XGRSCONFBITrevDir)

static long xgrsConfPosnArray[] = { 
XGRSCONFIalPosn, XGRSCONFIclearPosn, XGRSCONFIhomePosn, XGRSCONFImgPosn , -1L };
static char *xgrsConfPosnNames[] = { "AL", "CLEAR", "HOME", "MG", (char *) 0 };
#define XGRSCONFI000Posn XGRSCONFIalPosn

static long xgrsConfDirArray[] = { XGRSCONFIforDir, XGRSCONFIrevDir, -1L };
static char *xgrsConfDirNames[] = { "FORWARD", "REVERSE", (char *) 0 };
#define XGRSCONFI000Dir XGRSCONFIforDir

/* AUTOEXPOSEFRAG *_auto; */
#define msiAutoTImDelta _data._auto->_longs[0]
#define msiAutoFile4TIm _data._auto->_longs[1]
#define msiAutoTImExpTime _data._auto->_longs[2]
#define msiAutoAllowSat _data._auto->_longs[3]
#define msiAutoTargSatDN _data._auto->_longs[4]
#define msiAutoOvrExpFlbkTim _data._auto->_longs[5]
#define msiAutoNoiseOffset _data._auto->_longs[6]

/* LOADFILTFRAG *_loadFilt; */
#define msiLoadFiltSensArray _data._loadFilt->_longs

/* MSICONFIGFRAG *_msiConf; */
#define msiConfFullIm _data._msiConf->_longs[0]
#define msiConfSumIm _data._msiConf->_longs[1]
#define msiConfBits _data._msiConf->_bits

/* values, bits, long array & names array for msiConfFullIm & msiConfSumIm */

enum { MSICONFInoneFullIm = 0   /* NONE */
     , MSICONFIunchFullIm       /* UNCHANGED */
     , MSICONFIvc2_FullIm       /* VC2 */
     , MSICONFInoneSumIm        /* NONE */
     , MSICONFIunchSumIm        /* UNCHANGED */
     , MSICONFIvc30SumIm        /* VC30 */
     };
#define MSICONFBITnoneFullIm (1L<<MSICONFInoneFullIm)
#define MSICONFBITunchFullIm (1L<<MSICONFIunchFullIm)
#define MSICONFBITvc2_FullIm (1L<<MSICONFIvc2_FullIm)
#define MSICONFBITnoneSumIm (1L<<MSICONFInoneSumIm)
#define MSICONFBITunchSumIm (1L<<MSICONFIunchSumIm)
#define MSICONFBITvc30SumIm (1L<<MSICONFIvc30SumIm)
#define MSICONFBITall_FullIm (MSICONFBITnoneFullIm|MSICONFBITunchFullIm| \
                             MSICONFBITvc2_FullIm)
#define MSICONFBITall_SumIm (MSICONFBITnoneSumIm|MSICONFBITunchSumIm| \
                             MSICONFBITvc30SumIm)

static long msiConfFullImArray[] = 
  { MSICONFInoneFullIm, MSICONFIunchFullIm, MSICONFIvc2_FullIm, -1L };
static char *msiConfFullImNames[] =
  { "NONE", "UNCHANGED", "VC2", (char *) 0 };
#define MSICONFI000FullIm MSICONFInoneFullIm

static long msiConfSumImArray[] = 
  { MSICONFInoneSumIm, MSICONFIunchSumIm, MSICONFIvc30SumIm, -1L };
static char *msiConfSumImNames[] =
  { "NONE", "UNCHANGED", "VC30", (char *) 0 };
#define MSICONFI000SumIm MSICONFInoneSumIm

/* PARKFRAG *_park;              - no parameters */
/* NISCAL *_nisCal;              - no parameters */
/* NISBUFFLUSH *_nisBufFlush;    - no parameters */
/* RELFRAG *_rel;                - no parameters */

/* NISCONFIG *_nisConf; */
#define nisConfSlitDriveSel _data._nisConf->_longs[0]
#define nisConfShutDriveSel _data._nisConf->_longs[1]
#define nisConfVoltSet _data._nisConf->_longs[2]
#define nisConfScanDriveSel _data._nisConf->_longs[3]
#define nisConfBits _data._nisConf->_bits

/* values, bits, long array & names array for nisConf* */

enum { NISCONFInochSlitDriveSel = 0   /* NOCHANGE */
     , NISCONFIpri_SlitDriveSel       /* PRI */
     , NISCONFIsec_SlitDriveSel       /* SEC */
     , NISCONFInochShutDriveSel       /* NOCHANGE */
     , NISCONFIpri_ShutDriveSel       /* PRI */
     , NISCONFIsec_ShutDriveSel       /* SEC */
     , NISCONFInochVoltSet            /* NOCHANGE */
     , NISCONFI15v_VoltSet            /* 15V */
     , NISCONFI20v_VoltSet            /* 20V */
     , NISCONFInochScanDriveSel       /* NOCHANGE */
     , NISCONFIpri_ScanDriveSel       /* PRI */
     , NISCONFIsec_ScanDriveSel       /* SEC */
     };
#define NISCONFBITnochSlitDriveSel (1L<<NISCONFInochSlitDriveSel)
#define NISCONFBITpri_SlitDriveSel (1L<<NISCONFIpri_SlitDriveSel)
#define NISCONFBITsec_SlitDriveSel (1L<<NISCONFIsec_SlitDriveSel)
#define NISCONFBITnochShutDriveSel (1L<<NISCONFInochShutDriveSel)
#define NISCONFBITpri_ShutDriveSel (1L<<NISCONFIpri_ShutDriveSel)
#define NISCONFBITsec_ShutDriveSel (1L<<NISCONFIsec_ShutDriveSel)
#define NISCONFBITnochVoltSet (1L<<NISCONFInochVoltSet)
#define NISCONFBIT15v_VoltSet (1L<<NISCONFI15v_VoltSet)
#define NISCONFBIT20v_VoltSet (1L<<NISCONFI20v_VoltSet)
#define NISCONFBITnochScanDriveSel (1L<<NISCONFInochScanDriveSel)
#define NISCONFBITpri_ScanDriveSel (1L<<NISCONFIpri_ScanDriveSel)
#define NISCONFBITsec_ScanDriveSel (1L<<NISCONFIsec_ScanDriveSel)

#define NISCONFBITall_SlitDriveSel (NISCONFBITnochSlitDriveSel|\
 NISCONFBITpri_SlitDriveSel|NISCONFBITsec_SlitDriveSel)
#define NISCONFBITall_ShutDriveSel (NISCONFBITnochShutDriveSel|\
 NISCONFBITpri_ShutDriveSel|NISCONFBITsec_ShutDriveSel)
#define NISCONFBITall_VoltSet (NISCONFBITnochVoltSet|\
 NISCONFBIT15v_VoltSet|NISCONFBIT20v_VoltSet)
#define NISCONFBITall_ScanDriveSel (NISCONFBITnochScanDriveSel|\
 NISCONFBITpri_ScanDriveSel|NISCONFBITsec_ScanDriveSel)

static long nisConfSlitDriveSelArray[] = { NISCONFInochSlitDriveSel\
               , NISCONFIpri_SlitDriveSel, NISCONFIsec_SlitDriveSel, -1L };
static long nisConfShutDriveSelArray[] = { NISCONFInochShutDriveSel\
               , NISCONFIpri_ShutDriveSel, NISCONFIsec_ShutDriveSel, -1L };
static long nisConfVoltSetArray[] = { NISCONFInochVoltSet\
               , NISCONFI15v_VoltSet, NISCONFI20v_VoltSet, -1L };
static long nisConfScanDriveSelArray[] = { NISCONFInochScanDriveSel\
               , NISCONFIpri_ScanDriveSel, NISCONFIsec_ScanDriveSel, -1L };

static char *nisConfDriveSelNames[] = { "NOCHANGE", "15V", "20V", (char *) 0 };
#define nisConfSlitDriveSelNames nisConfDriveSelNames
#define nisConfShutDriveSelNames nisConfDriveSelNames
#define nisConfScanDriveSelNames nisConfDriveSelNames
static char *nisConfVoltSetNames[] = { "NOCHANGE", "15V", "20V", (char *) 0 };

#define NISCONFI000SlitDriveSel NISCONFInochSlitDriveSel
#define NISCONFI000ShutDriveSel NISCONFInochShutDriveSel
#define NISCONFI000VoltSet NISCONFInochVoltSet
#define NISCONFI000ScanDriveSel NISCONFInochScanDriveSel

/* NISSU *_nisSu SU2 & SU */

#define nisSuRepDelay _data._nisSu->_repDelay
#define nisSuSeq _data._nisSu->_seq
#define nisSuMirrorPosn _data._nisSu->_mirrorPosn
#define nisSuAperture _data._nisSu->_aperture
#define nisSuGain _data._nisSu->_gain
#define nisSuPpss _data._nisSu->_ppss
#define nisSuStepDir _data._nisSu->_stepDir
#define nisSuNoss _data._nisSu->_noss
#define nisSuBits _data._nisSu->_bits

/* NISREP *_nisRep */

#define nisRepDelay _data._nisRep->_repDelay
#define nisRepSetup _data._nisRep->_repSetup
#define nisRepMirrorPosnSetup _data._nisRep->_repMirrorPosnSetup
#define nisRepApertureSetup _data._nisRep->_repApertureSetup
#define nisRepGainSetup _data._nisRep->_repGainSetup
#define nisRepBitsSetup _data._nisRep->_repBitsSetup
#define nisRepSeq _data._nisRep->_seq
#define nisRepDel _data._nisRep->_del
#define nisRepIter _data._nisRep->_iter
#define nisRepMirrorPosn _data._nisRep->_mirrorPosn
#define nisRepAperture _data._nisRep->_aperture
#define nisRepGain _data._nisRep->_gain
#define nisRepBits _data._nisRep->_bits

/* SHOOT *_msiShoot */

#define msiShootCount _data._msiShoot->_count
#define msiShootSeq _data._msiShoot->_seq
#define msiShootDel _data._msiShoot->_del
#define msiShootImageTypeDbl _data._msiShoot->_imageTypeDbl

/* MSIREP *_msiRep */

#define msiRepSeq _data._msiRep->_seq
#define msiRepDelay _data._msiRep->_repDelay
#define msiRepDel _data._msiRep->_del
#define msiRepIter _data._msiRep->_iter
#define msiRepSeq2Start _data._msiRep->_seq2Start
#define msiRepImageType _data._msiRep->_imageType
#define msiRepImageTypeDbl _data._msiRep->_imageTypeDbl
#define msiRepBits _data._msiRep->_bits
#define msiRepImageTypeDblTR _data._msiRep->_imageTypeDblTR
#define msiRepBitsTR _data._msiRep->_bitsTR

#define ds40Bits _data._ds40->_bits
#define ds40AimptFrmType _data._ds40->_aimptFrmType 
#define ds40AimptVec _data._ds40->_aimptVec
#define ds40ScRollVec _data._ds40->_scRollVec
#define ds40AimptSelect _data._ds40->_aimptSelect
#define ds40VbVec _data._ds40->_vbVec
#define ds40RollRefVec _data._ds40->_rollRefVec
#define ds40RollFrmType _data._ds40->_rollFrmType
#define ds40SlewDuration _data._ds40->_slewDuration

#define ds56FrmType _data._ds56->_frmType 
#define ds56ReUse _data._ds56->_reUse 
#define ds56RateVec _data._ds56->_rateVec 
#define ds56Bits _data._ds56->_bits
#define ds56RatePauseDur _data._ds56->_ratePauseDur
#define ds56ScanDur _duration

#define msiSeqDefIdNum _data._msiSeqDef->_idNum
#define msiSeqDefNumImages _data._msiSeqDef->_numImages
#define msiSeqDefInterval _data._msiSeqDef->_interval
#define msiSeqDefBits _data._msiSeqDef->_bits
#define msiSeqDefMsecExp _data._msiSeqDef->_msecExp
#define msiSeqDefFilt _data._msiSeqDef->_filt
#define msiSeqDefPixels _data._msiSeqDef->_pixels
#define msiSeqDefCmpTbl _data._msiSeqDef->_cmpTbl
#define msiSeqDefCmpAlg _data._msiSeqDef->_cmpAlg
#define msiSeqDefDpcm _data._msiSeqDef->_dpcm
#define msiSeqDefMode _data._msiSeqDef->_mode
#define msiSeqDefTopIndex _data._msiSeqDef->_topIndex

#define nisIdNum _data._nisSeqDef->_idNum
#define nisNumScans _data._nisSeqDef->_numScans
#define nisNumObs _data._nisSeqDef->_numObs
#define nisCalInterval _data._nisSeqDef->_calInterval
#define nisNumRests _data._nisSeqDef->_numRests
#define nisNumDarks _data._nisSeqDef->_numDarks
#define nisStepMirror _data._nisSeqDef->_stepMirror
#define nisSecPerObs _data._nisSeqDef->_timings[0]
#define nisSecBtwObs _data._nisSeqDef->_timings[1]
#define nisSecBtwScan _data._nisSeqDef->_timings[2]
#define nisTopIndex _data._nisSeqDef->_topIndex

#define reqAddTop _data._reqCAS->_addTop
#define reqAddCasTemplate _data._reqCAS->_addCasTemplate
#define reqAddCasDef _data._reqCAS->_addCasDef
#define reqAddCasDefTemplate _data._reqCAS->_addCasDefTemplate
#define reqAddCasOpnavBits _data._reqCAS->_addCasOpnavBits
#define reqAddCasOpnavTemplate _data._reqCAS->_addCasOpnavTemplate

#define REQ_SCHED_POINT "REQ_SCHED_POINT"
#define REQ_START_DELAY "REQ_START_DELAY"

#define casDefList _data._casDefCAS->_List
#define casDefListPtrs _data._casDefCAS->_ListPtrs
#define casDefListSize _data._casDefCAS->_ListSize
#define casDefAddMsi _data._casDefCAS->_addMsi
#define casDefAddNis _data._casDefCAS->_addNis
#define casDefBits _data._casDefCAS->_bits

#define CASDEF_SCHED_POINT "CASDEF_SCHED_POINT"
#define CASDEF_START_DELAY "CASDEF_START_DELAY"

#define casCasList _data._casCAS->_casList
#define casArgListPtrs _data._casCAS->_argListPtrs
#define casArgListSize _data._casCAS->_argListSize
#define casBits _data._casCAS->_bits

#define CAS_SCHED_POINT "CAS_SCHED_POINT"
#define CAS_START_DELAY "CAS_START_DELAY"

#define TOP_SCHED_POINT "TOP_SCHED_POINT"   /* for backwards compatibility */
#define TOP_START_DELAY "TOP_START_DELAY"   /*  " */

/* CAS-related routines - see also orbit_cas.c */

static long
int orbit_CAS_isEnabled( CAS *cas) {
  while ( cas->top && CASISENABLED( *cas)) cas = cas->top;
  return (cas->_enabled & 1L);
}

void orbit_CAS_ds40Vec( POINTING *bore        /* bore structure to modify */
                      , POINTING *roll        /* roll structure to modify */
                      , CAS *cas              /* 1st CAS ptr in linked list */
                      , double et             /* s past j2k */
                      );

void orbit_CAS_ds56Vec( VEC                   /* aimpt or virtual bore IN */
                      , CAS *cas              /* 1st CAS ptr in linked list */
                      , int ds56RefFrameType  /* aimpt reffrm | SBF for vbore */
                      , double et             /* s past j2k */
                      , VEC aimpt_or_scvec_OUT/* aimpt or virtual bore OUT */
                      );

CAS * orbit_CAS_new( int casType);
CAS * orbit_CAS_getCASByName( CAS *startCAS, char *target_name);

CAS * orbit_CAS_getMSISeqDefByNum( CAS *startCAS, double et, long seqNum);
CAS * orbit_CAS_getNISSeqDefByNum( CAS *startCAS, double et, long seqNum);

CAS * orbitgui_get1stCASFromCurItem( void *cur_item_target);

void orbit_CAS_addTopArgs( CAS *topCas, int type, char *argList);
CAS *orbit_CAS_ArgsToCAS( char *, CAS *);
void orbit_CAS_readFile( char *, void *, void (*)(CAS *));
void orbit_CAS_readFile_underTop( char *, void *, CAS *, void (*)(CAS *));

int orbit_CAS_NameToType( char *);
char *orbit_CAS_TypeToName( int);
char *orbit_CAS_FragToName( CAS *);
char *orbit_CAS_CASToArgs( CAS *);
void orbit_CAS_updateTop( CAS *);
IMGFRM *orbit_gen_anyInstrFrames( ORBIT *o, IMGFRM *i, long *framesLeft);

/***********************************************************/
/* fancy stuff for CASTYPE_CASOPNAV */

typedef struct {
  long _casTypeIdx, _opnavFragIdx;
} SUBFRAGS;

static SUBFRAGS subFrags[] = {
  CASTYPE_SHOOT, OPNAVFRAG_SHOOT
, CASTYPE_MSISR, OPNAVFRAG_MSISR
, CASTYPE_DS40, OPNAVFRAG_DS40
, CASTYPE_DS56, OPNAVFRAG_DS56
, CASTYPE_NAVRELATT, OPNAVFRAG_ATT
, -1L, -1L
};

#define OPNAVFRAGPTR(F,OPNAVFRAGIDX) \
( ((OPNAVFRAGIDX)==OPNAVFRAG_SHOOT) ? (F)->opnavShoot : \
( ((OPNAVFRAGIDX)==OPNAVFRAG_MSISR) ? (F)->opnavMsiSR : \
( ((OPNAVFRAGIDX)==OPNAVFRAG_DS40) ? (F)->opnavDs40 : \
( ((OPNAVFRAGIDX)==OPNAVFRAG_DS56) ? (F)->opnavDs56 : \
( ((OPNAVFRAGIDX)==OPNAVFRAG_ATT) ? (F)->opnavNavRelAtt : (CAS **) NULL \
)))))

#define FOROPNAVSUBFRAGS(F,SUBFRAGS,CASPTR) \
  for ( SUBFRAGS=subFrags; SUBFRAGS->_opnavFragIdx != -1; ++SUBFRAGS) { \
  long iFrag; \
    CASPTR = OPNAVFRAGPTR( F, SUBFRAGS->_opnavFragIdx); \
    for ( iFrag=0 \
        ; iFrag<opnavArgFragCount[(F)->opnavSubType][SUBFRAGS->_opnavFragIdx] \
        ; ++iFrag, ++CASPTR) { \

#endif /* #ifndef _ORBIT_CAS_H_ */ /* end orbit_cas.h */
