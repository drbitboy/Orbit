/* orbit3d.h */

#ifndef _ORBIT3D_H_
#define _ORBIT3D_H_

#include <string.h>
#include <math.h>
#include "orbitfort.h"
#include "orbitgui_framesout.h"

static double tswap;

#define SWAP(A,B,C) C=A; A=B; B=C

#define MTSWAP(I,J,M) SWAP((M)[I],(M)[J],tswap)

#define MT(M) MTSWAP(1,3,(M)); MTSWAP(2,6,(M)); MTSWAP(5,7,(M));

#define LOADVEC(X,Y,Z,D) (D)[0]=(X); (D)[1]=(Y); (D)[2]=(Z)
#define CPYVEC(S,D) memcpy((D),(S),VECSIZ*sizeof(double))
#define CPYMTX(S,D) memcpy((D),(S),MTXSIZ*sizeof(double))

#define MT2(S,D) CPYMTX((S),(D)); MT(D)

#define VSCAL2(K,S,D) (D)[0]= (K)*(S)[0]; (D)[1]= (K)*(S)[1]; (D)[2]= (K)*(S)[2]
#define VSCAL(K,V) VSCAL2(K,V,V)

#define VNEG2(S,D) (D)[0]= -(S)[0]; (D)[1]= -(S)[1]; (D)[2]= -(S)[2]
#define VNEG(V) VNEG2(V,V)

#define VADD2(V,VV,VVV) \
  (VVV)[0]= (V)[0]+(VV)[0]; (VVV)[1]= (V)[1]+(VV)[1]; (VVV)[2]= (V)[2]+(VV)[2]
#define VMINUS2(V,VV,VVV) \
  (VVV)[0]= (V)[0]-(VV)[0]; (VVV)[1]= (V)[1]-(VV)[1]; (VVV)[2]= (V)[2]-(VV)[2]

#define VDOT(V,VV) ((double) \
  ( ((V)[0] * (VV)[0]) + ((V)[1] * (VV)[1]) + ((V)[2] * (VV)[2]) ) ) 
#define VLEN(V) sqrt( VDOT(V,V))

#define VECSIZ 3
#define VEC6SIZ 6
#define MTXSIZ 9
typedef double VEC[VECSIZ];
typedef double VEC6[VEC6SIZ];
typedef double MTX[MTXSIZ];
typedef double *MVp;

#define MTXOUT(A,B) "%s%15lg%15lg%15lg\n%15lg%15lg%15lg\n%15lg%15lg%15lg\n" \
                , A, B[0], B[1], B[2], B[3], B[4], B[5], B[6], B[7], B[8]

#define VEC6OUT(A,B) "%s%15lg%15lg%15lg\n%15lg%15lg%15lg\n" \
               , A, B[0], B[1], B[2], B[3], B[4], B[5]

#define VEC3OUT(A,B) "%s%15lg%15lg%15lg\n" \
               , A, B[0], B[1], B[2]

#define UTCLEN 50    /* max length of UTC/TDT string */
#define IDLEN 50     /* max length of AGEN name string */

enum { SC_NIS=0L       /* nis must be 0 & 1st; nis2 must be last */
     , SC_MSI
     , SC_NIS2
     , SC_NUMINSTR};   /* non-existent, used to end loops */

enum { OVERLAP_TYPE_ANN_BIT=0
     , OVERLAP_TYPE_JIM_BIT
     , OVERLAP_TYPE_FOV100_BIT
     , OVERLAP_TYPE_JIM_NEW_BIT
     , OVERLAP_TYPE_LAST_BIT
     };

enum { AGEN_TYPE_OLAP=0     /* - adjust time btw frames, constant overlap */
     , AGEN_TYPE_OLAP2      /* - adjust time btw frames, overlap in a range */
     , AGEN_TYPE_TIMING     /* - from timing */
     , AGEN_TYPE_FRAG       /* - from one of SHOOT, MSISR/DR/DSR, NISEX/SR/DR */
     };

/* Structure to save auto-generation parameters */

typedef struct AGENstruct {

  char _utc[UTCLEN]
     , _name[IDLEN]
     ;

  void **_addGenFrames;   /* actually an IMGFRM ** to put gen'ed frames */

  double _MaxOrbits       /* duration in # orbits, 0 is ignored */
       , _MaxTime         /* duration in seconds, 0 is ignored */
       , _FrmOverlap      /* frame overlap spec, fraction */
       , _FrmOverlap2     /* 2nd frame overlap spec, fraction */
       , _EtStart         /* epoch at start of frames, s past J2000 */
       ;

  int _typeAgen;            /* type of autogen - */

  /* generation method MSI0 */

  /* int _TimePerMSIFrm        /* time per image frame */
  double _TimePerMSIFrm        /* time per image frame */
    ;

  /* generation method NIS0 */

  int _StartingStep
    , _EndingStep
    , _DeltaStep
    , _TimePerStep
    , _DelayPerStep
    , _DelayPerRow
    ;

  long _NISFOV
     , _FrmOverlapTypeBits
     , _refs;               /* # of frames referring to this AGEN struct */
     ;

  void *_seqDef;            /* MSI or NIS seqdef cas */
  int _fragType;           /* CASTYPE_... */
  int _seqDefType[12];     /* CASTYPE_...SEQDEF */
  char *_fragArgs;          /* NIS/MSI Repeat Fragment's arguments */
  char *_seqDefArgs[12];    /* NIS/MSI Sequence Definitions' arguments */
                            /* ***N.B. malloc'ed memory (via strdup) */

} AGEN;

/* structure to define orbits */

#define ORB_USESPK 0
#define ORB_USESO 1
#define ORB_USEAA 2

#define REFRMLEN 10  /* max length of reference frame string */

typedef struct ORBITstruct {

  fortint _status;    /* Type of orbit ORB_USESPK, ORB_USEAA or ORB_USESO */


  double _aa[8];      /* Astronomical Almanac orbital elements: */
  char _aa_tdt[UTCLEN];         /* epoch, tdt */

  char _aa_refrm[REFRMLEN];     /* reference frame, e.g. "eclipb1950" */
  fortint _aa_irefrm;           /* - ref. frame index */

#define AA_INC 0                /* Incidence angle, deg */
#define _aa_inc _aa[AA_INC]
#define AA_OMEGA 1              /* longitude of ascending node, deg */
#define _aa_omega _aa[AA_OMEGA]
#define AA_W 2                  /* argument of perihelion, deg */
#define _aa_w _aa[AA_W]
#define AA_AU 3                 /* mean distance, AU */
#define _aa_au _aa[AA_AU]
#define AA_N 4                  /* daily motion, deg/d */
#define _aa_n _aa[AA_N]
#define AA_E 5                  /* eccentricity */
#define _aa_e _aa[AA_E]
#define AA_M 6                  /* mean anomaly, deg */
#define _aa_m _aa[AA_M]
#define AA_ET 7                 /* epoch, s after J2000 */
#define _aa_et _aa[AA_ET]

  double _so[10];  /* SPICE orbital elements for CONICS: */
  char _so_utc[UTCLEN];         /* epoch, utc */
  fortint _so_precess;          /* set to one to estimate precession of
                                   lnode & argp around non-spherical body */
  double _so_J2;                /* J2 of oblate spheroid */
  double _so_Rpl;               /* Equivalent equatorial Radius of body */
  VEC _so_uPA0;                /* unit vector toward periapse at T=0 */
  VEC _so_uTP0;                /* unit vector toward trajectory pole at T=0 */

#define SO_RP 0                 /* perifocal distance, km */
#define _so_rp _so[SO_RP]
#define SO_ECC 1                /* eccentricity */
#define _so_ecc _so[SO_ECC]
#define SO_INC 2                /* inclination, rad */
#define _so_inc _so[SO_INC]
#define SO_LNODE 3              /* longitude of ascending node, rad */
#define _so_lnode _so[SO_LNODE]
#define SO_ARGP 4               /* argument of perihelion, rad */
#define _so_argp _so[SO_ARGP]
#define SO_M0 5                 /* mean anomaly at epoch, rad */
#define _so_m0 _so[SO_M0]
#define SO_T0 6                 /* epoch, s past J2000 */
#define _so_t0 _so[SO_T0]
#define SO_MU 7                 /* GM1, km^3/s^2 */
#define _so_mu _so[SO_MU]
#define SO_LNODEDOT 8           /* d(lnode)/dt, rad/s */
#define _so_lnodedot _so[SO_LNODEDOT]
#define SO_ARGPDOT 9            /* d(argp)/dt, rad/s */
#define _so_argpdot _so[SO_ARGPDOT]


#define GRAVCON 6.6720E-20      /* km^3/(kg-s^2) */
#define KMPERAU 1.49457E+08     /* km/AU */

  double _period;               /* orbital period, s */
  double _rho;                  /* density of central body, g/cc */
  MTX _toj2k;         /* matrix to convert orbital position to J2000 */

  void *_sc;    /* pointer back to SCstruct that contains this ORBIT */

/* put new values here until final OK pushed */

  fortint _newcenterid; /* central body id */
  fortint _newstat;     /* _status */
  double _newvals[10];   /* _aa or _so */
  fortint _newirefrm;   /* _irefrm */
  char _newutc[UTCLEN]; /* _aa_tdt or _so_utc */
  char _newrefrm[REFRMLEN]; /* _aa_refrm */
  fortint _newprecess; /* _so_precess */
  MTX _newtoj2k;        /* _toj2k */

/* for generation of frames */

  double _newgenvals[4];
  char _newgenutc[UTCLEN];

  AGEN _agen;

  long _framesoutBits[FRAMESOUT_NUMROWS+1];  /* last member is for CK output */

#define _ckOutBit _framesoutBits[FRAMESOUT_NUMROWS]

#define OVERLAP_TYPE_LABEL "Overlap type:\0Annbo the Fly\0Jimbo the Cyclops\0FOV 100\0"

#define OVERLAP_TYPE_FIRST_BIT OVERLAP_TYPE_ANN_BIT

} ORBIT;

#define _boresight _b0
typedef struct SCstruct {

  fortint _instrument;
  char _instrName[20];

  double *_b0
  ,   *_updir    /* up direction of current instrument frame in s/c coord's */
  ,   *_sampdir  /* right direction of current instrument frame in s/c coord's*/
  ;

  /* S/C vectors from PCK */

  VEC _scB0       /* Instrument Boresight in s/c coordinates */
  ,   _panel    /* Solar panels / Antenna */

  /* MSI */

  ,   _msiB0
  ,   _msiUpDir
  ,   _msiSampDir

  /* NIS */

  ,   _nisNomB0      /* NIS boresight at nominal step number */
  ,   _nisB0         /* actual NIS boresight at step _nisStepAct */
  ,   _nisNomUpDir   /* nominal up direction of NIS frame in s/c coord's */
  ,   _nisUpDir      /* up direction of NIS frame in s/c coord's */
  ,   _nisNomSampDir /* nominal right direction of NIS frame in s/c coord's */
  ,   _nisSampDir    /* actual right direction of NIS frame in s/c coord's */
  ,   _nisAxis       /* rotation axis of NIS mirror */
  
  /* Virtual Boresight and S/C Roll Vectors - user input */

  ,   _vb0      /* Virtual Boresight in s/c coord's */
  ,   _rb0      /* Roll Boresight in s/c coord's */

  /* Solution vectors in asteroid body fixed coordinates */

  ,   _babf     /* boresight in abf */
  ,   _vbabf    /* virt. b/s in abf */
  ,   _rbabf    /* roll b/s in abf */
  ,   _scFromAstAbf   /* asteroid to s/c vec in abf coord's */
  ,   _sunFromAstAbf   /* asteroid to sun vec in abf coord's */
  ,   _sunFromScAbf   /* s/c to sun vec in abf coord's */
  ,   _earthFromScAbf   /* asteroid to s/c vec in abf coord's */
  ;

  double _asterad[3]    /* triax. ellips. radii */
  ,   _maxrad           /* maximum of _asterad */
  ,   _fovy, _fovx      /* current instrument FOV values */
  ,   _msifovy, _msifovx /* MSI FOV values */
  ,   _nisfovy, _nisfovx /* NIS FOV values */
  ,   _nisStepAngle     /* NIS rotation per step, radians */
  ;

  double _et;           /* s past J2000 */

  int _numInstr    /* number of instruments loaded so far */
  ,   _numInstrAlloc /* number of instruments allocated so far */
  ;

  VEC *_instrUpdir /* unit vectors - up directions in FOV for all instruments */
  ,   *_instrSampdir /* "          - right "                                  */
  ;

  double
      *_camvert /* vecs from s/c to current instrument frame verts-s/c coords */
  ,   *_camabf  /*  " - abf coordinates */
  ,   *_msivert /*  " for MSI instrument */
  ,   *_msiabf  /*         " */
  ,   *_nisvert /*  " for NIS instrument */
  ,   *_nisabf  /*         " */
  ,   **_instrVertPtrs  /* " for all instr - MALLOC*2 */
  ,   **_instrAbfPtrs   /* " for all instr - MALLOC*2 */
  ,   *_fov100Pts   /* ~100 pts in FOV for overlap calcs - current instrument */
  ,   **_fov100InstrPtsPtrs             /* " - for all instruments - MALLOC*2 */
  ;
      /* # of fov points in FOV - allocate +10%, allow +10% / -0% */
#     define NUMFOV100PTS 100L
#     define MAXFOV100PTS ((110L*NUMFOV100PTS)/100L) /* alloc # Pts per instr */
#     define MINFOV100PTS NUMFOV100PTS

  fortint _ncampts         /* # of vertices in current instr camvert & camabf */
  ,    *_nClosedPts        /* # of vertices in current instr's closed polygon */
  ,    *_nFov100Pts              /* # of points in current instr's _fov100Pts */
  ,    _nmsipts                     /* number of vertices in msivert & msiabf */
  ,    _nnispts                     /* number of vertices in nisvert & nisabf */
  ,    *_nVert    /* array of # of vertices each of _instrVertPtrs[] - MALLOC */
  ,    *_nInstrClosedPtsPtr   /* # verts in closed poly in all instr - MALLOC */
  ,    *_nFov100InstrPtsPtr    /* # of points in _fov100InstrPtsPtrs - MALLOC */
  ,    _nisStepNom  /* nominal step number of NIS */
  ,    _nisStepMax  /* step number of maximum NIS boresight */
  ,    _nisStepAct  /* step number of actual NIS boresight */
  ,    _scid        /* spice body id codes */
  ,    _asterid
  ,    _sunid
  ,    _earthid
  ;

#      define _nMsiClosedPts _nInstrClosedPtsPtr[SC_MSI]
#      define _nNisClosedPts _nInstrClosedPtsPtr[SC_NIS]

  MTX _msctoj2k  /* matrix to convert from s/c to j2000 */
  ,   _mj2ktosc  /* matrix to convert from j2000 to s/c */
  ,   _mj2ktoabf /* matrix to convert from j2000 to abf */
  ,   _mabftoj2k /* matrix to convert from abf to j2000 */
  ,   _msctoabf  /* matrix to convert from s/c to abf */
  ,   _mabftosc  /* matrix to convert from abf to s/c */
  ,   _msctocam  /* matrix to convert from s/c to camera */
  ,   _mcamtosc  /* matrix to convert from camera to s/c */
  ,   _mabftocam /* matrix to convert from abf to camera */
  ,   _mcamtoabf /* matrix to convert from camera to abf */
  ;

  ORBIT *_scOrbit
  , *_asterOrbit
  , *_earthOrbit
  ;

  int _jjmode
# define JJMODE_MOSAIC 0
# define JJMODE_ORBIT  1
# define JJMODE_NORMAL 2
# define JJMODE_LAST   JJMODE_NORMAL

  , _doFov100    /* non-zero to copy & rotate fov100 points from SC to IMGFRM */
  , _tryck                                     /* get pointing from C-Kernels */
  ;
  struct SCstruct *nextsc, *prevsc;

  void *_cur_item; /* cur_item that holds this SCstruct */
} SC;


/* bits for _doFov100 above; setting any bit will cause invocation of 
 * orbit_fov_loadFov100()
 */
enum { 
  DOFOV100_GLOBAL=0           /* whether fov100 points are visible in display */
, DOFOV100_OVERLAP     /* whether fov100 points are used to calculate overlap */
};

#define DOFOV100_SETGLOBAL( SCPTR) DOFOV100_SETBIT( SCPTR, DOFOV100_GLOBAL)
#define DOFOV100_CLRGLOBAL( SCPTR) DOFOV100_CLRBIT( SCPTR, DOFOV100_GLOBAL)
#define DOFOV100_TSTGLOBAL( SCPTR) DOFOV100_TSTBIT( SCPTR, DOFOV100_GLOBAL)

#define DOFOV100_SETOVERLAP( SCPTR) DOFOV100_SETBIT( SCPTR, DOFOV100_OVERLAP)
#define DOFOV100_CLROVERLAP( SCPTR) DOFOV100_CLRBIT( SCPTR, DOFOV100_OVERLAP)
#define DOFOV100_TSTOVERLAP( SCPTR) DOFOV100_TSTBIT( SCPTR, DOFOV100_OVERLAP)

#define DOFOV100_SETBIT(SCPTR, BITPOS) ORBIT3D_SETBIT((SCPTR)->_doFov100,BITPOS)
#define DOFOV100_CLRBIT(SCPTR, BITPOS) ORBIT3D_CLRBIT((SCPTR)->_doFov100,BITPOS)
#define DOFOV100_TSTBIT(SCPTR, BITPOS) ORBIT3D_TSTBIT((SCPTR)->_doFov100,BITPOS)

#define ORBIT3D_SETBIT( I, BITPOS) I |= (1<<(BITPOS))
#define ORBIT3D_CLRBIT( I, BITPOS) I &= ~(1<<(BITPOS))
#define ORBIT3D_TSTBIT( I, BITPOS) (I & (1<<(BITPOS)))

#define COPYB0( B0, APTYPE, VOUT) \
  if ( APTYPE == Inadir) { \
  double cosel = cos( (M_PI/180.0) * B0[0]); \
  double sinel = sin( (M_PI/180.0) * B0[0]); \
  double cosaz = cos( (M_PI/180.0) * B0[1]); \
  double sinaz = sin( (M_PI/180.0) * B0[1]); \
    /* B0[0] is elevation (altitude) away from nadir, deg */ \
    /* B0[1] is azimuth clockwise from sun, deg */ \
    /* - nadir => X; sun az => Y; XxY => Z */ \
    VOUT[0] = cosel; VOUT[1] = sinel * cosaz; VOUT[2] = sinel * sinaz; \
  } else { CPYVEC( B0, VOUT); }

typedef struct APstruct {
  VEC ba0      /* virtual b/s aimpt */
  ,   ra0      /* roll b/s aimpt */
  ;
  struct APstruct *nextap, *prevap;
} AP;

typedef struct OTHERBODIESstr
{
  fortint _bodyId;
  char *_bodyName;
  VEC _scToBodyJ2k;
  VEC _scToBodySbf;
  VEC _bodyToScBf;
  double _angleBodyToBore;
  double _bodyQuat[4];
  struct OTHERBODIESstr *next;    /* linked list */
} OTHERBODIES;

typedef struct IMGFRMstruct {
  VEC _boreVec       /* intersection of boresight with body, abf coord's */
    , _normVec       /* unit normal to frame, similar to surface normal, abf */
    , _p5normVec     /* unit surface normal at boreVec, abf */
    , _boretoscVec   /* vector to s/c from boresight intersection, abf */
    , _boretosunVec  /* vector to sun from boresight intersection, abf */
    , _boresightJ2k  /* boresight vector (s/c to b/s intersect), J2000 */
    , _panelJ2k      /* s/c panel vector, J2000 */
    , _sunFromScJ2k  /* vector to sun from s/c, J2000 */
    , _earthFromScSC /* vector to earth from s/c, s/c coordinates */
    , _astFromScSC   /* vector to target from s/c, s/c coordinates */
    , _boreAimptVec
    , _boreScvecVec
    , _rollAimptVec
    , _rollScvecVec
    , _fov100Pts[MAXFOV100PTS]     /* points distributed in fov, abf */
    ;
  long _nFov100Pts                 /* count of fov points from SC struct */
     , _fov100Plates[MAXFOV100PTS] /* plate # hit by each fov point or -1 */
     , _nFov100Plates              /* count of fov points that hit a plate */
     ;
  double _orbitnum;               /* fractional orbit from et0 of orbit */
  double _incid, _emiss, _phase;  /* at boreVec, degrees */
  double *vec3;                   /* camera or NIS points' locations */
  int _ncampts                    /* number of camera points */
    , _nClosedPts                 /* from (SC).->_nClosedPts */
    , _nhits4                     /* # of first 3/4 verts that are on body */
    , _nisPosition                /* NIS scan mirror position number */
    , _nisScannum                 /* NIS mirror scan number since start */
    , _nisSpecnum                 /* NIS spectrum number since start */
    , _nisDuration                /* NIS spectrum integration time */
    , _nisDarkFollows             /* NIS dark frame follows this frame */
    , _boreAimptType 
    , _boreScvecType
    , _rollAimptType 
    , _rollScvecType
    , _isHidden
    ;
  double _et;                     /* time of frame */
  long _etDelta;                  /* time from previous frame in autogen seq */
  double _Overlap;                /* overlap from prev frame in autogen seq */
  int imgfrmAlloced;              /* if this structure created by malloc() */
  struct IMGFRMstruct *nextif     /* pointer to previous IMGFRM */
                    , *previf;    /* pointer to next IMGFRM, or to first */
  fortint _instrument;            /* instrument number */
  char _instrName[20];            /* instrument name (MSI, NIS, NIS2) */
  AGEN *_agen;                    /* pointer to autogen info */
  unsigned long _seqNum;          /* frame # in autogen sequence */
  double _scQuat[4];              /* NAIF SPICE Spacecraft Quaternion J2K->SC */
  double _abfQuat[4];             /* NAIF SPICE ABF Quaternion ABF/J2K */
  double _av[3];                  /* NAIF SPICE angular velocity */
  double _sclk;                   /* double precision spacecraft clock */
  fortint _scid;                  /* NAIF Spacecraft ID */
  OTHERBODIES *_otherBodies;
} IMGFRM ;


/* calculate miss distance squared of a target point relative to a boresight */
static void
orbit_missDist( VEC inScVec, VEC inUvBore, VEC inTargVec
              , double *outHypotVec, double *outHypotSqd
              , double *outBoreRange, double *outMissDistSqd) {
double lclHypotVec[VECSIZ];
double *hypotVec = outHypotVec ? outHypotVec : lclHypotVec;
  VMINUS2( inTargVec, inScVec, hypotVec);
  *outHypotSqd = VDOT(hypotVec, hypotVec);
  *outBoreRange = VDOT( inUvBore, hypotVec);
  *outMissDistSqd = *outHypotSqd - (*outBoreRange * *outBoreRange);
  return;
}

void vcross( VEC, VEC, VEC);
void vunit( VEC vin, VEC vout, double *vlength);
void vmxpb( double m, VEC x, VEC b, VEC y); /* m*x + b = y */
double vlen( VEC a);
void vxm( VEC, MTX, VEC);
void mxm( MTX, MTX, VEC);

void Rot2AbyB( char A, char B, VEC vecin, MTX Mout);
void solveSC( SC *, AP *);
void solveAxisRot( VEC, double, MTX);
void rotation3D(VEC, const double, MTX);

IMGFRM *loadImageFrame( SC *, double, IMGFRM *);

IMGFRM *gen_frames( ORBIT *);
IMGFRM *gen_nis0_frames( ORBIT *);
IMGFRM *gen_msi0_frames( ORBIT *);
void add_gen_frames( ORBIT *, IMGFRM *);

char *orbit_set_instrument( SC *, fortint);
void *copyInstrumentScToImgfrm( SC *, IMGFRM *);
void *copyInstrumentImgfrmToSc( IMGFRM *, SC *);

int orbit_inside( VEC pt, double *poly, int npoly, int ix, int iy);

#endif /* _ORBIT_3D_ */
