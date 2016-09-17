/* frm2iep.c - input orbit .frames file, output photometric geometry for
 * each plate from a shape model
 *
 * Usage:
 *
 *   frm2iep [options] [-intype <inputType> [inputTypeOptions]] [< xyz.frames]
 *
 * Purpose:
 *
 *   Read "orbit" program frames file output (standard input or file), 
 *   calculate photometric parameters for each plate in a shape model 
 *   which is visible and in the field of view (FOV) for each frame 
 *   (observation).   Two outputs are possible:
 *   1)  flat file, one line per plate per frame with photometric parameters;
 *   2)  plate model file, single statistic summarized for each plate.
 *
 * Options:
 *
 *   -framesin <framesfn>  Input frames from file <framesfn> 
 *                         (default:  standard input, "" => none)
 *
 *   -outiep <fn>          Output one line for each plate in a FOV to flat file
 *   -outiep -             named <fn>; <fn> = '-' => standard output (default).
 *   -nooutiep             - if -nooutiep is specified, do not write flat file
 *                         ***N.B. Default output is standard output which may 
 *                                 conflict with plate model statistics.
 *
 *   -spud <viewfn>        Use spud model from file <viewfn>
 *                         ***N.B. -plates overrides -spud
 *                         ***N.B. ignore plate model error messages
 *
 *   -plates <platefn>     Use plate model from file <platefn>
 *
 *   -instats <instatfn>   input plate model statistics file 
 *                         from file <instatfn> - overrides statistics in
 *                         any plate model 
 *
 *   -outstats <outstatfn> output updated plate model statistics 
 *                         to file <outstatfn>
 *
 *   -biep <biepfn>        Output binary file containing 
 *                         1) a single long integer giving the number of 
 *                              plate/met combinations that follow
 *                         2) for each plate in a given MET's FOV
 *                              long    FOV kMET
 *                              long    plate number
 *                              double  incidence angle, degrees
 *                              double  emission angle, degrees
 *                              double  phase angle, degrees
 *                              double  range TO PLATE, km
 *
 *   -iep -vine -xgrs      Output flat file format (default=iep).
 *   -vmag -lcrv           -iep  => cpLLR i e alpha vertLL time
 *                         -vine => cpLLR i e alpha iV nV eV time
 *                         -xgrs => plate # cpLLR  i  e  alpha  time
 *                         -vmag => plate_area  plate_range i e alpha \
 *                                                           cpLL time plate#
 *                         -lcrv => plate_area  plate_range i e alpha \
 *                                            scLL solLL met time plate#
 *
 *                         Key:
 *                           cpLLR = plate center lat, lon, radius, ABF
 *                           cpLL = plate center lat, lon, ABF
 *                           scLL/sunLL = spacecraft/sun lat, lon, ABF
 *                           vertLL = plate vertices lats, lons, ABF
 *                           i,e,alpha = incidence, emission, phase angles, deg
 *                           iV,nV,eV = incid, norm, emiss vectors, ABF
 *
 *   -fov <fovType>        How to interpret whether a given plate is in the 
 *                         FOV defined by p5Vec & campts (vertices) in .frames 
 *                         file (default=winding).  Valid values for <fovType>:
 *
 *                         winding - project FOV vertices & plate center (cp) 
 *                                   into common (image) plane.  If the cross 
 *                                   products of adjacent cp-to-vertex vectors 
 *                                   all go in the same direction, the plate
 *                                   is in the FOV
 *                                   ***N.B. ASSUMES FOV SHAPE IS CONVEX
 *                         w2 - another form of winding:  if the cp is on the 
 *                              same side of all planes formed by two adjacent 
 *                              FOV vectors as the next FOV vectors, the
 *                              plate is in the FOV
 *                              ***N.B. ASSUMES FOV SHAPE IS CONVEX
 *                         cart4 - project FOV vertices & plate center (cp)
 *                                 into common image plane, assume first 3
 *                                 vertices are at (X,Y) = (1,0),(0,0),(0,1)
 *                                 in a 2D cartesian coord system, convert cp 
 *                                 to (X,Y)  If 0 < X < 1 && 0 < Y < 1, the
 *                                 plate is in the FOV
 *                         xgrs - If plate center is within 2.5 degrees of p5
 *                                vector (boresight), the plate is in the FOV
 *
 *   -stat <statistic>     Summarize <statistic> for each plate,
 *                         output new plate model with stats to standard output.
 *                         Plates not in any FOV will be given
 *                         a Null value.  Valid values for <statistic>:
 *                                     
 *                         FRMSTAT=    Description [Range]
 *
 *                         incid       incidence angle, degrees [0-90]
 *                         mu0         cos(incid) [0-1]
 *                         emiss       emission ange, degrees [0-90]
 *                         mu          cos(emiss) [0-1]
 *                         mu0mu       (mu0 x mu) [0-1]
 *                         phase       phase angle, degrees [0-180]
 *                         cosphase    cos(phase)+1  [0-2]
 *                         resolution  mu / (Pixel_size x Range), pxl/km [0-*]
 *                         coverage    # FOV containing this plate [1-*]
 *
 *                         MARK ROBINSON (MR) calcs:
 *
 *                         mrmorph     (PxlSize*Range)*(1/mu), m/pxl [0-*]
 *                         mralbedo    (PxlSize*Range)*(1/mu+1/mu0), m/pxl [0-*]
 *
 *                         Nixel database:
 *
 *                         obs         observation parameter from frames file
 *
 * ***N.B. -stat is overridden by -instats
 *
 * ***N.B. Pixel_size used in resolution calculations is average NEAR MSI 
 *         pixel dimension i.e. sqrt(161uRad/Pxl*95uRad/Pxl)
 *
 *   -max -sum -min  Selection criterion for per-plate statistics.  Has no 
 *                   effect if -stat option is not specified.
 *                   -max  save maximum value of statistic (e.g. incid)
 *                   -min  save minimum value of statistic (e.g. phase)
 *                   -sum  sum statistic (e.g. resolution, coverage, mu0mu)
 *
 * ***N.B. -max, -min & sum are overridden if either -instats or -outstats 
 *         are specified
 *
 *   -platesout <platesoutfn>  output plate model to file <platesoutfn>
 *                             (default is standard output; "" => none)
 *
 *   -platesoutstat <statistic>  output this statistic to plate model instead of
 *                               -stat <statistic>
 *
 *   -obsLowLim <value>  low limit of observational parameter - ignore values
 *                       less than this (defaults to 0; normally used 
 *                       for negative values; will be subtracted from 
 *                       any observational parameter in frames file)
 *
 *   -init           initialize plate model per-plate statistics to -1.0 before
 *                   reading frames file.  Has no effect if -stat option is
 *                   not specified.
 *
 *   -rigor          make special checks to ensure no artifacts from orthogonal
 *                   projection.  also, include any plate that is intersected 
 *                   by any corner point
 *
 *   -verbose        output progress if orbdb is used
 *
 ***********************************************************************
 * ***N.B. THE FOLLOWING OPTION(S) MUST COME AFTER ALL OTHER OPTIONS 
 *         LISTED ABOVE
 ***********************************************************************
 *
 *   -intype <intype>  [intype options]
 *
 *                   type of input frames file & related options
 *
 *                   <intype>  
 *
 *                   string to indicate type of input file from which
 *                   to get frames' info.  some possible entries are kMET,
 *                   MET, ET, UTC, SCLK.  see file toiep.c for all possibilies. 
 *                   or run "frm2iep -intype list[full]".
 *                   defaults to original orbit frames file using 
 *                   frm2iep_readnextOrig() function in this module.
 *
 *                   [intype options] 
 *
 *                   options relating to <intype>; will be 
 *                   interpreted by the appropriate *2iep_init routine
 *                   run "frm2iep -intype listfull" to see available options.
 */
/* CUT FROM HERE FOR FRM2IEP.HLP */

#define NEARPXLSIZ sqrt(161E-6 * 95E-6)  /* radians per pixel */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "debug.h"
#include "orbit_spice_names.h"
#include "orbit3d.h"
#include "orbit_stats.h"
#include "spudshap.h"
#include "orbdb.h"

#define MXFOVVTX 100
#define deg *M_PI/180.
#define r2d(a) (a*180.0/M_PI)
#if 0
#define deg *3.14159265358979323846/180.
#define r2d(a) (a*180.0/3.14159265358979323846)
#endif
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

/*************/
/* -<option> */

enum {
   OPT_MAX=0, OPT_MIN, OPT_SUM, OPT_INIT, OPT_IEP, OPT_VINE, OPT_XGRS, OPT_VMAG
 , OPT_LCRV, OPT_RIGOR, OPT_VERBOSE
 , OPT_NOOUTIEP, OPT_FOV, OPT_STAT, OPT_FRAMESIN, OPT_OUTIEP
 , OPT_BIEP
 , OPT_INTYPE
 , OPT_ORBDB
 , OPT_PLATES, OPT_SPUD
 , OPT_PLATESOUT
 , OPT_INSTATS, OPT_OUTSTATS, OPT_PLATEOUTSTAT
 , OPT_OBSLOWLIM
 , OPT_DEBUG
 , OPT_HELP
 , OPT_COUNT};

static OPTION opts[] = {
   "max", OPT_MAX, "select max of per-plate statistic"
 , "min", OPT_MIN, "select min of per-plate statistic"
 , "sum", OPT_SUM, "select sum of per-plate statistic"
 , "init", OPT_INIT, "init per-plate stat to -1"
 , "iep", OPT_IEP, "IEP output cpLLR i e a vertLL t"
 , "vine", OPT_VINE, "IEP output:  cpLLR i e a vertLL t"
 , "xgrs", OPT_XGRS, "IEP output:  cpLLR i e a iV nV eV t"
 , "vmag", OPT_VMAG, "IEP output:  pltA pltR i e a cpLL t plt#"
 , "lcrv", OPT_LCRV, "IEP output:  pltA pltR i e a scLL solLL met t plt#"
 , "rigor", OPT_RIGOR, "eliminate artifacts from othog proj"
 , "verbose", OPT_VERBOSE, "output progress if orbdb used"
 , "nooutiep", OPT_NOOUTIEP, "no iep output"
 , "fov", OPT_FOV, "-fov winding|w2|cart4|xgrs - how to define 'in FOV'"
 , "stat", OPT_STAT, "-stat <statistic> - see frm2iep.hlp"
 , "framesin", OPT_FRAMESIN, "-framesin <frames input filepath>"
 , "outiep", OPT_OUTIEP, "-output <IEP output filepath>"
 , "biep", OPT_BIEP, "-biep <Binary IEP output filepath>"
 , "intype", OPT_INTYPE
           , "-intype <intype> [opts], run frm2iep -intype listfull for more"
 , "orbdb", OPT_ORBDB, "-orbdb <ORBit DataBase output prefix>"
 , "plates", OPT_PLATES, "-plates <Plate model input filepath>"
 , "spud", OPT_SPUD, "-spud <SPUD model filepath>"
 , "platesout", OPT_PLATESOUT, "-platesout <Plate model output filepath>"
 , "instats", OPT_INSTATS, "-instats <plate model input stats filepath>"
 , "outstats", OPT_OUTSTATS, "-outstats <plate model output stats filepath"
 , "platesoutstat", OPT_PLATEOUTSTAT
                  , "-platesoutstat <stat> to plt model instead of -stat <stat>"
 , "plateoutstat", OPT_PLATEOUTSTAT, ""             /* backward compatibility */
 , "obsLowLim", OPT_OBSLOWLIM, "-obsLowLim <val> - ignore obs param < this"
 , "debug", OPT_DEBUG, "very verbose progress output"
 , "h", OPT_HELP, "output this list"
 , "help", OPT_HELP, "output this list"
 , (char *) 0, OPT_COUNT, ""
 };

/*****************/
/* FOV=<fovType> */

enum {
   FOV_WINDING=0
 , FOV_CART4
 , FOV_XGRS
 , FOV_WINDING2
 , FOV_COUNT
 };

static OPTION fovs[] = {
   "winding", FOV_WINDING, "treat FOV as convex"
 , "cart4", FOV_CART4, "treat FOV as rect"
 , "xgrs", FOV_XGRS, "treat FOV as circle with R=2.5 deg"
 , "w2", FOV_WINDING2, "treat FOV as frustum"
 , (char *) 0, FOV_COUNT, ""
 };

/****************************************************/
/* stat_Func:  summarize &/or accumulate statistics */

/* stat_Min - save smallest value e.g. Phase angle */

static void 
stat_Min( long statType, double *currentVal, double *bestVal, double *allVal) {
int iStatType;
  if ( currentVal[statType] < *bestVal || *bestVal == FRMNULLVAL) {
    *bestVal = currentVal[statType];
    for ( iStatType=0; iStatType < STAT_COUNT; ++iStatType) {
      *(allVal++) = currentVal[iStatType];
    }
  }
  return;
}

/* stat_Max - save largest value e.g. Mu0 */

static void 
stat_Max( long statType, double *currentVal, double *bestVal, double *allVal) {
int iStatType;
  if ( currentVal[statType] > *bestVal || *bestVal == FRMNULLVAL) {
    *bestVal = currentVal[statType];
    for ( iStatType=0; iStatType < STAT_COUNT; ++iStatType) {
      *(allVal++) = currentVal[iStatType];
    }
  }
  return;
}

/* stat_Sum - sum values e.g. coverage */

static void 
stat_Sum( long statType, double *currentVal, double *bestVal, double *allVal) {
  if ( *bestVal == FRMNULLVAL) *bestVal = 0.0;
  *bestVal += currentVal[statType];
  return;
}

/****************************************************/
/* convert vector to lat, lon, r */
static void
frm2iep_reclat( VEC v, VEC llr, int eastlon) { /* eastlon:  1=>east; -1=>west */
double big;
VEC lclVec;
double lclLen;

  big = max(fabs(v[0]),fabs(v[1]));
  big = max(big,fabs(v[2]));
  if ( big > 0.0) {
    big = 1.0 / big;
    VSCAL2( big, v, lclVec);
    llr[2] = big * (lclLen=VLEN( lclVec));                           /* range */
    llr[0] = r2d( asin( lclVec[2]/lclLen));                       /* latitude */
    llr[1] = (v[0] == 0.0 && v[1] == 0.0) ? 0.0 : atan2( v[1], v[0]); /* long */
    llr[1] = eastlon * r2d( llr[1]);         /* adjust long east/west degrees */
    if ( llr[1] < 0.0) llr[1] += 360.0;           /* keep long in range 0-360 */
  } else {
   llr[0] = llr[1] = llr[2] = 0.0;
  }
  return;
} /* frm2iep_reclat( VEC v, VEC llr, int eastlon) { */

/****************************************************/
/* start of macros & routine to read a single frame */

long readNextLineno;

/* #define TIMBIT 1L
/* #define SCBIT (TIMBIT<<1)
/* #define SUNBIT (SCBIT<<1)
/* #define P5BIT (SUNBIT<<1)
/* #define CPCOUNTBIT (P5BIT<<1)
/* #define CPSTARTBIT (CPCOUNTBIT<<1)
/* #define CPSTOPBIT (CPSTARTBIT<<1)
/* #define ALLBITS (TIMBIT|SCBIT|SUNBIT|P5BIT|CPCOUNTBIT|CPSTARTBIT|CPSTOPBIT)
/* #define OBSPARMBIT (CPSTOPBIT<<1)
/* #define INCOMPLETE (OBSPARMBIT<<1)
/* #define NOT3 (INCOMPLETE<<1)
/* #define NOT1 (NOT3<<1)
/* #define CAMPTS (NOT1<<1)
/* #define DOUBLELINE (CAMPTS<<1)
/* #define READERR (DOUBLELINE<<1)
/* 
/* #define TESTBIT(B) if (istate & B) return(DOUBLELINE | istate)
/* #define SETBIT(B) istate |= B
/* 
/* #define READVEC( C, V, B) \
/*   TESTBIT(B); \
/*   if ( sscanf( C, "%lf %lf %lf", V, V+1, V+2) != 3) return(NOT3 | istate); \
/*   SETBIT(B)
/* 
/* #define BADVAL -1e300
/**/

#include "toiep.h"

/* read next frame from frames file
 *  0 for all items read
 * -1 for end of file, nothing read
 * >0 read error - current istate | errorBIT
 * xyzBIT for error reading a line of type xyz e.g. a second line of that type
 */

long
frm2iep_readnextOrig( FILE *f, char *timch, VEC sc, VEC vsun, VEC p5
                    , long *fovcount, VEC campts[], char *metch
                    , double *obsParm, MTX cam2Abf, double *miscVals)
{
long istate;
char line[256], tmpch[256];
long ncampts;

  if ( !f) return -1;

  *metch = '\0';
  *obsParm = BADVAL;

  miscVals[0] = miscVals[1] = miscVals[2] = miscVals[3] = miscVals[4] = -999.0;

  for ( readNextLineno=ncampts=istate=0; fgets( line, 255, f); ){
  char *fust;
    readNextLineno++;
    if ( *line == '\n') {
      if ( istate == OBSPARMBIT) {
        fprintf( stderr, "Warning:  trailing observational parameter %s\n"
                       , "in previous frame");
        *obsParm = BADVAL;
        istate = 0;
      } else if ( istate != 0) return(INCOMPLETE | istate);
    } else {
      if ( (fust = strstr( line, ":obs parameter")) ) {
        if ( istate & OBSPARMBIT) {
          fprintf( stderr, "Warning:  double observational parameter\n");
        }
        if ( sscanf( line, "%lf", obsParm) != 1) {
          fprintf( stderr, "Warning:  error reading observational parameter\n");
          fflush( stderr);
        }
        SETBIT(OBSPARMBIT);

      } else if ( (fust = strstr( line, ":et, orbitnum")) ) {
        TESTBIT(TIMBIT);
        *fust = '\0';
        strcpy( timch, line);
        SETBIT(TIMBIT);
        readNextLineno = 0;
      } else if ( strstr( line, ":alt et") ) {
        *metch = '\0';                  /* look for met, but don't require it */
        sscanf( line, "%s %s", tmpch, metch);
      } else if ( strstr( line, ":p5 xyz") ) {
        READVEC( line, p5, P5BIT);
      } else if ( strstr( line, ":sc xyz") ) {
        READVEC( line, sc, SCBIT);
      } else if ( strstr( line, ":sun xyz") ) {
        READVEC( line, vsun, SUNBIT);
      } else if ( strstr( line, ":number of camera points to follow") ) {
        TESTBIT(CPCOUNTBIT);
        if ( sscanf( line, "%ld", fovcount) != 1) return(NOT1 | istate);
        if ( *fovcount < 3 || *fovcount > MXFOVVTX) return(CAMPTS | istate);
        SETBIT(CPCOUNTBIT);

      /* read generic campt line e.g. "X Y Z lat lon r :N campt xyz..." */
      /* ***N.B. ONLY AFTER # CAMPTS HAS BEEN READ */

      } else if ( (fust=strstr( line, " campt xyz")) && (istate&CPCOUNTBIT) ) {
      long fovvtxno = 0;
      long tentothei = 1;
      VEC lclvec;
        while ( fust > line && *fust == ' ') --fust;   /* back up over spaces */
        if ( isdigit( *fust)) {
          while ( fust > line && isdigit(*fust)) {     /* back up over digits */
            fovvtxno += tentothei * (*(fust--) - '0'); /* ... parsing campt # */
            tentothei *= 10;
          }
          if ( *fust == ':'                          /* fust should be on a : */
            && fust > line         /* fust shouldn't be the beginning of line */
            && fovvtxno < *fovcount         /* ignore once fovcount is reset */
             ) {
            if ( fovvtxno >= MXFOVVTX || fovvtxno < 0) return( CAMPTS | istate);
            if ( fovvtxno != ncampts)                    /* force single step */
              return( CAMPTS | istate);

            if ( fovvtxno == 0) {                 /* special for fov vertex 0 */
              if (istate & CPSTARTBIT)                     /* only start once */
                return(CAMPTS | istate);
              istate |= CPSTARTBIT;
            }

            READVEC( line, campts[fovvtxno], CPSTOPBIT);   /* read the vector */

            /* set fovcount if this latest vertex equals the first one */
            /* i.e. if polygon is complete */
            if ( fovvtxno
              && campts[fovvtxno][0] == campts[0][0]
              && campts[fovvtxno][1] == campts[0][1]
              && campts[fovvtxno][2] == campts[0][2] ) {
              *fovcount = ncampts;
              if ( *fovcount < 3) return(CAMPTS | istate);
            }
            if ( ++ncampts < *fovcount)    /* if this is not the last vertex, */
              istate &= ~CPSTOPBIT;                   /* reset stop condition */

          } /* *fust == ':' && fust > line && fovvtxno < *fovcount */
        } /* if isdigit( *fust) */
      } /* end of if strstr ... else if strstr ... */
    }
    if ( (istate&(~OBSPARMBIT)) == ALLBITS) {
    /* Camera to ABF reference frame rotation
     * - ABF X = camera boresight (into image)
     * - ABF Y = camera "up"
     * - ABF Z = camera "right"
     *   - 1st 2 campts assumed to be in ABF XZ plane, 0-to-1 => toward +Z
     */
    VEC inXZPlane;
      VMINUS2( p5, sc, cam2Abf+0);           /* into image:  P5 - S/C = ABF X */
      vhat( cam2Abf+0, cam2Abf+0);
      VMINUS2( campts+1, campts+0, cam2Abf+6);    /* ~right:  0->1 ~ +Z in XZ */
      vcrss( cam2Abf+6, cam2Abf+0, cam2Abf+3);         /* up:  ~Z x X = ABF Y */
      vhat( cam2Abf+3,  cam2Abf+3);
      vcrss( cam2Abf+0, cam2Abf+3, cam2Abf+6);       /* right:  X x Y = ABF Z */
      /* VMINUS2( campts+1, campts+0, inXZPlane);    /* ~right:  0->1 ~ +Z in XZ */
      /* vcrss( cam2Abf+2, cam2Abf+0, cam2Abf+1);         /* up:  ~Z x X = ABF Y */
      /* vhat( cam2Abf+1,  cam2Abf+1);
      /* vcrss( cam2Abf+0, cam2Abf+1, cam2Abf+2);       /* right:  X x Y = ABF Z */
      return 0;
    }
  }
  if ( feof(f)) return -1;
  return(READERR|istate) ;
}  /* frm2iep_readnextOrig( FILE *f, char *timch, VEC sc, VEC vsun, VEC p5 ...*/

int
frm2iep_main(int argc, char **argv) {
int savargc = argc;
char **savargv = argv;

long instr = -93001;    /* default to NEAR MSI */
long instrPar = 0;
metCONTENT mC;
long *pK = (long *) NULL;

void (* stat_Func)( long, double *, double *, double *);

SPUDR spudr;

char *readType = (char *) 0;                           /* -intype <inputType> */
TOIEPREADNEXT readnextFn = frm2iep_readnextOrig;
int fovType;                                                /* -fov <fovType> */
int outType;                            /* -iep, -vine, -xgrs, -vmag or -lcrv */
int rigor;                                                          /* -rigor */
int verbose;                                                      /* -verbose */
int debug;                                                          /* -debug */

int statType;                                             /* -stat <statType> */
int statInit;       /* whether to initialize the per-plate stats in the model */
int spudStatType;                                /* -platesoutstat <statType> */

static double cos2_5i;
double dist;

SPUDF staticSpudf;
#define spudf (*dynamicSpudf)
SPUDF *dynamicSpudf;                /* SPUDF pointer, may point @ staticSpudf */

#define nf dynamicSpudf->nface
#define nvtx dynamicSpudf->nv

SPUDV *spudv;

#define VR spudv->_VR
#define SR spudv->_SR
#define vstat spudv->_vstat
#define pstat spudv->_pstat
#define xyz spudv->_xyz
#define hidden spudv->_hidden
#define persp spudv->_persp

long success;
MTX m1, m2, m3;
MTX cam2Abf;
fortint axis3=3;
fortint axis1=1;
VEC sunFromAstAbf, scFromAstAbf, scToLastChop, lclvec, p5, campts[MXFOVVTX];
unsigned long camPlates[MXFOVVTX+1];
double dotProds[MXFOVVTX];
VEC fovEdgeCrossProds[MXFOVVTX];
double pxlsiz = NEARPXLSIZ;
char timch[256] = { "<none>" };
char metch[256] = { "<none>" };
long fovcount;

long i, j, k, ii, iii, ip, ipn;
double *cf, *cfn, *cfllr, *cf3, *cfn3, *cfllr3, sumlen;
long *platenum2face;
VEC vtx[MXFOVVTX];
static VEC nulvec =  { 0.0, 0.0, 0.0 };

long iv;
double *vlatlon;

VEC vwid, vhgt, uvwid, uvhgt, uvp5;
double wid, hgt;
double scal[MXFOVVTX];
double currentVal[STAT_COUNT];
double *allVal;
double obsParm;
double obsLowLim = 0.0;
double rangeToPlate;

FILE *fIEP;           /* IEP output file stream */
char *fnIEP;          /* optional IEP output filename:  getenv("FILENAMEIEP") */
FILE *fBIEP;          /* BIEP output file stream */
char *fnBIEP;         /* optional BIEP output filename */
long countBIEP;       /* number of BIEP items written */
FILE *fInFrames;      /* input frames file stream (from orbit) */
char *fnInFrames;     /* input frames filename (from orbit) */
FILE *fOutPlates;     /* output plate model file stream */
char *fnOutPlates;    /* output plate model filename */
FILE *fInStats;       /* input plate model statistics file stream */
char *fnInStats;      /* input plate model statistics filename */
FILE *fOutStats;      /* output plate model statistics file stream */
char *fnOutStats;     /* output plate model statistics filename */
char *envPtr;
char *fnShape;
char *fnActual;
char *fnNull = { "" };

char *fnDB;           /* output ORBDB filename or filename prefix */
ORBDB_FILES *fDB;

long iopt;

#define MAXMALLOC 100
void *savMalloc0[MAXMALLOC];
void **savMalloc = savMalloc0;

#define SAVMALLOC(A) (*(savMalloc++) = (void *) malloc(A))

#define DVEC( V, X, Y, Z) (V)[0]=(X);(V)[1]=(Y);(V)[2]=(Z)

#define FWRITE1(A) fwrite( &A, 1, sizeof A, fBIEP)

#define RTN \
  if ( fIEP && (fIEP != stdout)) fclose( fIEP); \
  if ( fBIEP) { rewind(fBIEP); FWRITE1(countBIEP); fclose( fBIEP); } \
  if ( fInFrames && (fInFrames != stdin)) fclose( fInFrames); \
  if ( fOutPlates && (fOutPlates != stdout)) fclose( fOutPlates); \
  if ( fDB)  orbdb_close( fDB); \
  while (savMalloc > savMalloc0) { \
    if ( *(--savMalloc)) free( *savMalloc); \
  } \
  return

#define ERRRTN RTN -1
#define OKRTN RTN 0

#define FPRINTFERRRTN(A) fprintf A; fflush(stderr); ERRRTN

  /* defaults */

  for ( savMalloc = savMalloc0+MAXMALLOC; savMalloc > savMalloc0; ) {
    *(--savMalloc) = (void *) NULL;
  }

  toIepGetReadnextFn( 0, (char **) 0);                    /* initialize toIep */

  fovType = FOV_WINDING;
  outType = OPT_IEP;
  rigor = 0;
  verbose = 0;
  debug = 0;
  statInit = 0;
  statType = STAT_COUNT;                           /* no per-plate statistics */
  spudStatType = STAT_COUNT;    /* no special statistics in output spud model */
  fInFrames = stdin;
  fnInFrames = (char *) NULL;
  fOutPlates = stdout;
  fnOutPlates = (char *) NULL;
  fIEP = stdout;
  fnIEP = (char *) NULL;
  fnBIEP = (char *) NULL;
  fnShape = (char *) NULL;
  fnInStats = (char *) NULL;
  fnOutStats = (char *) NULL;
  fnDB = (char *) NULL;
  stat_Func = stat_Max;
  allVal = (double *) NULL;

  /*************************************************/
  /* process command line arguments */

  for ( argv++, argc--; argc > 0; ++argv, --argc) {
    if ( **argv != '-') {
      FPRINTFERRRTN(( stderr, "***FRM2IEP Unknown option:  %s; Returning ...\n"
                    , *argv));
    }
    TYPECHK( iopt, opts, (*argv)+1)
    switch ( iopt) {
    case OPT_FOV:
      if ( --argc < 1) break;
      argv++;
      TYPECHK( fovType, fovs, *argv)
      break;

    case OPT_STAT:
      if ( --argc < 1) break;
      argv++;
      TYPECHK( statType, stats, *argv)
      break;

    case OPT_PLATEOUTSTAT:
      if ( --argc < 1) break;
      argv++;
      TYPECHK( spudStatType, stats, *argv)
      break;

    case OPT_RIGOR:
      rigor = 1;
      break;

    case OPT_HELP:
      fprintf( stderr, "USAGE:  %s [options]\nOPTIONS:\n", *savargv);
      frm2_outOptHelp( opts, stderr);
      fprintf( stderr, "FOVs:\n");
      frm2_outOptHelp( fovs, stderr);
      return 0;

    case OPT_DEBUG:            /* ***N.B. -debug drops through & sets verbose */
      debug = 1;

    case OPT_VERBOSE:
      verbose = 1;
      break;

    case OPT_INIT:
      statInit = 1;
      break;

    case OPT_MAX:
      stat_Func = stat_Max;
      break;
    case OPT_SUM:
      stat_Func = stat_Sum;
      break;
    case OPT_MIN:
      stat_Func = stat_Min;
      break;

    case OPT_IEP:
    case OPT_VINE:
    case OPT_XGRS:
    case OPT_VMAG:
    case OPT_LCRV:
      outType = iopt;
      break;

    case OPT_BIEP:                                 /* -biep <biepOutFilename> */
      if ( --argc < 1) break;
      argv++;
      fnBIEP = *argv;
      break;

    case OPT_FRAMESIN:                        /* -framesin <framesInFilename> */
      if ( --argc < 1) break;
      argv++;
      fnInFrames = *argv;
      if ( *fnInFrames == '-')                      /* test for "-framesin -" */
        if ( !fnInFrames[1]) fnInFrames = (char *) 0;
      break;

    case OPT_PLATESOUT:                     /* -platesout <platesOutFilename> */
      if ( --argc < 1) break;
      argv++;
      fnOutPlates = *argv;
      if ( *fnOutPlates == '-')                    /* test for "-platesout -" */
        if ( !fnOutPlates[1]) fnOutPlates = (char *) 0;
      break;

    case OPT_OUTIEP:                                 /* -outiep <iepFilename> */
      if ( --argc < 1) break;
      argv++;
      fnIEP = *argv;
      if ( *fnIEP == '-')                             /* test for "-outiep -" */
        if ( !fnIEP[1]) fnIEP = (char *) 0;
      break;

    case OPT_INTYPE:                                   /* -intype <inputType> */
      if ( --argc < 1) break;
      argv++;
      readType = *argv;

      /***************************************************************
       * - use *argv to retrieve pointer to  *2iep_readnext() function
       * - call *2iep_init()
       * - remaining arguments are for the *2iep_init function
       */
      if ( !(readnextFn=toIepGetReadnextFn(argc,argv)) ) {
        if ( !strcmp( *argv, "list") || !strcmp( *argv, "listfull")) {
          fprintf( stderr, "Done listing intype's, frm2iep exiting ...\n");
          exit(0);
        }
        fprintf( stderr
               , "No *2iep_readnext function found to match '-intype %s'\n"
               , *argv);
        fprintf( stderr
               , "Try 'frm2iep -intype list[full]' to see valid options\n");
        fprintf( stderr, "FRM2IEP Exiting ...\n");
        exit(1);
      }
      argc = 1;            /* ensure no more arguments processed in this loop */
      break;

    case OPT_ORBDB:                                  /* -outiep <iepFilename> */
      if ( --argc < 1) break;
      argv++;
      fnDB = *argv;
      if ( *fnDB == '-')                              /* test for "-outiep -" */
        if ( !fnDB[1]) fnDB = (char *) 0;
      break;

    case OPT_OUTSTATS:                        /* -outstats <outStatsFilename> */
      if ( --argc < 1) break;
      argv++;
      fnOutStats = *argv;
      break;

    case OPT_INSTATS:                          /* -instats <outStatsFilename> */
      if ( --argc < 1) break;
      argv++;
      fnInStats = *argv;
      break;

    case OPT_NOOUTIEP:
      fnIEP = fnNull;
      break;

    case OPT_OBSLOWLIM:
      if ( --argc < 1) break;
      if ( 1 != sscanf( *(++argv), "%lf", &obsLowLim)) {
        FPRINTFERRRTN(( stderr
                      , "***FRM2IEP unable to read obsLowLim parameter,%s\n"
                      , *argv));
      }
      break;

    case OPT_PLATES:
    case OPT_SPUD:
      if ( --argc < 1) break;
      argv++;
      fnShape = *argv;
      break;

    default:
      FPRINTFERRRTN(( stderr
                    , "***FRM2IEP Program error, contact programmer, %s%s\n"
                    , "code WSNBATGH-1-", *argv));
      break;

    } /* switch iopt */

    if ( argc < 1) {
      FPRINTFERRRTN(( stderr
        , "***FRM2IEP:  No following argument for %s; Returning...\n"
        , *argv));
    }

  } /* for argv++ ... */

  /* done interpreting command line argumments */
  /*********************************************/

  cos2_5i = 1.0 / cos( 2.5 deg);   /* 1.0/cos(2.5 degrees) */

  /* if DB filename pointer is non-null,
   *   if the string is non-null, open those files as the DB output files
   *   if the string is null, set fDB stream pointer to null
   */

  if ( fnDB) {
    if ( *fnDB) {
      fDB = orbdb_openForWrite( fnDB, (ORBDB_FILES *) NULL);
      if ( !fDB) {
        FPRINTFERRRTN(( stderr
          , "FRM2IEP:  Could not open ORBDB output files (%s); Returning...\n"
          , fnDB));
      }
    } else fDB = (ORBDB_FILES *) NULL;     /* name is NULL string (""), no DB */
  } else fDB = (ORBDB_FILES *) NULL;

 /* do the same for the iep output file, default to stdout */

  if ( fnIEP) {
    if ( *fnIEP) {
      fIEP = fopen( fnIEP, "w");
      if ( !fIEP) {
        FPRINTFERRRTN(( stderr
          , "FRM2IEP:  Could not open IEP output file (%s); Returning...\n"
          , fnIEP));
      }
    } else fIEP = (FILE *) NULL;   /* name is NULL string (""), no IEP output */
  } else fIEP = stdout; /* default to stdout */

  /* do the same for input frames filename pointer, default to stdin */

  if ( fnInFrames) {
    if ( *fnInFrames) {
      fInFrames = fopen( fnInFrames, "r");
      if ( !fInFrames) {
        FPRINTFERRRTN(( stderr
          , "FRM2IEP:  Could not open framesin output file (%s); Returning\n"
          , fnInFrames));
      }
    } else fInFrames = (FILE *) NULL; /* name is NULL string (""), null input */
  } else fInFrames = stdin; /* default to stdin */

  /* do the same for output plate model filename pointer, default to stdout */

  if ( fnOutPlates) {
    if ( *fnOutPlates) {
      fOutPlates = fopen( fnOutPlates, "w");
      if ( !fOutPlates) {
        FPRINTFERRRTN(( stderr
          , "FRM2IEP:  Could not open framesin output file (%s); Returning\n"
          , fnOutPlates));
      }
    } else fOutPlates = (FILE *) NULL;/* name is NULL string (""), null output*/
  } else fOutPlates = stdout; /* default to stdout */

  /* do the same for biep output file */

  if ( fnBIEP) {
    if ( *fnBIEP) {
      fBIEP = fopen( fnBIEP, "w");
      if ( !fBIEP) {
        FPRINTFERRRTN(( stderr
          , "FRM2IEP:  Could not open biep output file (%s); Returning\n"
          , fnOutPlates));
      } else {
        countBIEP = 0;
        FWRITE1( countBIEP);                /* write initial count, fix later */
      }
    } else fBIEP = (FILE *) NULL;     /* name is NULL string (""), null output*/
  } else fBIEP = (FILE *) NULL;                     /* default to null output */

  /* read PLATES or VIEW (plate or spud model file) */

  spudr.eastlon = -1;                                /* assume WEST longitude */

  /**********************************************/
  /* get spudv & dynamicSpudf
   *
   * - from spicespec (toIepGetReadnextFn() => *2iep_init() )
   */

  orbitgui_return_spudv_sc( (SC *) 0, &spudv);
  if ( spudv ) dynamicSpudf = spudv->_spudf;

  /* - allow command line arguments to override toIep */

  if  ( (!spudv) || fnShape ) {

    if ( !(dynamicSpudf = getplatByname( (SPUDR *) 0, fnShape, &fnActual))) {

      /* failed to read plate model from file getenv("PLATES") or plates.dat,
       * try spud model from getenv("VIEW") or view
       */
      dynamicSpudf = &staticSpudf;

      /* read in shape model, convert to facet & view models */

      spudr.nlatR = SPUDRNLAT;
      spudr.nlonR = SPUDRNLON;
      spudr.eastlon = getviewByname( spudr.Rmodel[0], &spudr.nlatR, &spudr.nlonR
                                   , fnShape, &fnActual);
      if ( spudr.eastlon == -1) {
        fprintf( stderr, "FRM2IEP:  Failed to load shape model\n");
        ERRRTN;
      }
      if ( !spudr.eastlon) spudr.eastlon = -1;

      rmod2face( &spudr, dynamicSpudf);

    } /* if !(dyn. = getplatByname) */

    spudv = newspudv( dynamicSpudf);

    toIep_SetSpudv( spudv);
  }

  /* optionally set & test number of plates in database */

  if ( fDB) {
  long dbNumPlates = orbdb_getNumPlates( fDB);
    if ( !dbNumPlates) {
      orbdb_setNumPlates( fDB, (long) nf);
    }
    dbNumPlates = orbdb_getNumPlates( fDB);
    if ( dbNumPlates != (long)nf) {
      FPRINTFERRRTN(( stderr
        , "FRM2IEP:  Number of plates mismatch w/ORBDB(%ld != %ld); Returning\n"
        , dbNumPlates, nf));
    }
  }

  /* open input statistics file */

  if ( fnInStats) {
    fInStats = fopen( fnInStats, "r");
    if ( !fInStats) {
      FPRINTFERRRTN(( stderr
        , "FRM2IEP:  Could not open output statistics file (%s); Returning\n"
        , fnInStats));
    } else {
    long lclNFace;
    double *plt, *all;
    int oldStatType = statType;

      statType=orbit_readStatFile( fInStats, &lclNFace, &allVal);
      fclose( fInStats);

      if ( -1 == statType) {
        FPRINTFERRRTN( ( stderr
          , "...\n ***FRM2IEP:  ERROR in input statistics file (%s), %s\n"
          , fnInStats, "Returning"));
      }

      if ( oldStatType < STAT_COUNT && oldStatType != statType) {
        fprintf( stderr
               , "FRM2IEP:  commanded statistic (%s) overridden by statistic "
               , stats[oldStatType]._name);
        fprintf( stderr, "%s from file %s\n", stats[statType]._name, fnInStats);
      }

      if ( lclNFace != nf) {
        FPRINTFERRRTN( ( stderr
          , "***FRM2IEP ERROR:  number of plates in shape/plate model & %s\n"
          , "statistics files do not match, Returning"));
      }

      /* overwrite plate model statistics with controlling statistic 
       * from statistics file
       */
      plt=dynamicSpudf->platecolor+nf;
      all = allVal + statType + (STAT_COUNT * nf);
      while ( lclNFace) {
        all -= STAT_COUNT;
        *(--plt) = *all;
        lclNFace--;
      }
    }
  } else fInStats = 0;

  /* if using old or new stats, set stat_Func based on statType */

  if ( fnOutStats || fnInStats) {
    if ( !strcmp(statsMinMax[statType]._name, "max")) {
      if ( stat_Func != stat_Max) {
        fprintf( stderr, "***N.B. FRM2IEP statistic (%s) test set to MAX\n"
               , stats[statType]._name);
        stat_Func = stat_Max;
      }
    } else if ( !strcmp(statsMinMax[statType]._name, "min")) {
      if ( stat_Func != stat_Min) {
        fprintf( stderr, "***N.B. FRM2IEP statistic (%s) test set to MIN\n"
               , stats[statType]._name);
        stat_Func = stat_Min;
      }
    } else {
      FPRINTFERRRTN( ( stderr
                     , "FRM2IEP Program Error:  contact programmer, %s\n"
                     , "code WSNBATGH-defaultTest"));
    }
  }

  /* allocate space for all statistics if not done in fnInStats above */

  if ( !allVal) {
  double *allVal2;
  long i;
  long n = nf * STAT_COUNT;
    allVal2 = allVal = (double *) SAVMALLOC( n * sizeof( double));
    for ( i=0; i<n; ++i) *(allVal2++) = FRMNULLVAL;
  }

  pK = (long *) SAVMALLOC( nf * sizeof(long));      /* longs => plate DB keys */

  hidden = SPUDV_SHADHID; /* eliminate hidden & shadowed grid points */

  if ( outType == OPT_XGRS) spudr.eastlon = 1;     /* EAST longitude for XGRS */

  /***********************************************/
  /* initialize plate model colors to null value 
   * and allocate space for all statistics
   */

  if ( statType != STAT_COUNT && statInit) {
    for ( ip=0; ip<nf; ++ip) {
      dynamicSpudf->platecolor[ip] = FRMNULLVAL;
    }
  }

  /**************************/
  /* calculate lat's & lons */

  vlatlon = (double *) SAVMALLOC( 2 * nvtx * sizeof( double));

  for ( iv=0; iv<nvtx; ++iv) {
  double vtx[3];
  double lenvtx;
    vtx[0] = (spudf.Rxyz+(3*iv))[0];
    vtx[1] = (spudf.Rxyz+(3*iv))[1];
    vtx[2] = (spudf.Rxyz+(3*iv))[2];
    lenvtx = VLEN( vtx);
    vlatlon[iv] = (lenvtx>0) ? r2d(asin( vtx[2] / lenvtx)) : 0.0;
    vlatlon[iv+nvtx] = (lenvtx > vtx[2]) 
                     ? (spudf.eastlon * r2d( atan2( vtx[1], vtx[0]))) : 0.0;
#   if 1
    vlatlon[iv] += 0.666;
    vlatlon[iv+nvtx] += 0.888;
#   endif
    /* if ( vlatlon[iv+nvtx] < 0.0) vlatlon[iv+nvtx] += 360.0; /**/
  }

  cf = (double *) SAVMALLOC( sizeof(double) * 6 * nf);
  cfllr = cf + (3 * nf);
  cfn = spudf.uvnorms;

  /* make array of faces' indices sorted by plate number */
  platenum2face = (long *) SAVMALLOC( sizeof(long) * nf);
  for ( ip=0; ip<nf; ++ip) platenum2face[spudf.platenum[ip]-1] = ip;

  /* average each facet's vertices to get center point, 
   * take cross product of two sides to get facet normal 
   */
  for ( ip=0; ip<nf; ++ip) {
    /* init pointers */
    cf3 = cf + (ip *3);
    cfn3 = cfn + (ip *3);
    cfllr3 = cfllr + (ip *3);
    sumlen = cf3[0] = cf3[1] = cf3[2] = 0.0;

    /* do the summing & averaging */
#define SUMXYZ(IV, I) \
    vtx[I][0] = (spudf.Rxyz + (3 * IV))[0]; \
    vtx[I][1] = (spudf.Rxyz + (3 * IV))[1]; \
    vtx[I][2] = (spudf.Rxyz + (3 * IV))[2]; \
    VADD2( vtx[I], cf3, cf3); \
    sumlen += VLEN(vtx[I])

    SUMXYZ( spudf.faceapices[ip], 0);
    SUMXYZ( spudf.oe[spudf.faceoeidx[ip]], 1);
    SUMXYZ( spudf.oe[spudf.faceoeidx[ip] + 1], 2);

    /* llr => lat, lon, radius */
    cfllr3[2] = sumlen / 3.0;
    sumlen = cfllr3[2] / VLEN(cf3);
    VSCAL( sumlen, cf3);
    cfllr3[0] = (cfllr3[2]>0) ? r2d(asin(cf3[2]/cfllr3[2])) : -999.0;
    cfllr3[1] =(cfllr3[2]==0.0) ? 0.0 : r2d(spudr.eastlon*atan2(cf3[1],cf3[0]));
#if 1
    cfllr3[0] += 0.222;
    cfllr3[1] += 0.444;
#endif
    if ( cfllr3[1] < 0.0) cfllr3[1] += 360.0;
  }

  /************************************************/
  /* loop through frames in .frames input,
   *   get sun, s/c & corner vectors for each frame
   */
  while ( !(success = readnextFn( fInFrames, timch
                    , scFromAstAbf
                    , sunFromAstAbf, p5, &fovcount, campts, metch, &obsParm
                    , cam2Abf, mC._miscVals))) {

    /* figure out rotation matrices for viewing from sun & from s/c, 
     * ignore roll since we are only using these to calculate 
     * hidden & shadowed gridpoints
     */

    /* - rotate sc to +X first ... */

    Rot2AbyB( 'x', 'z', scFromAstAbf, m1);
    vxm( scFromAstAbf, m1, lclvec);
    Rot2AbyB( 'x', 'y', lclvec, m2);
    mxm( m1, m2, m3);

    /* ... then rotate X/Y/Z to -Y/+X/Z ... */

    m2[0] = m2[2] = m2[4] = m2[5] = m2[6] = m2[7] = 0.0;
    m2[1] = m2[8] = 1.0;
    m2[3] = -1.0;
    mxm( m3, m2, m1);

    /* copy sc matrix to VR */

    for (i=k=0; i<4; i++) for (j=0; j<4; j++) 
      VR[i][j] = (j<3 && i<3) ? m1[k++] : 0.0;

    /* - rotate sun to +X first ... */

    Rot2AbyB( 'x', 'z', sunFromAstAbf, m1);
    vxm( sunFromAstAbf, m1, lclvec);
    Rot2AbyB( 'x', 'y', lclvec, m2);
    mxm( m1, m2, m3);

    /* ... then rotate X/Y/Z to -Y/+X/Z ... */

    m2[0] = m2[2] = m2[4] = m2[5] = m2[6] = m2[7] = 0.0;
    m2[1] = m2[8] = 1.0;
    m2[3] = -1.0;
    mxm( m3, m2, m1);

    /* copy sun matrix to SR */

    for (i=k=0; i<4; i++) for (j=0; j<4; j++) 
      SR[i][j] = (j<3 && i<3) ? m1[k++] : 0.0;

    /* do projections & hidden/shadowed grid calc */

    spudv->_range = VLEN( scFromAstAbf);
    spudview( spudv);

    /**************************************************************/
    /* from here, all is done in Abf coordinates, spudview call was 
     * only used to determine which gridpoints are visible
     */

    /* vector to projection plane 1 unit along boresight 
     * i.e. unit vector toward p5 point
     */
    VMINUS2( p5, scFromAstAbf, uvp5); vhat( uvp5, uvp5);

    /* macro to chop vectors from s/c off at that plane */
#   define CHOP2(VABF, VFROMSC, SCAL) \
    VMINUS2( VABF, scFromAstAbf, scToLastChop); \
    SCAL = VDOT(scToLastChop,uvp5); \
    if ( fovType == FOV_WINDING2) { \
      CPYVEC( scToLastChop, VFROMSC); \
    } else { \
      vmxpb( 1.0/SCAL, scToLastChop, nulvec, VFROMSC); \
    }

#   define CHOP1(V, SCAL) CHOP2(V,V, SCAL)

    if ( fovType == FOV_XGRS) {               /* XGRS does not require campts */
      if ( rigor) fovcount = 0; /* but does use boresight as campt fovcount+1 */
    } else {

      for ( i=0; i<fovcount; ++i) {         /* convert campts to sc-to-campts */
        CHOP1( campts[i], scal[i])

        /* prepare for special tests #1 & #3 */
        if ( rigor) spudf_intersect( dynamicSpudf, scFromAstAbf, campts[i]
                                   , &dist, camPlates+i);
      }

      switch ( fovType) {
      case FOV_CART4:
        /* unit vectors & lengths along width & height */
        VMINUS2( campts[1], campts[0], vwid); vunit( vwid, uvwid, &wid);
        VMINUS2( campts[2], campts[1], vhgt); vunit( vhgt, uvhgt, &hgt);
        break;
      case FOV_WINDING2:
        /* get cross product of adjacent fov vectors defining edge of fov,
         * take dot product of that cross product with next fov vector, 
         * reverse cross product if necessary so dot product of any 
         * s/c centered vector with fovEdgeCrossProds[i] will be positive 
         * if vector is on same side of fov edge as next fov vector
         */
        for ( i=0; i<fovcount; ++i) {
          vcross( campts[i], campts[(i+1)%fovcount], fovEdgeCrossProds[i]);
          if ( VDOT(fovEdgeCrossProds[i],campts[(i+2)%fovcount]) < 0.0) {
            VNEG(fovEdgeCrossProds[i]);
          }
        }
        break;
      default:
        break;
      }
    }

    /* prepare for special tests #1 & #3 */
    if ( rigor) spudf_intersect( dynamicSpudf, scFromAstAbf, uvp5
                               , &dist, camPlates+fovcount);
    mC._nVisPlates = 0;
    if ( 1 != sscanf( metch, "%lf", &mC._met) ) mC._met = -1.0;
    mC._instr = instr;
    mC._instrPar = instrPar;
    mC._mu0 = mC._mu = 0.0;
    mC._alpha = - VDOT(uvp5,sunFromAstAbf) / VLEN(sunFromAstAbf); /*bore phase*/

    /* for each plate number ... */
    for ( ipn=0; ipn<nf; ++ipn) {
    VEC sctocf, cornertocf;
    double hrz, vrt, incid, emiss, phase, sctocf_scal;

    VEC v0, v1, crossprod;
    double dot0, curprod;
    int isInside;
    unsigned long ipIntersected;

      /* ... get corresponding facet */
      ip = platenum2face[ipn];

      isInside = 0;                     /* assume plate center is outside FOV */

      /* special test to include faces which are projected as hidden
       *  but are hit by campts (fov vertices) or p5 (boresight)
       * - because assumption of orthogonal projection along bore may be bad
       * - see if this plate is intersected by campt or p5 
       */
      if ( rigor) {
        for ( i=0; i<=fovcount; ++i) if ( camPlates[i] == ip) {
          isInside = 1;
          break;
        }
      }

#if 0
      /* ... test if surrounding vertices are visible ... */
      if ( isInside || ( (vstat[spudf.faceapices[ip]]==hidden) &&
           (vstat[spudf.oe[spudf.faceoeidx[ip]]]==hidden) &&
           (vstat[spudf.oe[spudf.faceoeidx[ip] + 1]]==hidden) ) ) {
#endif

      if ( isInside || pstat[ip]) {           /* test if plate is visible ... */

        /* ... translate to vec from s/c, chop it at plane at end of uvp5 ... */

        cf3 = cf + (ip * 3); 
        CHOP2( cf3, sctocf, sctocf_scal)

        /***********************************************/
        /* ... TESTS:  IF CENTER OF FACE IS INSIDE FOV */


        if ( !isInside) switch( fovType) {

        case FOV_CART4:
          /* determine FOV cartesian coordinates of facet ctr
           *     ***N.B. Assumes rectangular FOV
           * ... convert facet center into X & Y of FOV
           * ... now it's in plane, get vector from top-left corner ... 
           */
          VMINUS2( sctocf, campts[0], cornertocf);
          /* ... get horizontal & vertical distances from left & top, resp.,
           *     compare them to 0 & to wid & height, resp. ... 
           */
          hrz = VDOT( cornertocf, uvwid);
          if ( hrz >= 0.0 && hrz <= wid) {
            vrt = VDOT( cornertocf, uvhgt);
            if ( vrt >= 0.0 && vrt <= hgt) {
              isInside = 1;
            }
          }
          break;

        case FOV_WINDING:
          /* get vectors from facet center to first 2 vertices of fov,
           * cross them with each other,
           * dot result with uvp5
           */
          VMINUS2( campts[0], sctocf, v0);
          VMINUS2( campts[1], sctocf, v1);
          vcrss( v0, v1, crossprod);
          dot0 = VDOT(crossprod, uvp5);
          /* now do the same with all the other vertices, check that the product
           * of the first & current dot products is always >= 0 
           * i.e that the sign of the last two dot pro
           */
          curprod = 1.0;
          for ( i=2; i<=fovcount && curprod > 0.0; ++i) {
            CPYVEC( v1, v0);
            VMINUS2(campts[i%fovcount],sctocf,v1);/* recycle 1st campt as last*/
            vcrss( v0, v1, crossprod);
            curprod = dot0 * VDOT(crossprod,uvp5);
          }
          if ( curprod >= 0.0) isInside = 1;
          break;

        case FOV_WINDING2:

           sctocf_scal = VDOT(sctocf,uvp5);
           vmxpb( 1.0/sctocf_scal, sctocf, nulvec, sctocf);
          /* dot product sctocf with fovEdgeCrossProds will be positive if
           * sctocf is on same side of plane of campts[i] & campts[i+1] as 
           * campts[i+2]
           */
          for ( i=0; i<fovcount; ++i) {
            if ( VDOT(sctocf,fovEdgeCrossProds[i]) < 0.0) break;
          }
          if ( i == fovcount) isInside = 1;
          break;

        case FOV_XGRS:
          /* is facet center within 2.5 degrees of boresight */
          if ( VDOT( sctocf, uvp5) < cos2_5i) isInside = 1;
          break;
        }

        /* ... END OF TESTS:  IF CENTER OF FACE IS INSIDE FOV */
        /******************************************************/

        /* special test # 2 to exclude occluded plate 
         * - because spudview does hidden gridpt check on orthogonal projection
         * - ensure center of face is first intersection of ray from s/c
         */
        if ( rigor && isInside) { 
          spudf_intersect( dynamicSpudf, scFromAstAbf, sctocf
                                                     , &dist, &ipIntersected);
          if ( ip != ipIntersected) isInside = 0;
        }

        /* special test #3 to include faces whose centers are not inside fov 
         * but are hit by campts (fov vertices) or p5 (boresight)
         * - because assumption of small plates wrt large fov may be bad
         * - see if this plate is intersected by campt or p5 
         * - this is repeat special test #1 above
         */
        if ( rigor && !isInside) {
          for ( i=0; i<=fovcount; ++i) if ( camPlates[i] == ip) {
            isInside = 1;
            break;
          }
        }

        if ( isInside) {

          cfn3 = cfn + (ip * 3);
          cfllr3 = cfllr + (ip * 3);

          /* macro to convert from 2 vectors to an angle in degrees 
           * ANG will be the angle, for a while it holds the cos (dot product) 
           * V1 & V2 are the unit vectors that make the angle
           * SGN is the sign to flip the cosine if necessary e.g.
           *   to flip VDOT(NORMAL, SpacecrafttoCenterface) 
           *     to be VDOT(NORMAL, CenterFaceToSpacecraft) for emission angle
           * STATTYPE is the index of the relevant stats member
           * - special case:  save the cosine of that angle for statType
           */
#         define ACOSDDOT( ANG, V1,V2, SGN, STATTYPE) \
          ANG = SGN VDOT( V1, V2) / sqrt(VDOT(V1,V1)*VDOT(V2,V2)); \
          if ( ANG >= 1.0) ANG=1.0; \
          else if (ANG <= -1.0) ANG = -1.0; \
          currentVal[STATTYPE] = ANG; \
          ANG = r2d( acos( ANG))

          ACOSDDOT( incid, sunFromAstAbf, cfn3, +, STAT_MU0);
          ACOSDDOT( emiss, sctocf, cfn3, -, STAT_MU);
          ACOSDDOT( phase, sctocf, sunFromAstAbf, -, STAT_COSPHASE); 

          rangeToPlate = VLEN(scToLastChop);

          if ( (statType != STAT_COUNT) || fDB) {
          double kmPerPxl = rangeToPlate * pxlsiz;
            currentVal[STAT_INCID] = incid;
            currentVal[STAT_EMISS] = emiss;
            currentVal[STAT_PHASE] = phase;

            if ( (obsParm < obsLowLim) && (obsParm != BADVAL) ) {
              fprintf( stderr
                     , "Observational parameter (%lf) %s (%ld), %s\n"
                     , obsParm, "ignored, below low limit", obsLowLim);
            }
            currentVal[STAT_OBSPARM] = ((obsParm==BADVAL)||(obsParm<obsLowLim))
                                       ? -1.0 
                                       : (obsParm-obsLowLim);

            currentVal[STAT_COSPHASE] += 1.0;             /* make range [0-2] */

            currentVal[STAT_MU0MU] =                              /* Mu0 x Mu */
              currentVal[STAT_MU0] * currentVal[STAT_MU];

            /* the next few lines depend on the last CHOP2 above being this:
             *    CHOP2( cf3, sctocf, sctocf_scal);
             * so the vector from the spacecraft the the current center face 
             * is in scToLastChop
             *
             * - Resolution - as pixels/km (bigger is better; scale for mu)
             * - Coverage - 1 is already in ->_currentVal
             */

            currentVal[STAT_RESOLUTION] = 
              currentVal[STAT_MU] / kmPerPxl;

            currentVal[STAT_COVERAGE] = 1.0;

            currentVal[STAT_MRMORPH] = (currentVal[STAT_MU] > 0.0) 
                                     ? (kmPerPxl / currentVal[STAT_MU])
                                     : 1e10;

            currentVal[STAT_MRALBEDO] = 
                (currentVal[STAT_MU] > 0.0 && currentVal[STAT_MU0] > 0.0) 
                ? (kmPerPxl * ( 1.0 / currentVal[STAT_MU]
                              + 1.0 / currentVal[STAT_MU0]))
                : 1e10;

            if ( (statType != STAT_COUNT) ) {
              stat_Func( statType, currentVal, dynamicSpudf->platecolor+ip
                       , allVal + (ip*STAT_COUNT));
            }

            /* load metCONTENT structure for database */

              mC._mu0 += currentVal[STAT_MU0];
              mC._mu += currentVal[STAT_MU];
              mC._resolution += currentVal[STAT_MRMORPH] * 1000;   /* m/pixel */
              pK[mC._nVisPlates++] = spudf.platenum[ip];
            /* } /**/

          } /* if statType != STAT_COUNT || fDB */

          if ( fIEP) {
            if ( outType == OPT_XGRS) {
              /* print out plt # */
              fprintf( fIEP, " %10ld", spudf.platenum[ip]);
            }

            /* print out plate center lat, lon, r, & photom angles */

            switch ( outType) {
            case OPT_VMAG:
            case OPT_LCRV:
              break;
            default:
              fprintf( fIEP, " %15lf %15lf %15lf %10lf %10lf %10lf "
              , cfllr3[0], cfllr3[1], cfllr3[2], incid, emiss, phase);
              break;
            }
          }

          /* don't put parens around A in this macro so it can handle 
           * inverted vector e.g. PRINTV( -sctocf) -> -sctocf[0], ...
           */
#        define PRINTV( A) fprintf( fIEP, " %15lf %15lf %15lf", A[0],A[1],A[2])

          if ( fIEP) {

            switch ( outType) {
            VEC tmpVec, v01, v02, v01xv02;
            double *v0, *v1, *v2;

            case OPT_IEP:
              /* add lat, lon for vertices around this corner */
              iv = spudf.faceapices[ip];
              fprintf( fIEP, " %10lf %10lf", vlatlon[iv], vlatlon[iv+nvtx]);
              iv = spudf.oe[spudf.faceoeidx[ip]];
              fprintf( fIEP, " %10lf %10lf", vlatlon[iv], vlatlon[iv+nvtx]);
              iv = spudf.oe[spudf.faceoeidx[ip] + 1];
              fprintf( fIEP, " %10lf %10lf", vlatlon[iv], vlatlon[iv+nvtx]);
              break;

            case OPT_VINE:
              /* print out incident vector (center-of-facet to spacecraft),
               * normal vector, & emission vector 
               */
              PRINTV( sunFromAstAbf);
              PRINTV( cfn3);
              PRINTV( -sctocf_scal*sctocf);
              break;

            case OPT_VMAG:
            case OPT_LCRV:
              /* print out plate area, range to plate,  
               * incidence, emission, & phase angles, 
               * & (VMAG):  lat,long of plate center
               * | (LCRV):  lat,long of sc & sun
               */
              v0 = spudf.Rxyz + (3*spudf.faceapices[ip]);
              v1 = spudf.Rxyz + (3*spudf.oe[spudf.faceoeidx[ip]]);
              v2 = spudf.Rxyz + (3*spudf.oe[spudf.faceoeidx[ip] + 1]);
              VMINUS2( v1, v0, v01);
              VMINUS2( v2, v0, v02);
              vcross( v01, v02, v01xv02); /* length is |v01|*|v02|*sin(1/0/2) */
              fprintf( fIEP, " %15.7lg %15.7lg"
                     , VLEN( v01xv02)/2, VLEN(sctocf)*sctocf_scal);
              tmpVec[0] = incid;
              tmpVec[1] = emiss;
              tmpVec[2] = phase;
              PRINTV( tmpVec);
              switch ( outType) {
              VEC scllr, sunllr;
              case OPT_VMAG:
                fprintf( fIEP, " %15lf %15lf", cfllr3[0], cfllr3[1]);
                break;
              case OPT_LCRV:
                frm2iep_reclat( scFromAstAbf, scllr, spudr.eastlon);
                frm2iep_reclat( sunFromAstAbf, sunllr, spudr.eastlon);
                fprintf( fIEP, " %15.7lg %15.7lg %15.7lg %15.7lg"
                             , scllr[0], scllr[1], sunllr[0], sunllr[1]);
                /* if metch is null string, copy first token of timch */
                if ( !*metch) {
                  sscanf( timch, "%s", metch);
                }
                fprintf( fIEP, " %s", metch);
                break;
              }
              break;

            default:
              break;
            }

            /* and the time string & plate number */

            fprintf( fIEP, " %s %ld\n", timch, spudf.platenum[ip]);
          } /* if fIEP */
 
          /* binary IEP file - for speed */

          if ( fBIEP) {
          double dmet;
          long kmet = (1==sscanf( metch, "%lf", &dmet)) ? (long) (dmet/1000.0)
                                                        : -1;
            if ( kmet > 0) {
#             define FWRITE1(A) fwrite( &A, 1, sizeof A, fBIEP)
              FWRITE1( kmet);
              FWRITE1( spudf.platenum[ip]);
              FWRITE1( incid);
              FWRITE1( emiss);
              FWRITE1( phase);
              FWRITE1( rangeToPlate );
              countBIEP++;
            } /* if kmet > 0 */
          } /* if fBIEP */
/*
#ifdef DEBUG
fflush( stdout);
#endif
DPR((stderr, VEC3OUT( "sunFromAstAbf:  ", sunFromAstAbf)));
DPR((stderr, VEC3OUT( "cfn3:  ", cfn3)));
DPR((stderr, VEC3OUT( "sctocf:  ", sctocf)));
DPR((stderr, VEC3OUT( "cf3:  ", cf3)));
DPR((stderr, VEC3OUT( "scFromAstAbf:  ", scFromAstAbf)));
DPR((stderr, VEC3OUT( "campts[0]:  ", campts[0])));
DPR((stderr, VEC3OUT( "campts[1]:  ", campts[1])));
DPR((stderr, VEC3OUT( "campts[2]:  ", campts[2])));
DPR((stderr, VEC3OUT( "p5:  ", p5)));
DPR((stderr, VEC3OUT( "uvp5:  ", uvp5)));
/**/
        }   /* if isInside */
      }     /* if isInside || pstat ... */
    }       /* for ipn ... */

    /* update database */

    if ( mC._nVisPlates && (mC._met != -1.0)) {
    int returnVal;
    static long icou;
      mC._mu0 /= mC._nVisPlates;
      mC._mu /= mC._nVisPlates;
      mC._resolution /= mC._nVisPlates;
      CPYVEC( scFromAstAbf, mC._scVec);
      CPYVEC( sunFromAstAbf, mC._sunVec);
      m2eul( cam2Abf, &axis3, &axis1, &axis3
           , mC._eulCam2Abf+2,  mC._eulCam2Abf+1,  mC._eulCam2Abf); 
      if ( (returnVal=orbdb_addMet(fDB,&mC,pK)) ) {
        fprintf( stderr, "FRM2IEP:  orbdb_addMet returned %d\n", returnVal);
      }
      ++icou;
      if ( verbose  && (debug || ((icou % 100) == 0)) ) {
        fprintf( stderr, ".");
        if ( (icou % 5000) == 0) {
          fprintf( stderr, ".%ld:   MET=%lf\n", icou, mC._met);
        } else if ( debug ) {
          if ( (icou%100) == 0) fprintf( stderr, "%ld:", icou);
          fprintf( stderr, "%.0lf", mC._met);
        }
      }
    }

  } /* while success = readnextFn ... */

  if ( success != -1) { 
    fprintf( stderr, "%sProblem reading FRAMES input file:  status = %08xx\n"
           , "FRM2IEP:  "
           , success);
    fprintf( stderr, "  Last successful time read was\n\n  %s\n\n", timch);
    fprintf( stderr, "  Problem probably occurred %s FRAME with that time\n"
                   , (success&TIMBIT) ? "in" : "after");
    fprintf( stderr,   "Last line read was %ld line(s) after that time line\n"
                   , readNextLineno);
    fprintf( stderr, "  Check input file for %s\n"
           , (success&INCOMPLETE)  ? "missing lines - are campts in file?" 
           : ((success&NOT3)       ? "missing 1 or more of 3 vector elements"
           : ((success&NOT1)       ? "missing number of fov points"
           : ((success&CAMPTS)     ? "Problem interpreting fov vertices"
           : ((success&DOUBLELINE) ? "duplicate line types within one FRAME"
           : ((success&READERR)    ? "cause of I/O read error:"
           : "??? Contact programmer, Code WSNBATGH-1"
           ))))));
    if ( success&READERR) perror( "");
  } else {

    if ( statType != STAT_COUNT) {
      /* save requested statistic if different from key stat 
       * - used to copy statistic file data to plate model
       */
      if ( spudStatType != STAT_COUNT && spudStatType != statType) {
      double *ptr = spudf.platecolor;
      double *all = allVal + spudStatType;
        for ( ip=0; ip<nf; ++ip) {
          *(ptr++) = *all;
          all += STAT_COUNT;
        }
      }
      if ( fOutPlates) {
        spudprint_plateBareToFile( &spudf, fOutPlates);  /* write plate model */
      }
      
    }

    /* write output statistics file */

    if ( fnOutStats) {
      fOutStats = fopen( fnOutStats, "w");
      if ( !fOutStats) {
        FPRINTFERRRTN(( stderr
          , "FRM2IEP:  Could not open output statistics file (%s); Returning\n"
          , fnOutStats));
      }

      orbit_writeStatFile( fOutStats, nf, statType, allVal);

    } else fOutStats = 0;

    fprintf( stderr, "FRM2IEP:  Normal completion; Returning ...\n");
  }
  OKRTN;
} /* frm2iep_main(int argc, char **argv) { */

#ifndef _NO_MAIN_

#define DUMYROUTINE( A, B) \
void *A () \
{ if ( *(B)) { fprintf(stderr,"Call to dummy routine:  %s\n",B); exit(1); } \
return 0; }

/* DUMYROUTINE( orbitgui_return_spudv_sc, "orbitgui_return_spudv_sc") */
/* DUMYROUTINE( orbitgui_return_boreroll, "orbitgui_return_boreroll") */

DUMYROUTINE( orbitgui_update_BoreRoll, "orbitgui_update_BoreRoll")

DUMYROUTINE( orbitgui_get1stCASFromCurItem, "")
DUMYROUTINE( orbit_CAS_ds40Vec, "")
DUMYROUTINE( orbit_CAS_ds56Vec, "")
DUMYROUTINE( orbit_CAS_TypeToName, "")
DUMYROUTINE( orbitgui_add_comment_sc, "")

int
main(int argc, char **argv) { return frm2iep_main(argc, argv); }
#endif /* #ifndef _NO_MAIN_ *

/**********************************************************/
/* LAST LINE OF FRM2IEP.C */
