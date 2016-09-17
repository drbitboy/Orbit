/* orbitgui_framesout.h
 * include information for output control of frames files
 */
#ifndef _ORBITGUI_FRAMESOUT_H_
#define _ORBITGUI_FRAMESOUT_H_

enum {
        FRAMESOUT_BIT_TIME = 0
#define FRAMESOUT_LBL_TIME "Time"
#define FRAMESOUT_SFX_TIME "et, orbitnum, time"

      , FRAMESOUT_BIT_P5PHOTOM
#define FRAMESOUT_LBL_P5PHOTOM "Photom"
#define FRAMESOUT_SFX_P5PHOTOM "surf norm, i, e, alpha at p5"

      , FRAMESOUT_BIT_P5VEC
#define FRAMESOUT_LBL_P5VEC "P5Vec"
#define FRAMESOUT_SFX_P5VEC "p5 xyz,r,lat,lon"

      , FRAMESOUT_BIT_SCVEC
#define FRAMESOUT_LBL_SCVEC "SCVec"
#define FRAMESOUT_SFX_SCVEC "sc xyz,r,lat,lon"

      , FRAMESOUT_BIT_SUNVEC
#define FRAMESOUT_LBL_SUNVEC "SunVec"
#define FRAMESOUT_SFX_SUNVEC "sun xyz,r,lat,lon"

      , FRAMESOUT_BIT_BOREJ2K
#define FRAMESOUT_LBL_BOREJ2K "BoreVec"
#define FRAMESOUT_SFX_BOREJ2K "boreJ2k xyz,rRADEC"

      , FRAMESOUT_BIT_SC2SUNJ2K
#define FRAMESOUT_LBL_SC2SUNJ2K "SC>SunVec"
#define FRAMESOUT_SFX_SC2SUNJ2K "sctosunJ2k xyz,rRADEC"

      , FRAMESOUT_BIT_OFFSUN
#define FRAMESOUT_LBL_OFFSUN "OffSunAngs"
#define FRAMESOUT_SFX_OFFSUN_BORE "boreoffsunang"
#define FRAMESOUT_SFX_OFFSUN_PANEL "paneloffsunang"

      , FRAMESOUT_BIT_GENINFO
#define FRAMESOUT_LBL_GENINFO "GeneralInfo"
#define FRAMESOUT_SFX_GENINFO "Instrument/DarkFollows"

      , FRAMESOUT_BIT_NISINFO
#define FRAMESOUT_LBL_NISINFO "NisInfo" /* Posn, Scan#, Spec#, Duration */
#define FRAMESOUT_SFX_NISINFO "MirrorPosnScan#Spec#Duration"

      , FRAMESOUT_BIT_POINTING
#define FRAMESOUT_LBL_POINTING "Pointing" /* Pointing types: Aimpt, Virt Bore */
                                         /* Roll Aimpt, Roll ScVec */
#define FRAMESOUT_SFX_POINTING_TYPE "Aimpt/VBore/ExtRollRef/SCRollType"
#define FRAMESOUT_SFX_POINTING_AIMPT "AimptVec"
#define FRAMESOUT_SFX_POINTING_VBORE "VirtualBoresightVec"
#define FRAMESOUT_SFX_POINTING_ROLLAIMPT "ExternRollRefVec"
#define FRAMESOUT_SFX_POINTING_ROLLSCVEC "S/CRollVec"

      , FRAMESOUT_BIT_AGENINFO
#define FRAMESOUT_LBL_AGENINFO "Autogen"  /* Auto frame gen'n */
#define FRAMESOUT_SFX_AGENINFO "autogen seq frame #, ID" /* Auto frame gen'n */
#define FRAMESOUT_SFX_AGENINFO_RESULT "autogen delta t(s), overlap from prev frame"
#define FRAMESOUT_SFX_AGENINFO_START "autogen # frames gen'ed; autogen start et, utc"
#define FRAMESOUT_SFX_AGENINFO_DURTYP "autogen min duration -s, -orbits; autogen type"
#define FRAMESOUT_SFX_AGENINFO_OLAPSPEC "autogen overlap -min, -max & -type"
#define FRAMESOUT_SFX_AGENINFO_MSITIMESPEC "autogen time (s) per MSI frame incl exp & filt move"
#define FRAMESOUT_SFX_AGENINFO_NISTIMESPEC1 "autogen scan start-, end- & delta- step"
#define FRAMESOUT_SFX_AGENINFO_NISTIMESPEC2 "autogen integration s/step, delay/step, delay/scan"

      , FRAMESOUT_BIT_CAMPTVECS
#define FRAMESOUT_LBL_CAMPTVECS "CamVerts" /* Camera Vertices */
#define FRAMESOUT_SFX_CAMPTVECS_NCAMPTS "number of camera points to follow, # of 1st 3/4 on body"
#define FRAMESOUT_SFX_CAMPTVECS_PTFMT "campt xyz,r,lat,lon"

      , FRAMESOUT_BIT_EARTHVEC
#define FRAMESOUT_LBL_EARTHVEC "EarthVec"
#define FRAMESOUT_SFX_EARTHVEC "sc2earthSC xyz,rRA(YZ>X)DEC(ZX>Y), in Fanbeam"

      , FRAMESOUT_BIT_P5PLATE
#define FRAMESOUT_LBL_P5PLATE "P5 Plate #"
#define FRAMESOUT_SFX_P5PLATE "Contact Programmer, code WSNBATGH-FRAMESOUT-p5p"

      , FRAMESOUT_BIT_ALLINFO
#define FRAMESOUT_LBL_ALLINFO "ALL info"
#define FRAMESOUT_SFX_ALLINFO "Contact programmer, code WSNBATGH-FRAMESOUT-1"

      , FRAMESOUT_BIT_SCLKQUAT
/* #define FRAMESOUT_LBL_SCLKQUAT "SCLK; S/C & ABF quats" */
#define FRAMESOUT_LBL_SCLKQUAT "SCLK&quats"
#define FRAMESOUT_SFX_SCLKQUAT "SCLK, SPICE quaternion(a,x,y,z) (J2k=>S/C)"
#define FRAMESOUT_SFX_ABFQUAT "ABF SPICE quaternion(a,x,y,z) (J2k->ABF)"

        /* provide an alternative type of time:  
         * - add sclk
         * - no non-numeric text (& therefore no colon, ":") before 
         *   the trailing label that starts with a colon
         */
      , FRAMESOUT_BIT_TIME_ALT
#define FRAMESOUT_LBL_TIME_ALT "alt Time"
#define FRAMESOUT_SFX_TIME_ALT "alt et, sclk; UTC, MET->"

      , FRAMESOUT_BIT_TARGVECSBF
#define FRAMESOUT_LBL_TARGVECSBF "TARGVecSBF"
#define FRAMESOUT_SFX_TARGVECSBF "target XYZsbf,Xsbf=>R,RA(RotY),DEC(RotZ)"

      , FRAMESOUT_BIT_OTHERBODIES
#define FRAMESOUT_LBL_OTHERBODIES "Other Bodies"
#define FRAMESOUT_SFX_OTHERBODIES "otherBody"

      , FRAMESOUT_BIT_LAST /* last item for loops:  icount<FRAMESOUT_BIT_LAST */
 };

#define FRAMESOUT_FLDSPEROW 4    /* this must be an int factor of BITSPERLONG */
#define FRAMESOUT_BITSPERLONG 32     /* this must be a multiple of BITSPERROW */
#define FRAMESOUT_NUMROWS \
        ((FRAMESOUT_BIT_LAST + FRAMESOUT_FLDSPEROW - 1) / FRAMESOUT_FLDSPEROW)

#endif /* #ifndef _ORBITGUI_FRAMESOUT_H_ */

/* end orbitgui_framesout.h */
