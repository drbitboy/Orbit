#include <stdio.h>
#include <string.h>
#include <math.h>

#ifdef vms
char *malloc();
char *realloc();
#else
#include <malloc.h>
#endif
#include "debug.h"

#include "orbit_spice_names.h"
#include "orbit_cas.h"
#include "orbit_util.h"

/******************************************************************/
/* set timing & other parameters in children of CASTYPE_CASOPNAV */

void
orbit_CAS_opnavSetChildren( CAS *opnavCAS) {
long *slewDur = opnavCAS->opnavSlewDur;
long *startNthSlew = opnavCAS->opnavStartNthSlew;
long subType = opnavCAS->opnavSubType;
long *shootImDeltas;
double *shootImTypes;
long iImDel;
CAS **shoot = opnavCAS->opnavShoot;
CAS **msiSR = opnavCAS->opnavMsiSR;
CAS **ds40 = opnavCAS->opnavDs40;
CAS **ds56 = opnavCAS->opnavDs56;
CAS **navrelatt = opnavCAS->opnavNavRelAtt;

#define shootCount shootCountArr(0)
#define shootCountArr(I) shoot[I]->msiShootCount

/***************** SHOOTINC() set time wrt prev seq **************/

# define SHOOTINC3(SEQID,D,IMGTYP)                                 /* all 3 */ \
  shootImTypes[iImDel] = IMGTYP; \
  SHOOTINC2(SEQID,D)
    
# define SHOOTINC2(SEQID,D)                                 /* Seq # & time */ \
  shoot[0]->msiShootSeq[iImDel] = SEQID; \
  SHOOTINC(D)

# define SHOOTINC(D)                                                /* time */ \
  shootImDeltas[iImDel] = shootImDeltas[iImDel-1]; \
  shootImDeltas[iImDel++] += (D); \
  shootCount = iImDel

/***************** SHOOTSET() set time directly ******************/

# define SHOOTSET3(SEQID,D,IMGTYP)                                 /* all 3 */ \
  shootImTypes[iImDel] = IMGTYP; \
  SHOOTSET2(SEQID,D)

# define SHOOTSET2(SEQID,D)                                 /* Seq # & time */ \
  shoot[0]->msiShootSeq[iImDel] = SEQID; \
  SHOOTSET(D)

# define SHOOTSET(D)                                                /* time */ \
  shootImDeltas[iImDel++] = slewDur[0] + (D); \
  shootCount = iImDel

# define SETSTSD(CASPTR,ST,SD) SETST(CASPTR,ST); (CASPTR)->ds40SlewDuration = SD
# define SETST(CASPTR,ST) (CASPTR)->_delayStart = ST

# define BITVALSET(BITS,VAL,I) \
  BITS |= (((VAL)?1L:0L)<<(I))

# define BITVALSET3(BITS,V1,V2,V3,I000) \
  BITVALSET(BITS,V1,I000); \
  BITVALSET(BITS,V2,(I000)+1); \
  BITVALSET(BITS,V3,(I000)+2)

  /* GNC(DS40_POINT,... macro
   *   SYS = aimpt coord sys
   *   AX/AY/AZ = aimpt vector
   *   SC = aimpt shortcut
   *   BX/BY/BZ = virtual boresight vector
   *   TM = team, i.e. "NAV" (ignored)
   *   F = frag pointer
   *   ST = F->_delayStart
   */
# define GNC_DS40_POINT(SD,SYS,AX,AY,AZ,SC,BX,BY,BZ,TM,F) \
  (F)->ds40SlewDuration = (SD); \
  (F)->ds40AimptFrmType = ds40SysArray[SYS];   /* aimpt coord sys */ \
  (F)->ds40AimptSelect = ds40SysArray[SC];      /* aimpt shortcut */ \
  LOADVEC( AX,AY,AZ, (F)->ds40AimptVec); \
  LOADVEC( BX,BY,BZ, (F)->ds40VbVec)

  /* GNC(NADIR_POINT,... macro - emulate using GNC_NADIR_POINT
   */
# define GNC_NADIR_POINT(TM,SD,F) \
  GNC_DS40_POINT( SD, 4, 0.0,0.0,0.0, 0, 1.0,0.0,0.0, TM, F)

  /* MSI(SINGLE_REPEAT,...
   * SEQNUM = sequence number
   * DELT = delta time
   * ITER = # iterations
   * IMTYP = image type
   */
# define MSI_SINGLE_REPEAT( SEQNUM, DELT, ITER, IMTYP, F) \
  (F)->msiRepSeq[0] = (SEQNUM); \
  (F)->msiRepDel[0] = (DELT); \
  (F)->msiRepIter[0] = (ITER); \
  sscanf( IMTYP, "%lf", &(F)->msiRepImageTypeDbl) /* string IMTYP e.g "2.3"*/

  /* MSI(MSI_IMAGE_EX,... - emulate as one sequence execution in a SHOOT frag
   * OFFSET = time from start of SHOOT to this sequence's execution
   * ISHOOT = this sequence's number in SHOOT (zero-based)
   *          - used to reset shoot->_count, so do highest ISHOOT last
   */
# define MSI_MSI_IMAGE_EX(SEQNUM,OFFSET,IMTYP,ISHOOT,F) \
  (F)->msiShootCount = (ISHOOT) + 1; \
  (F)->msiShootSeq[ISHOOT] = (SEQNUM); \
  (F)->msiShootDel[ISHOOT] = (OFFSET); \
  sscanf( IMTYP, "%lf", (F)->msiShootImageTypeDbl+(ISHOOT))

  /* GNC(DS56_SCAN,... macro */
# define GNC_DS56_SCAN( \
   SCD,SYS     /* scan_duration, coord_sys */ \
  ,RVX,RVY,RVZ /* reverse */ \
  ,HX,HY,HZ    /* hold_place */ \
  ,RDX,RDY,RDZ /* rate_dur */ \
  ,RTX,RTY,RTZ /* rate */ \
  ,PDX,PDY,PDZ /* pause_dur */ \
  ,TM,REU,F)     /* team, re_use, fragment ptr */ \
  (F)->ds56Bits = 0; \
  (F)->ds56ScanDur = (SCD); \
  (F)->ds56FrmType = ds56SysArray[SYS]; \
  BITVALSET3((F)->ds56Bits,RVX,RVY,RVZ,DS56IxCHG); \
  BITVALSET3((F)->ds56Bits,HX,HY,HZ,DS56IxPAUSE); \
  LOADVEC(RDX,RDY,RDZ,(F)->ds56RatePauseDur[0]); \
  LOADVEC(RTX,RTY,RTZ,(F)->ds56RateVec); \
  LOADVEC(PDX,PDY,PDZ,(F)->ds56RatePauseDur[1]); \
  (F)->ds56ReUse = (REU) ? DS56IreUSE : DS56IreUSE_not

# define GNC_DS56_SCAN_212(F)                /* this one is used five times */ \
  GNC_DS56_SCAN \
  (212,4,1,0,1,1,0,1,0,82,64,0.0,0.030061,0.0298828,0,64,82,"NAV",0,F)

  if ( shoot[0] ) {
    shootImTypes = shoot[0]->msiShootImageTypeDbl;
    shootImDeltas = shoot[0]->msiShootDel;
    iImDel = 0;
  }

  switch (subType) {
  long sumSlewDur, lastIm, delE;

  case OPNAVTYPE_A:

    GNC_NADIR_POINT("NAV",slewDur[0],ds40[0]);

    GNC_DS56_SCAN(40,4,0,0,0,1,1,1,40,40,40,0.0,0.0003402,0.0005765,0,0,0
                 ,"NAV",0,ds56[0]);
    SETST( ds56[0], slewDur[0]);

    MSI_SINGLE_REPEAT(30,2,16,"2.5",msiSR[0]);
    SETST( msiSR[0], slewDur[0] + 4);

    SETST( navrelatt[0], slewDur[0] + 90);
    break;

  case OPNAVTYPE_B:

    ds40[0]->ds40AimptSelect = 0;
    SETSTSD( ds40[0], 0, slewDur[0]);

    SHOOTSET2(29,  0);
    SHOOTINC3(28, 15, shootImTypes[0]);
    SHOOTINC3(29, 12, 2.3);
    SHOOTINC3(29,  3, 2.3);

    SETST( navrelatt[0], slewDur[0] + 90);
    break;

  case OPNAVTYPE_BP:

    ds40[0]->ds40AimptSelect = 0;
    SETSTSD( ds40[0], 0, slewDur[0]);

    SHOOTSET2(29,  0);
    SHOOTINC3( 8, 15, shootImTypes[0]);
    SHOOTINC3( 9,  8, shootImTypes[0]);

    SETST( navrelatt[0], slewDur[0] + 90);
    break;

  case OPNAVTYPE_C:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, startNthSlew[0]+slewDur[1]);
    SHOOTINC2( 27, 64);
    SHOOTINC2( 27, 82);
    SHOOTINC2( 27, 64);

    SETSTSD( ds40[0], 0, slewDur[0]);
    SETSTSD( ds40[1], slewDur[0] + startNthSlew[0], slewDur[1]);

    GNC_DS56_SCAN_212(ds56[0]);
    SETST( ds56[0], slewDur[0] + startNthSlew[0] + slewDur[1]);

    SETST( navrelatt[0], slewDur[0] + startNthSlew[0] + slewDur[1] + 210 + 75);
    break;

  case OPNAVTYPE_CACRUISE:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, startNthSlew[0] + slewDur[1]);
    SHOOTINC2( 27, 64);
    SHOOTINC2( 27, 82);
    SHOOTINC2( 27, 64);

    SETSTSD( ds40[0], 0, slewDur[0]);
    SETSTSD( ds40[1], slewDur[0] + startNthSlew[0], slewDur[1]);

    GNC_DS56_SCAN_212(ds56[0]);
    SETST( ds56[0], slewDur[0] + startNthSlew[0] + slewDur[1]);

    GNC_NADIR_POINT("NAV",510,ds40[2]);
    SETST( ds40[2], ds56[0]->_delayStart + 210 + 120 + 510);

    GNC_DS56_SCAN(40,4,0,0,0,1,1,1,40,40,40,0.0,0.0003402,0.0005765,0,0,0
                 ,"NAV",0,ds56[1]);
    SETST( ds56[1], ds40[2]->_delayStart);

    MSI_SINGLE_REPEAT(30,2,16,"2.5",msiSR[0]);
    SETST( msiSR[0], ds56[2]->_delayStart + 4);

    SETST( navrelatt[0], msiSR[0]->_delayStart + 94);
    break;


    /********** has anyone noticed that _CE & _CT are identical? **********/

#   define CE_AND_CT(OPNAVTYPE_CECT) \
    \
  case OPNAVTYPE_CECT: \
    \
    SHOOTSET2( 27, 0); \
    SHOOTSET2( 27, startNthSlew[0] + slewDur[1]); \
    SHOOTINC2( 27, 64); \
    SHOOTINC2( 27, 82); \
    SHOOTINC2( 27, 64); \
    \
    SETSTSD( ds40[0], 0, slewDur[0]); \
    SETSTSD( ds40[1], slewDur[0] + startNthSlew[0], slewDur[1]); \
    \
    GNC_DS56_SCAN_212(ds56[0]); \
    SETST( ds56[0], slewDur[0] + startNthSlew[0] + slewDur[1]); \
    \
    SETST( navrelatt[0], ds56[0]->_delayStart + 210 + 75); \
    break;

  CE_AND_CT( OPNAVTYPE_CE);
  CE_AND_CT( OPNAVTYPE_CT);

  case OPNAVTYPE_D:

    SHOOTSET2( 27,   0);
    SHOOTSET2( 27,  64);
    SHOOTSET2( 27, 146);
    SHOOTSET2( 27, 210);

    SETSTSD( ds40[0], 0, slewDur[0]);

    GNC_DS56_SCAN_212(ds56[0]);
    SETST( ds56[0], slewDur[0]);

    SETST( navrelatt[0], slewDur[0] + 210 + 75);
    break;

  case OPNAVTYPE_E:
  case OPNAVTYPE_E2:
  case OPNAVTYPE_E3:
  case OPNAVTYPE_E4:

    /* all image type = shootImTypes[0] i.e. from CASOPNAV/user
     * ***N.B. _E-_E4 MUST be sequential
     * ***N.B. must increment sumSlewDur AFTER SETSTSD( ds40...
     * ***N.B. SHOOTSET3 increments iImDel
     * ***N.B. slewDur[0] is implicit in SHOOTSET3, so take it out 
     *         of sumSlewDur
     */
    sumSlewDur = 0;
    while ( iImDel <= (subType-OPNAVTYPE_E) ) {
    static long ds40MsiInc[4] = { 0, 20, 40, 60 };
    static long navRelAttInc[4] = { 90, 90, 100, 120 };
      delE = iImDel;
      SETSTSD( ds40[delE], sumSlewDur+ds40MsiInc[delE], slewDur[delE]);
      sumSlewDur += slewDur[delE];
      SHOOTSET3(27, sumSlewDur + ds40MsiInc[delE]-slewDur[0], shootImTypes[0]);
      if ( iImDel > (subType-OPNAVTYPE_E)) sumSlewDur += navRelAttInc[delE];
    }
    SETST( navrelatt[0], sumSlewDur);
    break;

  case OPNAVTYPE_F:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, 64);
    SHOOTSET2( 27, 128);
    SHOOTSET2( 27, 210);
    SHOOTSET2( 27, 274);
    SHOOTSET2( 27, 338);

    SETSTSD( ds40[0], 0, slewDur[0]);

    GNC_DS56_SCAN(340,4,1,0,1,1,0,1,0,82,128,0.0,0.030061,0.0298828,0,128,82
                 ,"NAV",0,ds56[0]);
    SETST( ds56[0], slewDur[0]);

    SETST( navrelatt[0], slewDur[0] + 340 + 75);
    break;

  case OPNAVTYPE_G:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, 82);
    SHOOTSET2( 27, 164);
    SHOOTSET2( 27, 228);
    SHOOTSET2( 27, 310);
    SHOOTSET2( 27, 392);

    SETSTSD( ds40[0], 0, slewDur[0]);

    GNC_DS56_SCAN(394,4,1,1,0,1,1,0,0,164,64,0.0,0.030061,-0.0298828,0,64,164
       ,"NAV",0,ds56[0]);
    SETST( ds56[0], slewDur[0]);

    SETST( navrelatt[0], slewDur[0] + 394 + 75);
    break;

  case OPNAVTYPE_H:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, 64);
    SHOOTSET2( 27, 128);
    SHOOTSET2( 27, 210);
    SHOOTSET2( 27, 274);
    SHOOTSET2( 27, 338);
    SHOOTSET2( 27, 420);
    SHOOTSET2( 27, 484);
    SHOOTSET2( 27, 548);

    SETSTSD( ds40[0], 0, slewDur[0]);

    GNC_DS56_SCAN(550,4,1,0,1,1,0,1,0,82,128,0.0,0.030061,0.0298828,0,128,82
                 ,"NAV",0,ds56[0]);
    SETST( ds56[0], slewDur[0]);

    SETST( navrelatt[0], slewDur[0] + 550 + 75);
    break;

  case OPNAVTYPE_I:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, 82);

    SETSTSD( ds40[0], 0, slewDur[0]);

    GNC_DS56_SCAN(84,4,1,1,0,1,1,0,0,82,0,0.0,0.030061,0.0,0,0,0
                 ,"NAV",0,ds56[0]);
    SETST( ds56[0], slewDur[0]);

    SETST( navrelatt[0], slewDur[0] + 84 + 75);
    break;

  case OPNAVTYPE_J:

    SHOOTSET2( 27, 0);
    SHOOTSET2( 27, 64);

    SETSTSD( ds40[0], 0, slewDur[0]);

    GNC_DS56_SCAN(66,4,1,1,0,1,1,0,0,0,64,0.0,0.0,-0.0298828,0,0,0
                 ,"NAV",0,ds56[0]);
    SETST( ds56[0], slewDur[0]);

    SETST( navrelatt[0], slewDur[0] + 66 + 75);
    break;

  case OPNAVTYPE_K:
  case OPNAVTYPE_DOUBLE_K:

    /* ***N.B. save lastIm for navrelatt[0] timing test below
     * ***N.B. shootImTypes for DOUBLE_K are done below
     */
    for ( lastIm=iImDel=0; iImDel < shootCount; ++iImDel) {
      shoot[0]->msiShootSeq[iImDel] = 27;
      shootImDeltas[iImDel] = (lastIm=opnavCAS->opnavImDeltas[iImDel])
                            + slewDur[0];
    }

    SETSTSD( ds40[0], 0, slewDur[0]);

    SETST( ds56[0], slewDur[0]);

    /***************************
     * single K breaks out here:
     */
    if ( subType == OPNAVTYPE_K) { 
      if ( (lastIm+60) < ds56[0]->_duration ) {
        SETST( navrelatt[0], slewDur[0] + ds56[0]->_duration + 75); 
      } else {
        SETST( navrelatt[0], slewDur[0] + lastIm + 60); 
      }
      break;
    }

    /***********
     * DOUBLE_K:
     ***********/

    sumSlewDur = slewDur[0] 
               + ds56[0]->_duration
               + opnavCAS->opnavMiscInt[0]                         /* Delta_K */
               + slewDur[1];

    /* ***N.B. save lastIm for navrelatt[0] timing test below
     * ***N.B. reset single _K
     */
    for ( lastIm=iImDel=0; iImDel < shootCountArr(1); ++iImDel) {
      shootImTypes[iImDel] = *shootImTypes;
      shoot[1]->msiShootImageTypeDbl[iImDel] = *shoot[1]->msiShootImageTypeDbl;
      shoot[1]->msiShootSeq[iImDel] = 27;
      shoot[1]->msiShootDel[iImDel] = (lastIm=opnavCAS->opnavImDeltas[iImDel])
                                    + sumSlewDur;
    }

    SETSTSD( ds40[1], sumSlewDur-slewDur[1], slewDur[1]);

    SETST( ds56[1], sumSlewDur);

    if ( (lastIm+60) < ds56[0]->_duration ) {
      SETST( navrelatt[0], sumSlewDur + ds56[1]->_duration + 75); 
    } else {
      SETST( navrelatt[0], sumSlewDur + lastIm + 60); 
    }
    break;

  default:
    break;
  } /* switch subType */
  return;
} /* orbit_CAS_opnavSetChildren( CAS *opnavCAS) { */

/**********************************************************************/
/* allocate child fragments for an OPNAV, load default values */

CAS *
orbit_CAS_opnavNew( CAS *opnavCAS) {
static VEC nulvec = { 0.0, 0.0, 0.0 };
static VEC xvec = { 1.0, 0.0, 0.0 };
static long casNum = 0;
long iFrag, subType, iArgFrag, i;
SUBFRAGS *lclSubFrags, *lclSubFrags2;
CAS **casPtr, **casPtr2;
CAS *tmpCas;
CAS **shoot = opnavCAS->opnavShoot;
CAS **msiSR = opnavCAS->opnavMsiSR;
CAS **ds40 = opnavCAS->opnavDs40;
CAS **ds56 = opnavCAS->opnavDs56;
CAS **navrelatt = opnavCAS->opnavNavRelAtt;
double *shootImTypes;

# define NULLFRAGS(CASPTR,IMAX) \
  for ( i=0; i<IMAX;++i) { *(CASPTR+i) = (CAS *) NULL; }

  NULLFRAGS( shoot, OPNAVMAX_SHOOT)                 /* null out frag pointers */
  NULLFRAGS( msiSR, OPNAVMAX_MSISR)
  NULLFRAGS( ds40, OPNAVMAX_DS40)
  NULLFRAGS( ds56, OPNAVMAX_DS56)
  NULLFRAGS( navrelatt, OPNAVMAX_NAVRELATT)

  /* add FRAG's per opnavArgFragCount */

  FOROPNAVSUBFRAGS( opnavCAS, lclSubFrags, casPtr)
    if ( !(*casPtr=orbit_CAS_new( lclSubFrags->_casTypeIdx))) {
      FOROPNAVSUBFRAGS( opnavCAS, lclSubFrags2, casPtr2)
        if ( *casPtr2) free( *casPtr2);
      }}
      return (CAS *) NULL;
    } else {
      (*casPtr)->top = opnavCAS;
    }
  }}

  subType = opnavCAS->opnavSubType;

  /* create macros that match as closely as possible the CAS definitions
   * in near.satf so coding can use cut & paste & minimal editing
   *
   * INIT macro - initialize slewDur & prio
   *   TF = priority, TRUE/FALSE
   *   SD = first slew_duration
   *   shoot[0]->msiShootCount = opnavArgFragCount
   *   - will be set properly in SetChildren for all but OPNAV_K
   *   - set here specifically for OPNAV_K
   */
# define INIT(TF,SD) \
  opnavCAS->opnavPrio[0] = (TF); \
  opnavCAS->opnavSlewDur[0] = (SD); \

# define sd opnavCAS->opnavSlewDur
# define sd0 sd[0]
# define sd1 sd[1]
# define sd2 sd[2]
# define sd3 sd[3]

  /* initialize image types & number of image executes in shoot[0] */

# define INITIMTYP(I,V) \
  for (i=0; i<(I); ) shootImTypes[i++] = (V)

# define INITIMTYPALL(V) \
  shoot[0]->msiShootCount = opnavArgFragCount[subType][OPNAVARG_IM_TYPE]; \
  INITIMTYP( shoot[0]->msiShootCount, V)

  /* load default values into opnavCAS children per near.satf
   *
   * - typical defaults - saves doing it inside switch clause
   */
  opnavCAS->opnavPrio[0] = 0;        /* priority=FALSE:  common default value */
  sd0 = 510;           /* slew_dur:  common default value for most CASOPNAV's */
  if (  shoot[0]) {
    shootImTypes = shoot[0]->msiShootImageTypeDbl;
    INITIMTYPALL( 1.3);                                    /* common img type */
  }

  switch ( subType) {

  case OPNAVTYPE_A:          /* all else will be taken care of by SetChildren */
    break;

  case OPNAVTYPE_B:
  case OPNAVTYPE_BP:
    GNC_DS40_POINT(sd0,4,0,0,0,0,1,0,0,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_C:
  case OPNAVTYPE_CACRUISE:        /* _CACRUISE had default image types of 2.5 */
  case OPNAVTYPE_CE:                               /* _CE & _CT are identical */
  case OPNAVTYPE_CT:
    GNC_DS40_POINT(sd0,4,0,0,0,0,1,0,0,"NAV",ds40[0]);
    opnavCAS->opnavStartNthSlew[0] = 30;
    sd1 = 300;
    GNC_DS40_POINT(sd1,4,0,0,0,0,0.9996294,-0.0166889,0.0215065,"NAV",ds40[1]);

    if ( subType == OPNAVTYPE_CACRUISE) { INITIMTYPALL(2.5); }
    break;

  case OPNAVTYPE_D:
    GNC_DS40_POINT(sd0,4,0,0,0,0,0.9996294,-0.0166889,0.0215065,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_E:
  case OPNAVTYPE_E2:
  case OPNAVTYPE_E3:
  case OPNAVTYPE_E4:
    for ( iFrag=0; iFrag<opnavArgFragCount[subType][OPNAVARG_SLEWDUR]; ++iFrag){
      opnavCAS->opnavSlewDur[iFrag] = (!iFrag) ? 510 : 180;
      GNC_DS40_POINT(sd0,4,0,0,0,0,1,0,0,"NAV",ds40[iFrag]);
    }
    break;

  case OPNAVTYPE_F:
    GNC_DS40_POINT(sd0,4,0,0,0,0,0.9992117,-0.0333732,0.0214975,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_G:
    GNC_DS40_POINT(sd0,4,0,0,0,0,0.9989355,0.0166889,0.0430031,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_H:
    GNC_DS40_POINT(sd0,4,0,0,0,0,0.9985182,-0.0333732,0.0429851,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_I:
    GNC_DS40_POINT(sd0,4,0,0,0,0,0.99976864,0.0,0.02150952,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_J:
    GNC_DS40_POINT(sd0,4,0,0,0,0,0.9998607,0.0166889,0.0,"NAV",ds40[0]);
    break;

  case OPNAVTYPE_DOUBLE_K:
    opnavCAS->opnavMiscInt[0] = 10;                                /* Delta_K */

  case OPNAVTYPE_K:
    for ( i=0; i < ((subType==OPNAVTYPE_K)?1:2); ++i) {
    long j;
      opnavCAS->opnavSlewDur[i] = (!i) ? 510 : 510;
      GNC_DS40_POINT(sd[i],4,0,0,0,0,1,0,0,"NAV",ds40[i]);
      GNC_DS56_SCAN(400,4,0,1,1,0,0,1,0,58,58,0.0,0.0425,0.032974,0,58,58
                   ,"NAV",0,ds56[i]);
      shoot[i]->msiShootCount = 4;                                  /* FRAMES */
      for ( j=0; j<12; ++j) {
        opnavCAS->opnavImDeltas[j+(i*12)] = j * ((j<4) ? 58 : 0);
      }
    }
    break;

  } /* switch subType */

  orbit_CAS_opnavSetChildren( opnavCAS);

  /* prevent deleteCAS* routines from deleting children */

  opnavCAS->opnavLetMyPeopleGo = 0;

  return opnavCAS;
} /* orbit_CAS_opnavNew( CAS *opnavCAS) { */
