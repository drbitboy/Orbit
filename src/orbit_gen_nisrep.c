/* orbit_gen_nisrep.c - nis frame generation from NISEX/SR/DR repeat frags */

#include <math.h>
#include <stdio.h>
#include <malloc.h>

#include "orbit3d.h"
#include "orbit_cas.h"
#include "orbit_spice_names.h"
#include "debug.h"
#include "local.h"

/******************************************************************************/
/* MACROS for a given NISSEQDEF *CAS called DEFFRAG (itself probably a macro) */

#define NUMSCANS   DEFFRAG->nisNumScans
#define NUMOBS     DEFFRAG->nisNumObs
#define SECPEROBS  DEFFRAG->nisSecPerObs
#define SECBTWOBS  DEFFRAG->nisSecBtwObs
#define SECBTWSCAN DEFFRAG->nisSecBtwScan
#define MIRRORSTEP DEFFRAG->nisStepMirror

#define SECPERSCAN ((NUMOBS*(SECPEROBS+SECBTWOBS)) + SECBTWSCAN - SECBTWOBS)

#define MAXTIMESEQ  /* time from start of first to start of last obs in seq */ \
  (  (NUMSCANS * SECPERSCAN) /* start obs 1/scan 1 to start obs 1/scan NS+1 */ \
   - (SECBTWSCAN+SECPEROBS) )             /* start_obs/scan 1/NS+1 to NO/NS */
                                       /* NO => nisNumObs; NS => nisNumScan */

#define DURPERSEQ  /* time from start of first to end of last obs in seq */ \
  ( (NUMSCANS * SECPERSCAN) - SECBTWSCAN )

/***************************************************************/
/* NISSU2 (SUPER SCAN) & NISSU1 (SUPER or LOW PHASE SUPER SCAN) */
int 
orbit_gen_nisSU( CAS *suFrag, void *cur_item)
{
IMGFRM *finalImgfrmList=0;
ORBIT *orbit;
double et0;
int casType = suFrag->_type;
fortint utclenm1 = UTCLEN - 1;
fortint doylenout;

CAS *startCAS = orbitgui_get1stCASFromCurItem( cur_item);
CAS *nisSeqDefFrag;
long iPpss, iNoss, iMirPosn;
long delMirPosn;
int i;
AGEN saveAgen;

  /* confirm suFrag->_type is NISSU2 or NISSU1
   * - set change in initial mirror positions per scan
   * - set number of super scans
   */
  switch( casType) {
  case CASTYPE_NISSU2:
    delMirPosn = (suFrag->nisSuStepDir==NISSUIforeDir) ? 2 : -2;
    iNoss = 1;
    break;
  case CASTYPE_NISSU1:
    delMirPosn = 1;
    iNoss = suFrag->nisSuNoss;
    break;

  default:
    fprintf( stderr, "***orbit_gen_nisSU:  Program Error; %s%s\n"
           , "Contact Programmer, Code WSNBATGH-gen_nisSU-"
           , WHOAMI( casType));
    return 0;
  }

  if ( !orbit_CAS_isEnabled( suFrag)) return 0;

  /* convert cur_item to scOrbit pointer */
  orbitgui_return_scorbit_ci( cur_item, &orbit);

  et0 = CASSTART(suFrag) + suFrag->nisSuRepDelay;        /* get starting time */

  /* find if there is a valid nis sequence definition ...  */

  nisSeqDefFrag = 
    orbit_CAS_getNISSeqDefByNum( startCAS, et0, suFrag->nisSuSeq);

  if ( !nisSeqDefFrag) {
    fprintf( stderr, "Can't find NIS Sequence Definition %ld\n"
                   , suFrag->nisSuSeq);
    fflush( stderr);
    return 0;
  }

#define agen orbit->_agen

#define GETARGS( MBR, FRAG, TYPMBR) \
  if ( !(agen.MBR = orbit_CAS_CASToArgs(FRAG))) \
    agen.MBR = strdup( "Error in orbit_CAS_CASToArgs()"); \
  else \
    agen.TYPMBR = FRAG->_type 

#define FREEARGS(A) if (agen.A) { free(agen.A); agen.A = 0; }

  /* save initial agen; init agen CAS args */

  saveAgen = agen;

  GETARGS( _fragArgs, suFrag, _fragType);
  GETARGS( _seqDefArgs[0], nisSeqDefFrag, _seqDefType[0]);
  i = 1;
  while ( i<12) { agen._seqDefArgs[i++] = (char *) 0; }

#ifdef DEFFRAG
#undef DEFFRAG
#endif
#define DEFFRAG nisSeqDefFrag

  /* translate nisSu/nisSeqDef structures to agen structure */

  agen._MaxOrbits = 0.0;
  agen._EtStart = et0;
  agen._typeAgen = AGEN_TYPE_TIMING;
  agen._DeltaStep = MIRRORSTEP;
  agen._TimePerStep = SECPEROBS > 0 ? SECPEROBS : 1;    /* ensure dT/dObs > 0 */
  agen._DelayPerStep = SECBTWOBS > 0 ? SECBTWOBS : 0;
  agen._DelayPerRow = SECBTWSCAN > 0 ? SECBTWSCAN : 0;
  if ( (agen._MaxTime=MAXTIMESEQ)<=0.0) agen._MaxTime = 0.5;  /* just in case */

  if ( NUMOBS != 1) {
    fprintf( stderr, "***orbit_gen_nisSU:  User Error; %s%s (%ld) %s\007\n"
           , "NIS Sequence definition for Super Scan has "
           , "# observations per scan"
           , NUMOBS
           , "not equal to 1");
  }

  /* set to narrow fov unless BOTH_OUT 
   * - actually, it's only narrow if it's SLIT and it's closed otherwise, 
   *   but this program has no point if the shutter is closed
   */
  agen._NISFOV= (suFrag->nisSuAperture==NISREPIbothoutAper) ? 2 : 1;

  for ( ; iNoss>0; --iNoss) {    /* outer loop:  iNoss=1/nisSuNoss for SU2/SU */
  IMGFRM *imgfrmNew;
  IMGFRM *lastNew;
  IMGFRM *lastExisting;

    agen._StartingStep = suFrag->nisSuMirrorPosn; /* start pos for inner loop */

    for( iPpss=suFrag->nisSuPpss; iPpss>0; --iPpss) {           /* inner loop */

      agen._EndingStep = agen._StartingStep + ((NUMOBS-1) * MIRRORSTEP);

      ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);

      imgfrmNew = gen_nis0_frames( orbit);  /* generate next image frame list */

      if ( imgfrmNew) {
        if ( finalImgfrmList) {           /* append new list to existing list */
          lastNew = imgfrmNew->previf;                /* - save ends of lists */
          lastExisting = finalImgfrmList->previf;
          lastExisting->nextif = imgfrmNew;     /* - fwd link existing to new */
          imgfrmNew->previf = lastExisting;    /* - back link new to existing */
          finalImgfrmList->previf = lastNew;   /* - back link existing to new */

        } else finalImgfrmList = imgfrmNew;   /* if no existing list, use new */
      }

      agen._StartingStep += delMirPosn;    /* get ready for starting position */

      /* get next start time */
      agen._EtStart += (DURPERSEQ+1);                /* for seq + sel_mir_pos */
      agen._EtStart += 1;                          /* for inner loop end_loop */

    } /* inner for iPpss loop */

    if ( suFrag->_type == CASTYPE_NISSU1) {/* prepare for next super scan ... */
      agen._EtStart += (suFrag->nisSuPpss/4) + 1;/* ... time to return mirror */
    }

  } /* outer for iNoss loop */

  /* convert agen structure to fragment */

  agen._typeAgen = AGEN_TYPE_FRAG;
  agen._EtStart = CASSTART(suFrag);
  ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);
  add_gen_frames( orbit, finalImgfrmList);

  FREEARGS( _fragArgs)
  FREEARGS( _seqDefArgs[0])

  agen = saveAgen;              /* restore agen */

  return finalImgfrmList ? 1 : 0;

} /* orbit_gen_nisSU( CAS *suFrag, void *cur_item) */

/*********************************************/
int 
orbit_gen_nisRep( CAS *repFrag, void *cur_item)
{
IMGFRM *finalImgfrmList=0;
ORBIT *orbit;
double et0;
int casType = repFrag->_type;
fortint utclenm1 = UTCLEN - 1;
fortint doylenout;

CAS *startCAS = orbitgui_get1stCASFromCurItem( cur_item);
CAS *nisSeqDefFrag[2];
long iSeq01, iRep;
int i;
AGEN saveAgen;

  /* confirm repFrag->_type is NIS execute, single repeat, or double repeat */

  switch( casType) {
  case CASTYPE_NISDR:
  case CASTYPE_NISSR:
  case CASTYPE_NISEX:
    break;

  default:
    fprintf( stderr, "***orbit_gen_nisRep:  Program Error; %s%s\n"
           , "Contact Programmer, Code WSNBATGH-gen_nisRep-"
           , WHOAMI( casType));
    return 0;
  }

  if ( !orbit_CAS_isEnabled( repFrag)) return 0;

  /* convert cur_item to scOrbit pointer */
  orbitgui_return_scorbit_ci( cur_item, &orbit);

  et0 = CASSTART(repFrag) + repFrag->nisRepDelay;        /* get starting time */

  /* find if there is a valid nis sequence definition ...  */

  switch ( casType) {
  case CASTYPE_NISDR:

    nisSeqDefFrag[1] = 
      orbit_CAS_getNISSeqDefByNum( startCAS, et0, repFrag->nisRepSeq[1]);

    if ( !nisSeqDefFrag[1]) {
      fprintf( stderr, "Can't find NIS Sequence Definition %ld\n"
                     , repFrag->nisRepSeq[1]);
      fflush( stderr);
      return 0;
    }

  case CASTYPE_NISEX:
  case CASTYPE_NISSR:
    nisSeqDefFrag[0] = 
      orbit_CAS_getNISSeqDefByNum( startCAS, et0, repFrag->nisRepSeq[0]);

    if ( !nisSeqDefFrag[0]) {
      fprintf( stderr, "Can't find NIS Sequence Definition %ld\n"
                     , repFrag->nisRepSeq[0]);
      fflush( stderr);
      return 0;
    }

    break;
  }

  /* save initial agen; init agen CAS args */

  saveAgen = agen;

  GETARGS( _fragArgs, repFrag, _fragType);
  GETARGS( _seqDefArgs[0], nisSeqDefFrag[0], _seqDefType[0]);
  if ( casType == CASTYPE_NISDR) {
    GETARGS( _seqDefArgs[1], nisSeqDefFrag[1], _seqDefType[1]);
    i = 2;
  } else i = 1;
  while ( i<12) { agen._seqDefArgs[i++] = (char *) 0; }

  iRep = iSeq01 = 0;

  agen._MaxOrbits = 0.0;
  agen._EtStart = et0;
  agen._typeAgen = AGEN_TYPE_TIMING;

  /* initialize mirror position & aperture.  Someday init Gain, too. */
  if isNISDR(*repFrag) {
    agen._NISFOV= (repFrag->nisRepApertureSetup==NISREPIbothoutAper) ? 2 : 1;
    agen._StartingStep= repFrag->nisRepMirrorPosnSetup;
  }

  do {
  IMGFRM *imgfrmNew;
  IMGFRM *lastNew;
  IMGFRM *lastExisting;

#ifdef DEFFRAG
#undef DEFFRAG
#endif
#define DEFFRAG nisSeqDefFrag[iSeq01]

    /* translate nisRep/nisSeqDef structures to agen structure */

    /* set to narrow fov unless BOTH_OUT 
     * - actually, it's only narrow if it's SLIT and it's closed otherwise, 
     *   but this program has no point if the shutter is closed
     * - only set if nisRepAperture[iSeq01] is not NOCHANGE
     */
    if ( repFrag->nisRepAperture[iSeq01] != NISREPInochangeAper) {
      agen._NISFOV=(repFrag->nisRepAperture[iSeq01]==NISREPIbothoutAper)? 2 : 1;
    }

    if ( repFrag->nisRepMirrorPosn[iSeq01] != ORBIT_NOCHANGEVAL) {
      agen._StartingStep = repFrag->nisRepMirrorPosn[iSeq01];
    }
    agen._EndingStep = agen._StartingStep + ((NUMOBS-1) * MIRRORSTEP);
    agen._DeltaStep = MIRRORSTEP;
    agen._TimePerStep = SECPEROBS > 0 ? SECPEROBS : 1;  /* ensure dT/dObs > 0 */
    agen._DelayPerStep = SECBTWOBS > 0 ? SECBTWOBS : 0;
    agen._DelayPerRow = SECBTWSCAN > 0 ? SECBTWSCAN : 0;

    if ( (agen._MaxTime=MAXTIMESEQ)<=0.0) agen._MaxTime = 0.5;/* just in case */

    ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);

    imgfrmNew = gen_nis0_frames( orbit);    /* generate next image frame list */

    if ( imgfrmNew) {
      if ( finalImgfrmList) {            /* append new list to existing list */
        lastNew = imgfrmNew->previf;                  /* - save ends of lists */
        lastExisting = finalImgfrmList->previf;
        lastExisting->nextif = imgfrmNew;       /* - fwd link existing to new */
        imgfrmNew->previf = lastExisting;      /* - back link new to existing */
        finalImgfrmList->previf = lastNew;     /* - back link existing to new */

      } else finalImgfrmList = imgfrmNew;     /* if no existing list, use new */
    }

    switch ( casType) { /* move to next iteration for single or double repeat */
    case CASTYPE_NISSR:                                      /* single repeat */
      agen._EtStart += repFrag->nisRepDel[iSeq01];        /* - increment time */
      iRep++;                                     /* - increment repeat count */
      break;
    case CASTYPE_NISDR:                                      /* double repeat */
      agen._EtStart += repFrag->nisRepDel[iSeq01];/* - increment time for seq */
      if ( iSeq01) iRep++;  /* - increment repeat count after second sequence */
      iSeq01 = 1 - iSeq01;                  /* - toggle sequence 0->1 or 1->0 */
      break;
    default: break;
    }
    
  } while ( casType != CASTYPE_NISEX && iRep < repFrag->nisRepIter);

  /* convert agen structure to fragment */

  agen._typeAgen = AGEN_TYPE_FRAG;
  agen._EtStart = CASSTART(repFrag);
  ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);

  add_gen_frames( orbit, finalImgfrmList);

  FREEARGS( _fragArgs)
  FREEARGS( _seqDefArgs[0])
  FREEARGS( _seqDefArgs[1])

  agen = saveAgen;              /* restore agen */

  return finalImgfrmList ? 1 : 0;

} /* orbit_gen_nisRep( CAS *repFrag, void *cur_item) */
