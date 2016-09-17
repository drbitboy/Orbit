/* orbit_gen_msirep.c - msi frame generation from MSISR/DR/DSR repeat frags */

#include <math.h>
#include <stdio.h>
#include <malloc.h>

#include "orbit3d.h"
#include "orbit_cas.h"
#include "orbit_spice_names.h"
#include "debug.h"
#include "local.h"

int 
orbit_gen_msiRep( CAS *repFrag, void *cur_item) /* ORBIT *, FLDMENU*) */
{
IMGFRM *imgfrm_list1;
IMGFRM *imgfrm_list2;
IMGFRM *imgfrm1;
IMGFRM *imgfrm2;
IMGFRM *finalImgfrmList;
IMGFRM *curImgfrm;

ORBIT *orbit;
double et1;
int casType = repFrag->_type;
fortint utclenm1 = UTCLEN - 1;
fortint doylenout;

CAS *startCAS = orbitgui_get1stCASFromCurItem( cur_item);
CAS *msiSeqDefFrag;

int iSeq;
AGEN saveAgen;

int iRep, nRep;      /* which repeat we are on, last repeat */

  if ( !orbit_CAS_isEnabled( repFrag)) return 0;

  /* confirm repFrag->_type is MSI repeat */

  nRep = 0;
  switch ( casType) {
  case CASTYPE_MSITR:
    nRep++;
  case CASTYPE_MSIDR:
  case CASTYPE_MSIDSR:
    nRep++;
  case CASTYPE_MSISR:
    nRep++;
    break;
  default:
    fprintf( stderr, "***orbit_gen_msiRep:  Program Error; %s%s\n"
           , "Contact Programmer, Code WSNBATGH-gen_msiRep-"
           , WHOAMI( casType));
    return 0;
  }

  /* convert cur_item to scOrbit pointer */
  orbitgui_return_scorbit_ci( cur_item, &orbit);

  /* add offset to CAS start time to get et1, start of 1st execution of seq 1 */

  et1 = CASSTART(repFrag) + repFrag->msiRepDelay[0];

  /* sequence 1 repeats - fill orbit->_agen */

#define agen orbit->_agen

#define GETARGS( MBR, FRAG, TYPMBR) \
  if ( !(agen.MBR = orbit_CAS_CASToArgs(FRAG))) \
    agen.MBR = strdup( "Error in orbit_CAS_CASToArgs()"); \
  else \
    agen.TYPMBR = FRAG->_type

#define FREEARGS(A) if (agen.A) { free(agen.A); agen.A = 0; }

  /* save initial AGEN; init agen CAS args */

  saveAgen = agen;

  GETARGS( _fragArgs, repFrag, _fragType);
  agen._fragType = repFrag->_type;
  for ( iSeq=0; iSeq<12; ++iSeq) { agen._seqDefArgs[iSeq] = (char *) 0; }

  agen._EtStart = et1;
  ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);

  /* The meanings of delta times for MSIDR & MSIDSR are different.
   * ***N.B. (MSITR is an extension of MSIDR)
   * The descriptions in near.satf imply the following:
   *
   * =========================MSIDR========================
   *
   * SEQ:       1        2              1        2
   *            ^        ^              ^        ^
   * DELTA:     |<-del1->|<----del2---->|<-del1->|
   * TIME:     et1       |              |
   *                 (et1+del1)         |
   *                             (et1+del1+del2)
   *
   * ========================MSIDSR========================
   *
   * SEQ:       1        1        1     2              2              2
   *            ^        ^        ^     ^              ^              ^
   * DELTA:     |<-del1->|<-del1->|...  |<----del2---->|<----del2---->|...
   * TIME:     et1       |              |              |
   *            |    (et1+del1)         |  (et1+del1+seq2start+del2)
   *            |                       |
   * DELTA:     |<-------seq2start----->|
   *                             (et1+del1+seq2start)
   *
   * ***N.B. For MSIDSR, it is the caller's responsibility to maintain 
   *         consistency with respect to msiRepDelay[0] (START_SEQ_1_IMAGING) 
   *         between the redundant quantities msiRepSeq2Start and 
   *         msiRepDelay[1] (START_SEQ_2_IMAGING) - this routine only uses
   *         msiRepDelay[0] & msiRepSeq2Start.
   *
   * code to support this interpretation:
   */
  agen._TimePerMSIFrm = repFrag->msiRepDel[0];              /* MSISR & MSIDSR */
  switch ( repFrag->_type) {
  case CASTYPE_MSITR:
    agen._TimePerMSIFrm += repFrag->msiRepDel[2];
  case CASTYPE_MSIDR:
    agen._TimePerMSIFrm += repFrag->msiRepDel[1];
  default:
    break;
  }

  /* to get exactly I repeats at delta time DEL, 
   * set minimum duration of AGEN to ( ((I-1)*DEL) ) 
   */

  agen._MaxTime = agen._TimePerMSIFrm * (repFrag->msiRepIter[0]-1);
  agen._MaxOrbits = 0.0;
  agen._typeAgen = AGEN_TYPE_TIMING;

  /* just in case ... */

  if ( agen._TimePerMSIFrm <= 0.0) agen._TimePerMSIFrm = 1.0;
  if ( agen._MaxTime <= 0.0) agen._MaxTime = 0.5;

/* macro to blend imgfrm1 & imgfrm2 lists together as finalImgfrmList */

#define ET1 imgfrm1->_et
#define ET2 imgfrm2->_et

#define BLENDIMGFRM \
  if ( !imgfrm_list1) { \
    if ( !imgfrm_list2) finalImgfrmList = (IMGFRM *) 0; \
    else finalImgfrmList = imgfrm_list2; \
  } else { \
    if ( !imgfrm_list2) finalImgfrmList = imgfrm_list1; \
    else { \
      /* blend both image frame lists if they are non-NULL */ \
      imgfrm1 = imgfrm_list1; \
      imgfrm2 = imgfrm_list2; \
      if ( ET1 <= ET2) { \
        curImgfrm = finalImgfrmList = imgfrm1;  \
        imgfrm1 = imgfrm1->nextif; \
      } else { \
        curImgfrm = finalImgfrmList = imgfrm2;  \
        imgfrm2 = imgfrm2->nextif; \
      } \
      \
      /* add a frame at a time sorted by ET */ \
      \
      while ( imgfrm1 || imgfrm2 ) { \
      IMGFRM **nextImgfrm; \
        \
        /* select imgfrm1 or imgfrm2 */ \
        if ( imgfrm1 && imgfrm2) { \
          nextImgfrm = (ET1 <= ET2) ? &imgfrm1 : &imgfrm2; \
        } else { \
          nextImgfrm = (!imgfrm2) ? &imgfrm1 : &imgfrm2; \
        } \
        \
        /* add to list, update previf pointer */ \
        curImgfrm->nextif = *nextImgfrm; \
        curImgfrm->nextif->previf = curImgfrm; \
        \
        /* step pointers */ \
        curImgfrm = curImgfrm->nextif; \
        *nextImgfrm = curImgfrm->nextif;  /* steps imgfrm1 or imgfrm2 */ \
      } \
      finalImgfrmList->previf = curImgfrm; /* start of list points to end */ \
    } \
  }

  finalImgfrmList =
  imgfrm_list1 = gen_msi0_frames( orbit);  /* actually generate the frames */

  /* find if there is a valid msi sequence definition ... */

  msiSeqDefFrag = 
    orbit_CAS_getMSISeqDefByNum( startCAS
                               , agen._EtStart
                               , repFrag->msiRepSeq[0]);

  /* ... if there is, generate additional imgfrm lists offset by interval ... */

  if ( msiSeqDefFrag) { 
    GETARGS( _seqDefArgs[0], msiSeqDefFrag, _seqDefType[0]);
  }

  if ( msiSeqDefFrag) for ( iSeq=1; iSeq < msiSeqDefFrag->msiSeqDefNumImages; 
                            ++iSeq) {
    agen._EtStart += msiSeqDefFrag->msiSeqDefInterval;
    ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);
    imgfrm_list2 = gen_msi0_frames( orbit);

    /* ... and blend them */

    imgfrm_list1 = finalImgfrmList;
    BLENDIMGFRM
  }

  /* for MSIDR, MSITR & MSIDSR generate the second set of frames using 
   * sequence 2 (& 3)
   */

  for ( iRep=1; iRep<nRep; ++iRep) {

    /* offset from 1st execution of seq iRep to that of seq iRep+1 */

    agen._EtStart = et1;
    switch ( repFrag->_type) {
    int lclRep;
    case CASTYPE_MSIDSR:
      agen._EtStart += repFrag->msiRepSeq2Start;
      agen._TimePerMSIFrm = repFrag->msiRepDel[1];
      break;
    case CASTYPE_MSITR:
    case CASTYPE_MSIDR:
      for (lclRep = 0; lclRep<iRep; ++lclRep) {
        agen._EtStart += repFrag->msiRepDel[lclRep];
      }
      break;
    }

    ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);

    /* code to support near.satf meaning described above */

    agen._TimePerMSIFrm = repFrag->msiRepDel[1];                    /* MSIDSR */

    switch ( repFrag->_type) {
    case CASTYPE_MSITR:
      agen._TimePerMSIFrm += repFrag->msiRepDel[2];
    case CASTYPE_MSIDR:
      agen._TimePerMSIFrm += repFrag->msiRepDel[0];
    default:
      break;
    }

    /* for MSIDR & MSITR, SUM(_del) * _iter are the same as for MSISR, 
     * for MSIDSR, they need to be recalculated
     */
    if isMSIDSR((*repFrag)) agen._MaxTime = 
      (agen._TimePerMSIFrm * ( repFrag->msiRepIter[1] - 1));

    if ( agen._TimePerMSIFrm <= 0.0) agen._TimePerMSIFrm = 1.0;
    if ( agen._MaxTime <= 0.0) agen._MaxTime = 0.5;

    imgfrm_list2 = gen_msi0_frames( orbit);  /* actually generate the frames */
    imgfrm_list1 = finalImgfrmList;
    BLENDIMGFRM

    /* handle multiple images in the sequence definition 
     * - see above for comments
     */

    msiSeqDefFrag = 
      orbit_CAS_getMSISeqDefByNum( startCAS
                                 , agen._EtStart
                                 , repFrag->msiRepSeq[iRep]);

    if ( msiSeqDefFrag) { 
      GETARGS(_seqDefArgs[iRep], msiSeqDefFrag, _seqDefType[iRep]); }

    if ( msiSeqDefFrag) for ( iSeq=1;
                              iSeq < msiSeqDefFrag->msiSeqDefNumImages; 
                              ++iSeq) {
      agen._EtStart += msiSeqDefFrag->msiSeqDefInterval;
      ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);
      imgfrm_list2 = gen_msi0_frames( orbit);
      imgfrm_list1 = finalImgfrmList;
      BLENDIMGFRM
    }
  } /* for iRep ... */

  /* fill in autogen structure as fragment */

  agen._typeAgen = AGEN_TYPE_FRAG;
  agen._EtStart = CASSTART(repFrag);
  ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);
  add_gen_frames( orbit, finalImgfrmList);

  FREEARGS( _fragArgs)
  FREEARGS( _seqDefArgs[0])
  FREEARGS( _seqDefArgs[1])
  FREEARGS( _seqDefArgs[2])

  agen = saveAgen;                                    /* restore initial AGEN */

  return finalImgfrmList ? 1 : 0;
}

int 
orbit_gen_msiShoot( CAS *shootFrag, void *cur_item) {
IMGFRM *imgfrm_list1;
IMGFRM *finalImgfrmList;

ORBIT *orbit;
double et1;
int casType = shootFrag->_type;
fortint utclenm1 = UTCLEN - 1;
fortint doylenout;

CAS *startCAS = orbitgui_get1stCASFromCurItem( cur_item);
CAS *msiSeqDefFrag;

int iSeq;
AGEN saveAgen;

  if ( !orbit_CAS_isEnabled( shootFrag)) return 0;

  /* confirm shootFrag->_type is MSI shoot */

  switch ( casType) {
  case CASTYPE_SHOOT:
    break;
  default:
    fprintf( stderr, "***orbit_gen_msiShoot:  Program Error; %s%s\n"
           , "Contact Programmer, Code WSNBATGH-gen_msiShoot-"
           , WHOAMI( casType));
    return 0;
  }

  /* convert cur_item to scOrbit pointer, ensure _delayStart is ignored */

  orbitgui_return_scorbit_ci( cur_item, &orbit);
  shootFrag->_delayStart = 0.0;

#ifdef agen
#undef agen
#endif
#define agen orbit->_agen         /* macro to simplify access to orbit->_agen */

  /* save initial AGEN; init agen CAS args */

  saveAgen = agen;

  for ( iSeq=0; iSeq<12; ++iSeq) { agen._seqDefArgs[iSeq] = (char *) 0; }
  GETARGS( _fragArgs, shootFrag, _fragType);

  et1 = CASSTART(shootFrag);     /* get starting time of SHOOT fragment */

  finalImgfrmList = (IMGFRM *) 0;            /* initialize final list to null */

  /* fixed settings in agen */

  agen._MaxOrbits = 0.0;
  agen._typeAgen = AGEN_TYPE_TIMING;

  /* fill orbit->_agen for each sequence */

  for ( iSeq=0; iSeq<shootFrag->msiShootCount && iSeq<12; ++iSeq) {

    /* calculate time for sequence, convert to UTC in agen */

    agen._EtStart = et1 + shootFrag->msiShootDel[iSeq];

    /* et1 = agen._EtStart;/* FUTURE:  UNCOMMENT IF im_delta_* ARE CUMULATIVE */

    ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);

    /* find if there is a valid msi sequence definition for this sequence ... */

    msiSeqDefFrag = 
      orbit_CAS_getMSISeqDefByNum( startCAS
                                 , agen._EtStart
                                 , shootFrag->msiShootSeq[iSeq]);

    /* ... if there is, use the interval for that sequence definition, 
     * otherwise just take one frame  ... 
     */
    if ( msiSeqDefFrag) {

      GETARGS( _seqDefArgs[iSeq], msiSeqDefFrag, _seqDefType[iSeq]);

      /* to get exactly I repeats at delta time DEL, 
       * set minimum duration of AGEN to ( ((I-1)*DEL) - 1 ) 
       */
      agen._TimePerMSIFrm = msiSeqDefFrag->msiSeqDefInterval;
      agen._MaxTime = agen._TimePerMSIFrm
                    * (msiSeqDefFrag->msiSeqDefNumImages - 1);
    } else {
      agen._TimePerMSIFrm = 1.0;   /* ONE FRAME */
      agen._MaxTime = 0.5;
    }

    /* just in case ... */

    if ( agen._TimePerMSIFrm <= 0.0) agen._TimePerMSIFrm = 1.0;
    if ( agen._MaxTime <= 0.0) agen._MaxTime = 0.5;

    imgfrm_list1 = gen_msi0_frames( orbit);  /* actually generate the frames */

    /* append new list onto end of final list */

    if ( finalImgfrmList && imgfrm_list1) {
    IMGFRM *imgfrm_last1 = imgfrm_list1->previf;       /* save last new frame */
      imgfrm_list1->previf = finalImgfrmList->previf;/* new first to old last */
      finalImgfrmList->previf->nextif = imgfrm_list1;/* old last to new first */
      finalImgfrmList->previf = imgfrm_last1;        /* old first to new last */

    } else if ( imgfrm_list1) {               /* old list is null, new is not */
      finalImgfrmList = imgfrm_list1;
    }                                     /* ***N.B. if both null, do nothing */

  } /* for iSeq */

  /* fill in autogen structure as fragment */

  agen._typeAgen = AGEN_TYPE_FRAG;
  agen._EtStart = CASSTART(shootFrag);
  ospice_et2doy( &agen._EtStart, &utclenm1, &doylenout, agen._utc);
  add_gen_frames( orbit, finalImgfrmList);

  /* free agen CAS args */

  FREEARGS( _fragArgs)
  for ( iSeq=0; iSeq<12; ++iSeq) { FREEARGS( _seqDefArgs[iSeq]); }

  agen = saveAgen;                                    /* restore initial AGEN */

  return finalImgfrmList ? 1 : 0;
}
