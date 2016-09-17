#include <math.h>
#include <string.h>

#include "orbit3d.h"
#include "pointing.h"
#include "orbit_cas.h"

#include "debug.h"

#define radpd (M_PI / 180.0)

/**********************************************************************/
/* create list of generated nis frames for a duration */

IMGFRM *
gen_nis0_frames( ORBIT *inpOrbit) {
double etNext, etEnd;
double orbitStart, orbitEnd;
IMGFRM *imgfrm1, *imgfrmN, *oldif;
SC *lclsc = (SC *) inpOrbit->_sc;
POINTING *lclBore, *lclRoll;
long numSpectra = 0; /* spectra taken since first one */
long numScan = 0;    /* number of times scan mirror has reset */
double frac;
int i, ii;
char *nisName;

  lclsc->_nisStepAct = inpOrbit->_agen._StartingStep;

  if ( inpOrbit->_agen._NISFOV == 2) {
    nisName = orbit_set_instrument( lclsc, SC_NIS2);
  } else {
    nisName = orbit_set_instrument( lclsc, SC_NIS);
  }

  /* get bore and roll pointing structures for orbitgui.c */
  orbitgui_return_boreroll( lclsc, &lclBore, &lclRoll);

  /* solve for starting imgfrm i.e. at time = inpOrbit->_agen._EtStart */
  etNext = inpOrbit->_agen._EtStart;
  if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
    return( (IMGFRM *) 0);
  imgfrm1 = loadImageFrame( lclsc, etNext, (IMGFRM *) 0);

  if ( !imgfrm1) return( imgfrm1);

  /* start building links within this list */
  imgfrm1->previf = imgfrm1; /* *imgfrm1 is start of list, AND for now it is */
                              /*   also the end of the list */
  imgfrmN = imgfrm1;          /* *imgfrmN is last complete IMGFRM on list */

/* save nis frame info into imgfrm */
#define NISIMGFRMSET \
  copyInstrument_ScToImgfrm( lclsc, imgfrmN); \
  imgfrmN->_nisDuration = inpOrbit->_agen._TimePerStep; \
  imgfrmN->_nisSpecnum = numSpectra; \
  imgfrmN->_nisScannum = numScan; \
  imgfrmN->_nisDarkFollows = 0

  NISIMGFRMSET;

  /* set up finishing criteria:  max Time or max Orbits */
  etEnd = inpOrbit->_agen._EtStart + inpOrbit->_agen._MaxTime;

  orbitStart = imgfrm1->_orbitnum;
  orbitEnd = orbitStart + inpOrbit->_agen._MaxOrbits;

  /* loop until finished criteria (time or orbit) met */
  while ( ( (inpOrbit->_agen._MaxTime > 0.0 && etNext < etEnd) || 
            (inpOrbit->_agen._MaxTime <= 0.0)
          ) &&
          ( (inpOrbit->_agen._MaxOrbits > 0.0 && imgfrmN->_orbitnum<orbitEnd &&
             imgfrmN->_orbitnum != -999.0) ||
            (imgfrmN->_orbitnum == -999.0) ||
            (inpOrbit->_agen._MaxOrbits <= 0.0)
          )
        ) {

    /* - we have just finished a spectrum, increment time for it */
    etNext += inpOrbit->_agen._TimePerStep +inpOrbit->_agen._DelayPerStep;


    ++numSpectra;    /* - increment counter */

    /* - move scan mirror - test for end of row
     * - ***N.B. We might have gone past end of row if abs(DeltaStep) > 1
     * - ***N.B. DelayPerStep not used at end of scan, subtract it out
     *           since it was added in above
     */
#define ENDSCAN \
 lclsc->_nisStepAct = inpOrbit->_agen._StartingStep; \
 etNext += inpOrbit->_agen._DelayPerRow - inpOrbit->_agen._DelayPerStep; \
 numScan++

    if ( ( (lclsc->_nisStepAct <= inpOrbit->_agen._EndingStep) &&
           (inpOrbit->_agen._EndingStep <= inpOrbit->_agen._StartingStep) ) ||
         ( (lclsc->_nisStepAct >= inpOrbit->_agen._EndingStep) &&
           (inpOrbit->_agen._EndingStep >= inpOrbit->_agen._StartingStep) )
       ) {
      ENDSCAN;
    } else {

      /* - we didn't go past end of row, test for out of range */
      lclsc->_nisStepAct += inpOrbit->_agen._DeltaStep;
      if ( lclsc->_nisStepAct < 0 || lclsc->_nisStepAct > lclsc->_nisStepMax) {
        ENDSCAN;
      }
    }

    if ( etNext > etEnd) break;

    /* get new vectors */
    orbit_set_instrument( lclsc, lclsc->_instrument);

    if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
      return( imgfrm1);

    oldif = imgfrmN->nextif;
    imgfrmN->nextif = loadImageFrame( lclsc, etNext, oldif);

    if ( !imgfrmN->nextif) {
      freeImageFrame( oldif);
      return( imgfrm1);
    }

    /* maintain links in list */
    imgfrmN->nextif->previf = imgfrmN;
    imgfrmN = imgfrmN->nextif;
    imgfrm1->previf = imgfrmN;

    NISIMGFRMSET;

  } /* time or orbit loop */

  return( imgfrm1);
}

/**********************************************************************/
/* create list of generated nis frames for a single NIS sequence
 * inputs:  
 *  inpOrbit->_agen._EtStart       starting time, s past J2k
 *                 ._StartingStep  first mirror position
 *                 ._NISFOV        1 for narrow, 2 for wide
 *                 ._seqDef        NIS sequence definition to use
 *
 * - framesLeft is a pointer to the number of frames that were not taken
 *   due to an abnormal exit from this routine
 */
IMGFRM *
gen_nis1_frames( ORBIT *inpOrbit, IMGFRM *inpImgfrm, long *framesLeft) {
double etNext, etEnd;
double orbitStart, orbitEnd;
IMGFRM *imgfrm1, *imgfrmN, *saveImgfrm;
IMGFRM **nextImgfrmPtr;
SC *lclsc = (SC *) inpOrbit->_sc;
POINTING *lclBore, *lclRoll;
long numSpectra = 0;
long numScan = 0;                    /* number of times scan mirror has reset */
long iObs, iScans;
char *nisName;
CAS *seqDef = (CAS *) inpOrbit->_agen._seqDef;

  numSpectra = 0;              /* set number of frames successfully generated */

  lclsc->_nisStepAct = inpOrbit->_agen._StartingStep;

  if ( inpOrbit->_agen._NISFOV == 2) {
    nisName = orbit_set_instrument( lclsc, SC_NIS2);
  } else {
    nisName = orbit_set_instrument( lclsc, SC_NIS);
  }

  /* get bore and roll pointing structures for orbitgui.c */
  orbitgui_return_boreroll( lclsc, &lclBore, &lclRoll);

  /* solve for starting imgfrm i.e. at time = inpOrbit->_agen._EtStart */
  etNext = inpOrbit->_agen._EtStart;

  /* update agen._TimePerStep from ._seqDef so NISIMGFRMSET works */
  inpOrbit->_agen._TimePerStep = seqDef->nisSecPerObs;

  /* imgfrm1 is start of list, imgfrmN is new IMGRM, 
   * imgfrm1->previf is end of list
   */
  nextImgfrmPtr = &inpImgfrm;
  imgfrm1 = inpImgfrm;

  *framesLeft = seqDef->nisNumScans * seqDef->nisNumObs;

  for ( iScans=0; iScans<seqDef->nisNumScans; ++iScans) {
    for ( iObs=0; iObs<seqDef->nisNumObs; ++iObs) {
      orbit_set_instrument( lclsc, lclsc->_instrument);    /* get new vectors */
      if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
        return imgfrm1;

      /* saveImgfrm is pointer to next IMGFRM (null => alloc)  */
      saveImgfrm = *nextImgfrmPtr;
      if ( nextImgfrmPtr != &inpImgfrm)     /* if this is not the first frame */
        if ( saveImgfrm)                            /* & if new IMGFRM exists */
          if (saveImgfrm == imgfrm1)        /* & if it points back to imgfrm1 */
            saveImgfrm = (IMGFRM *) 0;      /* then force alloc of new IMGFRM */

      /* allocate (if saveImgfrm is null) & load IMGFRM */

      if ( !(imgfrmN=loadImageFrame(lclsc,etNext,saveImgfrm)) ) return imgfrm1;

      /* add imgfrmN to list that starts at imgfrm1 */

      if ( !imgfrm1) {                     /* imgfrm1 is null, start new list */
        imgfrm1 = imgfrmN;

      } else {                                         /* add imgfrmN to list */
        if ( !saveImgfrm) {           /* ... if imgfrmN did not already exist */
          imgfrm1->previf->nextif = imgfrmN;
          imgfrmN->previf = imgfrm1->previf;
          imgfrm1->previf = imgfrmN;
          imgfrmN->nextif = imgfrm1;
        }
      }
      if ( !imgfrm1->previf) imgfrm1->previf = imgfrmN;   /* fill null, leave */
      if ( !imgfrmN->nextif) imgfrmN->nextif = imgfrm1;      /* non-null ptrs */

      nextImgfrmPtr = &imgfrmN->nextif;                /* move to next imgfrm */

      NISIMGFRMSET;

      etNext += (seqDef->nisSecPerObs + seqDef->nisSecBtwObs);
      lclsc->_nisStepAct += seqDef->nisStepMirror;

      ++numSpectra;
      --*framesLeft;
    }
    lclsc->_nisStepAct = inpOrbit->_agen._StartingStep;
    etNext += (seqDef->nisSecBtwScan - seqDef->nisSecBtwObs);
    ++numScan;
  }
  return imgfrm1;
}
