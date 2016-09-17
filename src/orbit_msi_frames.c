#include <math.h>
#include <string.h>

#include "orbit3d.h"
#include "pointing.h"

#include "debug.h"

#define radpd (M_PI / 180.0)
/**********************************************************************/
/* create list of generated msi frames based on a constant deltat T */

IMGFRM *
gen_msi0_frames( ORBIT *inpOrbit) {
double etNext, etEnd;
double orbitStart, orbitEnd;
IMGFRM *imgfrm1, *imgfrm2, *oldif;
SC *lclsc = (SC *) inpOrbit->_sc;
POINTING *lclBore, *lclRoll;
long numMSIFrames = 0;
long numScan = 0;    /* number of times scan mirror has reset */
double frac;
int i, ii;
char *msiName;

  msiName = orbit_set_instrument( lclsc, SC_MSI);

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
  imgfrm2 = imgfrm1;          /* *imgfrm2 is last complete IMGFRM on list */

  /* save msi frame info into imgfrm */
  copyInstrument_ScToImgfrm( lclsc, imgfrm2);

  /* set up finishing criteria:  max Time or max Orbits */
  etEnd = inpOrbit->_agen._EtStart + inpOrbit->_agen._MaxTime;

  orbitStart = imgfrm1->_orbitnum;
  orbitEnd = orbitStart + inpOrbit->_agen._MaxOrbits;

  /* loop until finished criteria (time or orbit) met */
  while ( ( (inpOrbit->_agen._MaxTime > 0.0 && etNext < etEnd) || 
            (inpOrbit->_agen._MaxTime <= 0.0)
          ) &&
          ( (inpOrbit->_agen._MaxOrbits > 0.0 && imgfrm2->_orbitnum<orbitEnd &&
             imgfrm2->_orbitnum != -999.0) ||
            (imgfrm2->_orbitnum == -999.0) ||
            (inpOrbit->_agen._MaxOrbits <= 0.0)
          )
        ) {

    /* - we have just finished a spectrum, increment time for it */
    etNext += inpOrbit->_agen._TimePerMSIFrm;

    if ( etNext > etEnd) break;

    /* - increment counter */
    ++numMSIFrames;

    /* DPR(( stderr, "delT, delt = %lf %d\n"
    , etNext - inpOrbit->_agen._EtStart, inpOrbit->_agen._DeltaStep)); /**/

    /* get new vectors */
    orbit_set_instrument( lclsc, lclsc->_instrument);

    if ( !pointing_solve( (char *) 0, &etNext, lclsc, lclBore, lclRoll))
      return( imgfrm1);

    oldif = imgfrm2->nextif;
    imgfrm2->nextif = loadImageFrame( lclsc, etNext, oldif);

    if ( !imgfrm2->nextif) {
      freeImageFrame( oldif);
      return( imgfrm1);
    }

    /* maintain links in list */
    imgfrm2->nextif->previf = imgfrm2;
    imgfrm2 = imgfrm2->nextif;
    imgfrm1->previf = imgfrm2;

    copyInstrument_ScToImgfrm( lclsc, imgfrm2);

  } /* time or orbit loop */

  return( imgfrm1);
}
