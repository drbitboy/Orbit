/* orbitgui_shading.c - gui for shading scaling */

#include <X11/Intrinsic.h>

#include <math.h>
#include <stdio.h>
#include <malloc.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

enum { SF_TENTH=0, SF_ONE, SF_TEN, SF_100, SF_1K, SF_LAST };
#define SF_MIN SF_TENTH
#define SF_MAX (SF_LAST-1)

enum { SS_ABOVE=0, SS_DATA, SS_OVERRIDE };
#define SS_MIN SS_ABOVE
enum { OF_TENTH=0, OF_ONE, OF_TEN, OF_100, OF_1K, OF_LAST };
#define OF_MIN OF_TENTH

#define BITI(I) (1L << (I))
#define BITTEST(LNG, I) (LNG & BITI(I))

static
char *shadeLabels[] = {
   "Min Shading Value..."
 , "Max Shading Value..."
 , " ... x Scaling Factor\0.1\0 1\0 10\0 100\0 1k\0"
 , "Shading Source\0MinMax^\0Data\0Override\0"
 , "Override:  0 to:\0.1\0 1\0 10\0 100\0 1k\0"
 , (char *) 0
 } ;

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */ 
int /* void /**/
orbitguiShadingScale_ok_CB( void *cur_item, FLDMENU *fldMenu)
{
double minShad, maxShad, scaleFactor;
long scaleFactorBits, sourceBits, overrideBits;
int resetToData=0;
int i;
FLDMENU *fm = fldMenu;

  /* get sliders' values, convert (long) 0-100 to (double) 0-10 */

  minShad = *(fm++)->fld_lng / 10.0;
  maxShad = *(fm++)->fld_lng / 10.0;

  scaleFactorBits = *fm->fld_lng; fm++;
  sourceBits = *fm->fld_lng; fm++;
  overrideBits = *fm->fld_lng; fm++;

  if ( maxShad < minShad) {   /* ensure max > min */
  double tmpD = maxShad;
    maxShad = minShad;
    minShad = tmpD;
  }

  /* find override bit if not none; use while so break leaves {} */

  switch ( sourceBits) {

  case (1L<<SS_DATA):                                /* get scaling from data */

    resetToData = 1;
    break;

  case (1l<<SS_OVERRIDE):            /* use pre-defined scale (0 to 1) * 10^N */

    for ( i=OF_MIN; !BITTEST(overrideBits,i) && i < OF_LAST; ++i) ;
    if ( i == OF_LAST) {
      fprintf( stderr , "orbitguiShadingScale_ok_CB:  %s\n"
                      , "Contact programmer, code WSNBATGH-0");
      return 0;
    } else {
      minShad = 0.0;
      maxShad = pow( 10.0, (double) (i - OF_ONE));
      scaleFactor = 1.0;
    }
    break;

  default:                                    /* use sliders & scaling factor */

    for ( i=SF_TENTH; !BITTEST(scaleFactorBits,i) && i<SF_LAST; ++i);
    if ( i == SF_LAST) {
      fprintf( stderr, "orbitguiShadingScale_ok_CB:  %s\n"
                      , "Contact programmer, code WSNBATGH-1");
      return 0;
    }

    scaleFactor = pow( 10.0, (double) (i - SF_ONE));
    minShad *= scaleFactor;
    maxShad *= scaleFactor;
    break;
  }

  /* update shading scaling, update display */
  orbitguiSetShading( cur_item, minShad, maxShad, resetToData, (int) 1);

  /* return 0 => menu won't close => don't free( fldMenu) */
  return 0;
}

/* Cancel callback - clean up fldMenu[] and the trailing stuff */
int /* void /**/
orbitguiShadingScale_cancel_CB( void *cur_item, FLDMENU *fldMenu)
{
  free( fldMenu);
  return(1);
}

/**********************************************************************/
/* orbitguiCreateShadingScale_dialog -- create dialog for setting scale range
 */
void
orbitguiCreateShadingScale_dialog( Widget buttonw, void *cur_item)
{
int           i, numflds;
char          **labels;
char          lclstr[20];
double        *lclgen;
int allocsize;
static int icount;
static char wildcard[] = { "*.SHADESCALE" };
long *lng0;
double lo, hi, mxhilo;    /* actual values & maximum */
double mlo, mhi, mmxhilo; /* mantissas of same */
long flo, fhi;            /* actual values scaled 0.1 to 10 */
long exponent;            /* alog10 of scale factor */

int resetToData, outOfRange;

FLDMENU *fldMenu, *fm;

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_lng longs all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  for ( numflds=1, labels=shadeLabels; *labels; numflds++, labels++) ;
  allocsize = numflds * (sizeof( long) + sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);

  lng0 = (long *) (fldMenu + numflds);

  for ( i=0; i<numflds; i++, fm++) {
    fm->lbl_txt = shadeLabels[i];
    fm->fld_lng = lng0+i;
  }

  fm = fldMenu;

  orbitguiGetShading( cur_item, &lo, &hi, &resetToData);/* get current values */

  /* scale to mantissae (mlo & mhi) to displayable range (0-10), set exponent */

  mxhilo = (hi > lo) ? hi : lo;                           /* only look at max */
  mmxhilo = log(mxhilo) / log(10.0);                   /* ... assume positive */

  if ( mmxhilo <= ((SF_MIN-SF_ONE)-1) ) { /* below min possible display value */
    outOfRange = 1;
    fhi = 1;                                                /* mantissa = 0.1 */
    flo = 0;
    exponent = SF_MIN;

  } else {

    if ( mmxhilo >= ((SF_MAX-SF_ONE)+1) ) { /* above max possible display val */
      fhi = 100;                                           /* mantissa = 10.0 */
      exponent = SF_MAX;

    } else {                                         /* in display-able range */
      outOfRange = 0;
      exponent = (int) floor(mmxhilo);            /* (mantissa) * 10^exponent */
      fhi = 0.5 + pow( 10.0, 1+mmxhilo-exponent);/* (1 to 100) => (0.1 to 10) */
      exponent += SF_ONE;               /* translate exponent to bit position */

      if ( fhi == 100 && exponent < SF_MAX) { /* if at top of displayed range */
        fhi = 10;                            /* move to middle of (log) range */
        exponent++;                                    /* and adjust exponent */
      }

      if ( exponent < SF_MIN) {                    /* if exponent too low ... */
        exponent++;                                      /* ... adjust it ... */
        fhi = 0.5 + (fhi/10.0);                           /* ... and mantissa */
      }
    }
    flo = 0.5 + (fhi * ((lo<hi)?lo:hi) / mxhilo);  /* scale lo value wrt high */
    if ( flo == fhi) flo--;
  }

  /* first two fields:  Min/Max Shading Value - slider to long */
  /* - fld_txt_maxlen = number of decimal points */

  fm->type = FLD_SLIDER_UPRIGHT; fm->subtype = FLD_LNG; fm->fld_txt_maxlen = 1;
  fm->fld_loend = 0; fm->fld_hiend = 99;
  *fm->fld_lng = flo;
  fm++;
  fm->type = FLD_SLIDER_UPRIGHT; fm->subtype = FLD_LNG; fm->fld_txt_maxlen = 1;
  fm->fld_loend = 1; fm->fld_hiend = 100;
  *fm->fld_lng = fhi;
  fm++;

  /* next field:  Scaling Factor (as power of 10) */

  fm->type = FLD_BIT; fm->fld_lowbit = SF_TENTH;
  *fm->fld_lng = BITI(exponent);
  fm++;

  /* last two fields:  Scaling Source & Override scaling */

  fm->type = FLD_BIT; fm->fld_lowbit = SS_MIN;
  *fm->fld_lng = BITI( (resetToData && outOfRange) ? SS_DATA : SS_ABOVE);
  fm++;
  fm->type = FLD_BIT; fm->fld_lowbit = OF_MIN;
  *fm->fld_lng = BITI( OF_ONE);
  fm++;

  /* the ending field
   *  - see the callback info comments below
   */

  fm->type = FLD_END;
  fm->client_call = orbitguiShadingScale_cancel_CB;
  fm->fld_txt0 = wildcard;

  /* the callback info goes into the FLDMENU structures 
   *  - put the ok callback as the (client_call)() of the first structure
   *  - put the ptr for the callback in the client_data of the first structure
   *  - put the cancel callback as the (client_call)() of the ending structure
   *    ***N.B. this was done above
   *  - BOTH callbacks will be called with the client_data first and the
   *    fldMenu pointer second
   */

  fldMenu->client_call = orbitguiShadingScale_ok_CB;
  fldMenu->client_data = (void *) cur_item;

  orbitgui_create_fldmenu_dialog( buttonw, "Shading Scaling"
                              , fldMenu);

}
