/* orbitgui_framesout.c - gui for controlling frames output */

#include <X11/Intrinsic.h>

#include <math.h>
#include <stdlib.h>
#include <malloc.h>

#include "orbitgui_framesout.h"
#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */ 
int
orbitgui_framesout_ok_CB( ORBIT *orbit, FLDMENU *fldMenu)
{
IMGFRM *lclimgfrm;

  free( fldMenu);
  return(1);
}

/* Cancel callback - clean up fldMenu[] and the trailing strings */
int /* void /**/
orbitgui_framesout_cancel_CB( ORBIT *orbit, FLDMENU *fldMenu)
{
  free( fldMenu);
  return(1);
}

/**********************************************************************/
/* orbitgui_create_framesout_dialog -- create dialog for selecting frames 
 * output items
 */
void
orbitgui_create_framesout_dialog( Widget toplevel, ORBIT *orbit)
{
int           curbit, curbitinlong, curindex, currow, numflds;
int allocsize;

static char lbltxtarray[FRAMESOUT_NUMROWS][255];
char *lbltxt = (char *) 0;

static char *leftlbl[] = { "Write a CK\0In=>Write it\0"
                         , "Toggle item(s) in to include"
                         , "in frames output; toggle out"
                         , "to exclude."
                         , (char *) 0 
                         };
char **lcllbl = leftlbl;
static char wildcard[] = { "*.agenoutput" };

FLDMENU *fldMenu, *fm;

  /* allocate the space for the FLDMENU structures
   * 1 for CK out bit, 1 for FLD_END, FRAMESOUT_NUMROWS for the rest
   */

  numflds = 1 + FRAMESOUT_NUMROWS + 1;
  allocsize = numflds * (sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);

  /* whether to write a C Kernel file or not */

  fm->type = FLD_BITS; fm->fld_lowbit = 0; fm->lbl_txt = *(lcllbl++);
  fm->fld_lng = &orbit->_ckOutBit;
  fm++;

  /* frames' output bit fields - must be last fields before FLD_END */

  for ( currow = -1, curbit = 0, fm--; curbit < FRAMESOUT_BIT_LAST; curbit++) {
    curbitinlong = curbit % FRAMESOUT_BITSPERLONG;
    curindex = curbit / FRAMESOUT_BITSPERLONG;

#define SWITCHCASE( BIT, LBL) \
    case BIT: /* find which bit this is from enum in orbitgui_framesout.h */ \
      if ( !(curbitinlong % FRAMESOUT_FLDSPEROW) ) { /* special processing */ \
        currow++;                                  /* for first bit in row */ \
        fm++; \
        fm->fld_lowbit = curbitinlong; \
        fm->type = FLD_BITS; fm->fld_lng = orbit->_framesoutBits+curindex; \
        fm->lbl_txt = lbltxt = lbltxtarray[currow]; \
        if ( *lcllbl ) { \
          strcpy( lbltxt, *lcllbl); \
          lcllbl++; \
        } else { \
          strcpy( lbltxt, " "); \
        }  \
        lbltxt += strlen(lbltxt) + 1; \
      } \
      strcpy( lbltxt, LBL);         /* copy label text */ \
      lbltxt += strlen(lbltxt) + 1; /* move label ptr */ \
      *lbltxt = '\0';               /* terminate labels */ \
      break

     switch ( curbit) {
     SWITCHCASE( FRAMESOUT_BIT_TIME, FRAMESOUT_LBL_TIME);
     SWITCHCASE( FRAMESOUT_BIT_P5PHOTOM, FRAMESOUT_LBL_P5PHOTOM);
     SWITCHCASE( FRAMESOUT_BIT_P5VEC, FRAMESOUT_LBL_P5VEC);
     SWITCHCASE( FRAMESOUT_BIT_SCVEC, FRAMESOUT_LBL_SCVEC);
     SWITCHCASE( FRAMESOUT_BIT_SUNVEC, FRAMESOUT_LBL_SUNVEC);
     SWITCHCASE( FRAMESOUT_BIT_BOREJ2K, FRAMESOUT_LBL_BOREJ2K);
     SWITCHCASE( FRAMESOUT_BIT_SC2SUNJ2K, FRAMESOUT_LBL_SC2SUNJ2K);
     SWITCHCASE( FRAMESOUT_BIT_OFFSUN, FRAMESOUT_LBL_OFFSUN);
     SWITCHCASE( FRAMESOUT_BIT_EARTHVEC, FRAMESOUT_LBL_EARTHVEC);
     SWITCHCASE( FRAMESOUT_BIT_GENINFO, FRAMESOUT_LBL_GENINFO);
     SWITCHCASE( FRAMESOUT_BIT_NISINFO, FRAMESOUT_LBL_NISINFO);
     SWITCHCASE( FRAMESOUT_BIT_AGENINFO, FRAMESOUT_LBL_AGENINFO);
     SWITCHCASE( FRAMESOUT_BIT_POINTING, FRAMESOUT_LBL_POINTING);
     SWITCHCASE( FRAMESOUT_BIT_CAMPTVECS, FRAMESOUT_LBL_CAMPTVECS);
     SWITCHCASE( FRAMESOUT_BIT_P5PLATE, FRAMESOUT_LBL_P5PLATE);
     SWITCHCASE( FRAMESOUT_BIT_ALLINFO, FRAMESOUT_LBL_ALLINFO);
     SWITCHCASE( FRAMESOUT_BIT_SCLKQUAT, FRAMESOUT_LBL_SCLKQUAT);
     SWITCHCASE( FRAMESOUT_BIT_TIME_ALT, FRAMESOUT_LBL_TIME_ALT);
     SWITCHCASE( FRAMESOUT_BIT_TARGVECSBF, FRAMESOUT_LBL_TARGVECSBF);
     SWITCHCASE( FRAMESOUT_BIT_OTHERBODIES, FRAMESOUT_LBL_OTHERBODIES);
     }
  }

  /* the ending field
   *  - see the callback info comments below
   */

  fm++;
  fm->type = FLD_END;
  fm->client_call = orbitgui_framesout_cancel_CB;
  fm->fld_txt0 = wildcard;

  /* the callback info goes into the FLDMENU structures 
   *  - put the ok callback as the (client_call)() of the first structure
   *  - put the orbit structure ptr in the clientd_data of the first structure
   *  - put the cancel callback as the (client_call)() of the ending structure
   *    ***N.B. this was done above
   *  - BOTH callbacks will be called with the client_data first and the
   *    fldMenu pointer second
   */

  fldMenu->client_call = orbitgui_framesout_ok_CB;
  fldMenu->client_data = (void *) orbit;

  orbitgui_create_fldmenu_dialog( toplevel, "Frames' Output Content Control"
                              , fldMenu);

}
