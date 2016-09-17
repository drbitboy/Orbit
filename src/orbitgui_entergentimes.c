/* orbitgui_entergentimes.c */

#include <X11/Intrinsic.h>

#include <stdio.h>
#include <malloc.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "pointing.h"
#include "debug.h"
#include "orbit_gentimes.h"

static GENTIME dfltGentime;   /* defaults */

static char *lblptr[] = {
    "Filename"
  , "Time Format\0kMETdp\0METdp\0SCLKtxt\0ETdp\0UTCtxt\0"
  , "Time - Start,End Col #'s"
  , "Instrument - Col #"
  , "Mirror Position - Col #"
  , "# of Header lines to skip"
  , (char *) 0
  };

#define LENCOLNUM 16  /* 16 characters should be enough to enter a column # */

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */
int
orbitgui_entergentimes_ok_CB( GENTIME *gentime, FLDMENU *fldMenu)
{

  /* fill out time columns if only one entered
   * ***N.B. if .subtype is 0, these values will not change from their
   *         original settings
   */
  if (fldMenu[2].subtype==1)gentime->_timeColumns[1] = gentime->_timeColumns[0];

  /* if orbit_gentimes_act() returns 0 save gentime values to default 
   * GENTIME struct, free fldMenu, & return 1 to * close field menu.
   * otherwise return 0 so user can re-enter fields
   */
  if ( orbit_gentimes_act( gentime) == 0) {
    dfltGentime = *gentime;  /* success, save gentime struct to default */
    free( fldMenu);
    return 1;
  }

  return 0;
}

/* Cancel callback - clean up fldMenu[] and the trailing strings */
int
orbitgui_entergentimes_cancel_CB( GENTIME *gentime, FLDMENU *fldMenu)
{
  free( fldMenu);
  return 1;
}

void
orbitgui_entergentimes_CB( Widget w, XtPointer client_data, XtPointer call_data)
{
SC *sc = (SC *) client_data;
char **lcllblptr;
long numflds, allocsize, i;
FLDMENU *fldMenu, *fm;
char *txt0;
GENTIME *gentime;

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_txt0 strings all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  for ( numflds=1,lcllblptr=lblptr; *lcllblptr; numflds++,lcllblptr++) ; /**/
  allocsize = (numflds * sizeof( FLDMENU))              /* fldMenu structures */
            + sizeof(GENTIME)                            /* GENTIME structure */
            + (numflds * LENCOLNUM)        /* saved text - extra for time col */
            + MAXLENFILNAM;                        /* saved text for filename */

            /* ***N.B. extra txt0 space for time column # is available from
             *         time format bit field which uses no txt0 space
             */

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  gentime = (GENTIME *)(fldMenu + numflds);
  txt0 = (char *)(gentime + 1);

  /* first pass - dfltGentime is all zeroes
   * - set default gentime structure values */

  if ( dfltGentime._timeFormat == 0) {
    /* *dfltGentime._filnam = '\0'; */
    strcpy( dfltGentime._filnam, "*.met");
    dfltGentime._timeColumns[0] = dfltGentime._timeColumns[1] = 1;
    dfltGentime._timeFormat = 1 << GTFMT_KMET;
    dfltGentime._instrColumn = 2;
    dfltGentime._mirrorPosnColumn = 3;
    dfltGentime._headerLines = 3;
  }

  /* load defaults, save SC pointer from argument list */

  *gentime = dfltGentime;
  gentime->_sc = sc;

#define NEXTFM(A) fm->lbl_txt = lblptr[i++]; \
                  fm->fld_txt0 = txt0; \
                  if ( fm->type == FLD_TXT || fm->type == FLD_TXTFILEREAD || \
                       fm->type == FLD_TXTFILEREAD) \
                    strncpy( fm->fld_txt0, fm->fld_txt, fm->fld_txt_maxlen); \
                  txt0 += (A); fm++

  i = 0;

  /* Filename */
  fm->type = FLD_TXTFILEREAD;
  fm->fld_txt = gentime->_filnam;
  fm->fld_txt_maxlen = MAXLENFILNAM;
  NEXTFM( fm->fld_txt_maxlen);

  /* time format */
  fm->type = FLD_BIT;
  fm->fld_lng = &gentime->_timeFormat;
  fm->fld_lowbit = 0;
  NEXTFM( 0);               /* no txt0 space used */

  /* time start, end column #'s */
  fm->type = FLD_LNG8;
  fm->fld_lng = gentime->_timeColumns;
  fm->subtype = fm->fld_txt_maxlen = 2;
  NEXTFM( 2 * LENCOLNUM);   /* uses extra txt0 space */

  /* instrument column # */
  fm->type = FLD_LNG;
  fm->fld_lng = &gentime->_instrColumn;
  NEXTFM( LENCOLNUM);

  /* mirror position column # */
  fm->type = FLD_LNG;
  fm->fld_lng = &gentime->_mirrorPosnColumn;
  NEXTFM( LENCOLNUM);

  /* # of header lines to skip */
  fm->type = FLD_LNG;
  fm->fld_lng = &gentime->_headerLines;
  NEXTFM( LENCOLNUM);

  fm->type = FLD_END;
  fm->client_call = orbitgui_entergentimes_cancel_CB;
  strcpy( fm->fld_txt0 = txt0, "*.gentime");

  fldMenu->client_call = orbitgui_entergentimes_ok_CB;
  fldMenu->client_data = (void *) gentime;

  orbitgui_create_fldmenu_dialog( w, "Autogen from file"
                                , fldMenu);
  return;
}
