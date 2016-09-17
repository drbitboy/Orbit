/* orbitgui_gen_nis0.c - gui for nis frame generation from orbits */

/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* text_box.c -- demonstrate simple use of XmNactivateCallback
 * for TextField widgets.  Create a rowcolumn that has rows of Form
 * widgets, each containing a Label and a Text widget.  When
 * the user presses Return, print the value of the text widget
 * and move the focus to the next text widget.
 */
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/DialogS.h>

#include <math.h>
#include <stdio.h>
#include <malloc.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

static
char nisfovlbl[] = "FOV                           - degrees\0x.xx-y.yy---";

static
char *gen_labels[] = { "Name/ID for this sequence:"
                     , "max duration                  - # orbits (0 ignored):"
                     , "max duration                  - seconds (0 ignored):"
                     , "start time                    - UTC:"
                     , nisfovlbl
                     , "mirror scan start position"
                     , "mirror scan end position"
                     , "mirror steps/spectrum"
                     , "integration time per spectrum - seconds"
                     , "time delay per step           - seconds"
                     , "time delay per scan           - seconds"
                     , (char *) 0
                     } ;

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */
int /* void /**/
orbitgui_gen_nis0_ok_CB( ORBIT *orbit, FLDMENU *fldMenu)
{
IMGFRM *lclimgfrm;

  /* DPR1( "Entering orbitgui_gen_nis0_ok_CB // "); /**/
  orbit_utc2et( orbit->_agen._utc, &orbit->_agen._EtStart);
  orbit->_agen._typeAgen = AGEN_TYPE_TIMING;

  free( fldMenu);

  /* DPR1( "before gen_nis0_frames // "); /**/
  lclimgfrm = gen_nis0_frames( orbit);  /* actually generate the frames */
  /* DPR1( "before add_gen_frames // "); /**/
  add_gen_frames( orbit, lclimgfrm);
  /* DPR1( "leaving orbitgui_gen_nis0_ok_CB // "); /**/

  return(1);
}

/* Cancel callback - clean up fldMenu[] and the trailing strings */
int /* void /**/
orbitgui_gen_nis0_cancel_CB( ORBIT *orbit, FLDMENU *fldMenu)
{
  /* DPR(( stderr, "in gen_cancel_CB(); fldMenu=%08x\n", (int)fldMenu)); /**/
  free( fldMenu);
  return(1);
}

/**********************************************************************/
/* orbitgui_create_gen_nis0_dialog -- create dialog for frame generation
 */
void
orbitgui_create_gen_nis0_dialog( Widget buttonw, ORBIT *orbit)
{
int           i, numflds;
char          **labels;
char          lclstr[20];
double        *lclgen;
int allocsize;
char          *txt0;
static int icount;
static char wildcard[] = { "*.agennistmg" };

FLDMENU *fldMenu, *fm;

#define LENALLOC (1+( (UTCLEN>IDLEN) ? UTCLEN : IDLEN ))

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_txt0 strings all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  for ( numflds=1, labels=gen_labels; *labels; numflds++, labels++) ;
  allocsize = numflds * (LENALLOC + sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  txt0 = (char *)(fldMenu + numflds);

  /* DPR(( stderr, "in create_gen_nis0_dialog malloc'ed fldMenu=%08x\n"
         , (int)fldMenu)); /**/

  /* set up labels & saved text
   * ***N.B. the ->fld_txt0 fields point the the area after the FLDMENU area
   * ***N.B. fldMenu[numflds-1].txt0 is a null pointer
   */

  for ( i=0; i<numflds; i++, fm++, txt0 += LENALLOC) {
    fm->lbl_txt = gen_labels[i];
    fm->fld_txt0 = txt0;
  }

  fm = fldMenu;

  sprintf( fm->fld_txt0, "NIS autogen # %d", icount++);
  fm->fld_txt_maxlen = IDLEN;
  fm->type = FLD_TXT; fm->fld_txt = orbit->_agen._name; fm++;

  fm->type = FLD_DBL; fm->fld_dbl = &orbit->_agen._MaxOrbits; fm++;
  fm->type = FLD_DBL; fm->fld_dbl = &orbit->_agen._MaxTime; fm++;

  fm->fld_txt0 = orbit->_agen._utc;
  fm->fld_txt_maxlen = UTCLEN;
  fm->type = FLD_TXT; fm->fld_txt = orbit->_agen._utc; fm++;

  /* SPECIAL:  the nis frame size field */

  /* - put the NIS nominal FOV into the lbl_txt string */
  {
  char *cptr = fm->lbl_txt;
  SC *sc = (SC *) orbit->_sc;
  double fovx = sc->_nisfovx * 180.0 / M_PI;
    cptr += ( 1 + strlen( cptr));
    sprintf( cptr, "%4.2lf", fovx);
    cptr += ( 1 + strlen( cptr));
    sprintf( cptr, "%4.2lf", fovx*2.0);
    cptr += ( 1 + strlen( cptr));
    *cptr = '\0';
  }

  /* - set up the nis bits */
  fm->fld_lowbit = 0;
  fm->type = FLD_BIT; fm->fld_lng = &orbit->_agen._NISFOV; fm++;

  fm->type = FLD_INT; fm->fld_int = &orbit->_agen._StartingStep; fm++;
  fm->type = FLD_INT; fm->fld_int = &orbit->_agen._EndingStep; fm++;
  fm->type = FLD_INT; fm->fld_int = &orbit->_agen._DeltaStep; fm++;
  fm->type = FLD_INT; fm->fld_int = &orbit->_agen._TimePerStep; fm++;
  fm->type = FLD_INT; fm->fld_int = &orbit->_agen._DelayPerStep; fm++;
  fm->type = FLD_INT; fm->fld_int = &orbit->_agen._DelayPerRow; fm++;

  /* the ending field
   *  - see the callback info comments below
   */

  fm->type = FLD_END;
  fm->client_call = orbitgui_gen_nis0_cancel_CB;
  fm->fld_txt0 = wildcard;

  /* the callback info goes into the FLDMENU structures 
   *  - put the ok callback as the (client_call)() of the first structure
   *  - put the orbit structure ptr in the clientd_data of the first structure
   *  - put the cancel callback as the (client_call)() of the ending structure
   *    ***N.B. this was done above
   *  - BOTH callbacks will be called with the client_data first and the
   *    fldMenu pointer second
   */

  fldMenu->client_call = orbitgui_gen_nis0_ok_CB;
  fldMenu->client_data = (void *) orbit;

  /* DPR1( "before orbitgui_create_fldmenu_dialog // "); /**/
  orbitgui_create_fldmenu_dialog( buttonw
                              , "NIS Simple Frame Generation Data Entry"
                              , fldMenu);
  /* DPR1( "after orbitgui_create_fldmenu_dialog // "); /**/

}
