#include <X11/Intrinsic.h>

#include <stdio.h>
#include <malloc.h>
#include <string.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

void orbitgui_selnis_act( SC *, fortint, fortint);

/* OK callback - update the sc instrument using
 *                 the new step (fldmenu[0].fld_fortint)
 *                 & the new instrument (fldmenu[1].fld_fortint)
 */
int
orbit_selnis_menu_ok_CB( SC *sc, FLDMENU *fldMenu)
{
  orbitgui_selnis_act( sc, *fldMenu[0].fld_fortint, *fldMenu[1].fld_fortint);
  return(0); /* return 0 to avoid destroying widget */
}

/* Cancel callback - call selnis_done routine
 *                 - clean up fldMenu[] and the trailing strings
 */
int
orbit_selnis_menu_cancel_CB( SC *sc, FLDMENU *fldMenu)
{
void orbitgui_selnis_done(SC *);
  orbitgui_selnis_done( sc);
  free( fldMenu);
  return(1);
}

void
orbit_selnis_menu( SC *sc, Widget toplevel, int sc_instr)
{
FLDMENU *fldMenu, *fm;
int allocsize;
fortint *new_instr, *update_step;
static char wildcard[] = { "*.selnis" };

  /* allocate the space for the FLDMENU structures plus the
   * two fortints:  one for the instrument type (SC_NIS or SC_NIS2),
   * and the other for the current step
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  allocsize = (2 * sizeof(fortint)) + (2 * sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  new_instr = (fortint *)(fldMenu + 2);
  *new_instr = sc_instr;
  update_step = new_instr + 1;
  *update_step = sc->_nisStepAct;

  fm->type = FLD_SLIDER_UPLEFT;
  fm->subtype = FLD_FORTINT;
  fm->lbl_txt = "NIS Mirror Position";
  fm->fld_loend = 0;
  fm->fld_hiend = sc->_nisStepMax;
  fm->fld_txt_maxlen = 0;            /* number of decimal points */
  fm->fld_fortint = update_step;     /* store the step here */

  fm++;
  fm->type = FLD_END;
  fm->client_call = orbit_selnis_menu_cancel_CB;
  fm->fld_fortint = new_instr;       /* stor the new intrument here */
  fm->fld_txt0 = wildcard;

  fldMenu->client_call = orbit_selnis_menu_ok_CB;
  fldMenu->client_data = sc;

  orbitgui_create_fldmenu_dialog( toplevel, "Pointing Info Data Entry"
                              , fldMenu);

  return;
}
