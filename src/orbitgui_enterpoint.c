#include <X11/Intrinsic.h>

#include <stdio.h>
#include <malloc.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "pointing.h"
#include "debug.h"

void enterpoint_act(void *);
void orbitgui_return_boreroll_ci( void *, POINTING **, POINTING **);

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */
int /* void /**/
orbitgui_enterpoint_ok_CB( void *cur_item, FLDMENU *fldMenu)
{
POINTING *bore, *roll;

#define FINDBIT( TYPE, N) \
  if ( *fldMenu[N].fld_lng) { \
    for ( TYPE=0; !((1<<TYPE) & *fldMenu[N].fld_lng); TYPE++) ; \
  }

  /* get bore & roll structures from cur_item */
  orbitgui_return_boreroll_ci( cur_item, &bore, &roll);

  FINDBIT( bore->aimpt.type, 0)
  FINDBIT( bore->scvec.type, 2)
  FINDBIT( roll->aimpt.type, 4)
  FINDBIT( roll->scvec.type, 6)

  enterpoint_act( cur_item);
  return(0); /* return 0 to avoid destroying widget */
}

/* Cancel callback - clean up fldMenu[] and the trailing strings */
int /* void /**/
orbitgui_enterpoint_cancel_CB( void *cur_item, FLDMENU *fldMenu)
{
  free( fldMenu);
  return(1);
}

void
orbitgui_enterpoint( void *cur_item, Widget w)
{
static char *lblptr[] = {
    "Aimpt Type\0J2k\0ECI\0SCI\0ACI\0Nadir\0ABF\0"
  , "Aimpt Vec"
  , "S/C Virt Bore Type\0Instr\0Pnl\0X\0Y\0Z\0Usr:\0"
  , "S/C Virt Bore Vec"
  , "Extern Roll Ref Type\0J2k\0ECI\0SCI\0ACI\0Nadir\0ABF\0"
  , "Extern Roll Ref Vec"
  , "S/C Roll Vec Type\0Instr\0Pnl\0X\0Y\0Z\0Usr:\0"
  , "S/C Roll Vec"
  , (char *) 0
  };
char **lcllblptr;
long numflds, allocsize, i;
FLDMENU *fldMenu, *fm;
char *txt0;
POINTING *bore, *roll;

#define LENALLOC (1 + ( 3 * FLD_MAXTXTLENPERDBL))

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_txt0 strings all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  for ( numflds=1,lcllblptr=lblptr; *lcllblptr; numflds++,lcllblptr++) ; /**/
  allocsize = numflds * (LENALLOC + sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  txt0 = (char *)(fldMenu + numflds);

  /* get bore & roll structures from cur_item */
  orbitgui_return_boreroll_ci( cur_item, &bore, &roll);

  /* use txt0 for text widget text or for bits in FLD_BIT 
   * - only one will be used in each entry field
   * - even fields are BIT, odd fields are vectors
   */

  for ( i=0; i<(numflds-1); i++, fm++, txt0 += LENALLOC) {
    if ( i % 2) {
      fm->type = FLD_DBL8;
      fm->subtype = fm->fld_txt_maxlen = 3;
      fm->lbl_txt = lblptr[i];
      fm->fld_txt0 = txt0;
      switch (i) {
      case 1: fm->fld_dbl = bore->aimpt.vec; break;
      case 3: fm->fld_dbl = bore->scvec.vec; break;
      case 5: fm->fld_dbl = roll->aimpt.vec; break;
      case 7: fm->fld_dbl = roll->scvec.vec; break;
      }

    } else {
      fm->type = FLD_BIT;
      fm->lbl_txt = lblptr[i];
      fm->fld_lng = (long *) txt0;
      fm->fld_lowbit = 0;
      switch (i) {
      case 0: *fm->fld_lng = 1 << bore->aimpt.type; break;
      case 2: *fm->fld_lng = 1 << bore->scvec.type; break;
      case 4: *fm->fld_lng = 1 << roll->aimpt.type; break;
      case 6: *fm->fld_lng = 1 << roll->scvec.type; break;
      }
    }
  }

  fm->type = FLD_END;
  fm->client_call = orbitgui_enterpoint_cancel_CB;
  strcpy( fm->fld_txt0 = txt0, "*.point4");

  fldMenu->client_call = orbitgui_enterpoint_ok_CB;
  fldMenu->client_data = cur_item;

  orbitgui_create_fldmenu_dialog( w, "Pointing Info Data Entry"
                                , fldMenu);
 return;
}
