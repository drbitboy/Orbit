#include <X11/Intrinsic.h>

#include <stdio.h>
#include <malloc.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

#define EXYZOK -1
#define EXYZRESET -2
#define EXYZCANCEL -3

void enterxyz_act(void *);

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */
int /* void /**/
orbit_enterxyz_ok_CB( void *cur_item, FLDMENU *fldMenu)
{
  /* free( fldMenu); /**/
  enterxyz_act( cur_item);
  return(0); /* return 0 to avoid destroying widget */
}

/* Cancel callback - clean up fldMenu[] and the trailing strings */
int /* void /**/
orbit_enterxyz_cancel_CB( void *cur_item, FLDMENU *fldMenu)
{
  free( fldMenu);
  return(1);
}

void
orbit_enterxyz( void *cur_item, Widget toplevel
              , int icount, char **lblptr, char *wildcard, double *xyzout)
{
char c255[255];

int n;
int i;
FLDMENU *fldMenu, *fm;
int numflds;
char *txt0;
char **lcllblptr;
int allocsize;

#define LENALLOC 60

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_txt0 strings all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  /* for ( numflds=1,lcllblptr=lblptr; *lcllblptr; numflds++,lcllblptr++) ; /**/
  numflds = icount+1;
  allocsize = numflds * (LENALLOC + sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  txt0 = (char *)(fldMenu + numflds);

  for ( i=0; i<icount; i++, fm++, txt0 += LENALLOC) {
    fm->type = FLD_DBL;
    fm->lbl_txt = lblptr[i];
    fm->fld_txt0 = txt0;
    fm->fld_dbl = xyzout + i;
  }

  /* fm++; /**/
  fm->type = FLD_END;
  fm->client_call = orbit_enterxyz_cancel_CB;
  strcpy( fm->fld_txt0 = txt0, wildcard);

  fldMenu->client_call = orbit_enterxyz_ok_CB;
  fldMenu->client_data = cur_item;

  orbitgui_create_fldmenu_dialog( toplevel, "Pointing Info Data Entry"
                              , fldMenu);

  return;
}
