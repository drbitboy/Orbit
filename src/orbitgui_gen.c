/* orbitgui_gen.c - gui for frame generation from orbits */

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
/*
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/DialogS.h>
/**/
#include <X11/Intrinsic.h>

#include <math.h>
#include <stdio.h>
#include <malloc.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

static
char *gen_labels[] = { "Name/ID for this sequence:"
                     , "max duration  - # orbits (0 ignored):"
                     , "max duration  - seconds (0 ignored):"
                     , "frame overlap [& 'lap2] - fraction:"
                     , OVERLAP_TYPE_LABEL
                     , "start time    - UTC:"
                     , (char *) 0
                     } ;

/* OK callback - clean up fldMenu[] and the trailing strings,
 * then use the new values
 */ 
int /* void /**/
orbitgui_gen_ok_CB( ORBIT *orbit, FLDMENU *fldMenu)
{
IMGFRM *lclimgfrm;

  orbit->_agen._MaxOrbits = orbit->_newgenvals[0];
  orbit->_agen._MaxTime = orbit->_newgenvals[1];
  orbit->_agen._FrmOverlap = orbit->_newgenvals[2];
  /* set Overlap2 = Overlap if only one value was entered */
  /* - ***N.B. if 0 values entered, leave overlap & overlap2 as they are */
  if ( fldMenu[3].subtype == 1) {
    orbit->_newgenvals[3] = orbit->_newgenvals[2];
  }
  orbit->_agen._FrmOverlap2 = orbit->_newgenvals[3];
  orbit_utc2et( orbit->_newgenutc, &orbit->_agen._EtStart);
  orbit->_agen._typeAgen = 
    ( orbit->_agen._FrmOverlap2 == orbit->_agen._FrmOverlap) 
                       ? AGEN_TYPE_OLAP : AGEN_TYPE_OLAP2;

  free( fldMenu);

  lclimgfrm = gen_frames( orbit);  /* actually generate the frames */

  add_gen_frames( orbit, lclimgfrm);

  return(1);
}

/* Cancel callback - clean up fldMenu[] and the trailing strings */
int /* void /**/
orbitgui_gen_cancel_CB( ORBIT *orbit, FLDMENU *fldMenu)
{
  /* DPR(( stderr, "in gen_cancel_CB(); fldMenu=%08x\n", (int)fldMenu)); /**/
  free( fldMenu);
  return(1);
}

/**********************************************************************/
/* orbitgui_create_gen_dialog -- create dialog for frame generation
 */
void
orbitgui_create_gen_dialog( Widget buttonw, ORBIT *orbit)
{
int           i, numflds;
char          **labels;
char          lclstr[20];
double        *lclgen;
int allocsize;
char          *txt0;
static int icount;
SC *sc = (SC *)orbit->_sc;
static char wildcard[] = { "*.agenolap" };

FLDMENU *fldMenu, *fm;

#define MTLD2 (2 * FLD_MAXTXTLENPERDBL)
#define LENALLOC (1+( (UTCLEN>MTLD2) ? UTCLEN : MTLD2 ))

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_txt0 strings all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  for ( numflds=1, labels=gen_labels; *labels; numflds++, labels++) ;
  allocsize = numflds * (LENALLOC + sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  txt0 = (char *)(fldMenu + numflds);

  for ( i=0; i<numflds; i++, fm++, txt0 += LENALLOC) {
    fm->lbl_txt = gen_labels[i];
    fm->fld_txt0 = txt0;
  }

  fm = fldMenu;

  /* first field:  autogen Name/ID */

  fm->type = FLD_TXT; fm->fld_txt = orbit->_agen._name; 
  sprintf( fm->fld_txt0, "%s overlap autogen # %d", sc->_instrName, icount++);
  fm->fld_txt_maxlen = IDLEN;

  /* the next 2 fields:  max orbit & max time */

  fm++; fm->type = FLD_DBL; fm->fld_dbl = orbit->_newgenvals + 0;
  fm++; fm->type = FLD_DBL; fm->fld_dbl = orbit->_newgenvals + 1;

  /* the next field: overlap & optional overlap2
   *  - go into _newgenvals[2] & optionally into _newgenvals[3]
   *  - can handle up to 2 multiple doubles (->fld_txt_maxlen)
   *  - set current # of values (->fld_subtype) to 1 if 'lap == 'lap2, else 2 
   */

  fm++; fm->type = FLD_DBL8; fm->fld_dbl = orbit->_newgenvals + 2;
  fm->fld_txt_maxlen = 2;
  fm->subtype = (orbit->_newgenvals[2] == orbit->_newgenvals[3]) ? 1 : 2;

  /* the overlap type bits */
  fm++; fm->type = FLD_BIT; fm->fld_lng = &orbit->_agen._FrmOverlapTypeBits;
  fm->fld_lowbit = OVERLAP_TYPE_FIRST_BIT;

  /* the utc field */

  fm++; fm->type = FLD_TXT;
  fm->fld_txt0 = orbit->_agen._utc;
  fm->fld_txt = orbit->_newgenutc; fm->fld_txt_maxlen = UTCLEN;

  /* the ending field
   *  - see the callback info comments below
   */

  fm++; fm->type = FLD_END;
  fm->client_call = orbitgui_gen_cancel_CB;
  fm->fld_txt0 = wildcard;

  /* the callback info goes into the FLDMENU structures 
   *  - put the ok callback as the (client_call)() of the first structure
   *  - put the orbit structure ptr in the clientd_data of the first structure
   *  - put the cancel callback as the (client_call)() of the ending structure
   *    ***N.B. this was done above
   *  - BOTH callbacks will be called with the client_data first and the
   *    fldMenu pointer second
   */

  fldMenu->client_call = orbitgui_gen_ok_CB;
  fldMenu->client_data = (void *) orbit;

  /* put the current values into the new values
   * ***N.B. this should be obsolete if the new fldMenu callback stuff works
   */

  orbit->_newgenvals[0] = orbit->_agen._MaxOrbits;
  orbit->_newgenvals[1] = orbit->_agen._MaxTime;
  orbit->_newgenvals[2] = orbit->_agen._FrmOverlap;
  orbit->_newgenvals[3] = orbit->_agen._FrmOverlap2;

  strcpy( orbit->_newgenutc, orbit->_agen._utc);

  orbitgui_create_fldmenu_dialog( buttonw, "Frame Generation Data Entry"
                              , fldMenu);

}
