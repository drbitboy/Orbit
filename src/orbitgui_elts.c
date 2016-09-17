#include <X11/Intrinsic.h>

#include <stdio.h>
#include <malloc.h>
#include <math.h>

#include "orbitgui_fieldmenu.h"
#include "orbit3d.h"
#include "debug.h"

static
char *so_labels[] = { "rp    - perifocal dist., km:"
                    , "ecc   - eccentricity:"
                    , "inc   - inclination, deg:"
                    , "lnode - long. of asc. node, deg:"
                    , "argp  - arg. of periapsis, deg:"
                    , "m0    - mean anom. at epoch, deg:"
                    , "rho   - density of asteroid, g/cc:"
                    , "utc   - epoch, time:"
                    , "Select for precessing lnode & argp:\0(Press in to select)\0"
                    , (char *) 0
                    } ;

#define I_SO_UTC 7
#define I_SO_PRECESS 8

static
char *aa_labels[] = { "i     - inclination, deg:"
                    , "omega - long. of asc. node, deg:"
                    , "w     - argument of perih., deg:"
                    , "a     - mean distance, AU:"
                    , "n     - daily motion, deg/d:"
                    , "e     - eccentricity:"
                    , "m     - mean anom. at epoch, deg:"
                    , "tdt   - epoch, time:"
                    , "refrm - ref. frame. (e.g. J2000):"
                    , (char *) 0
                    } ;

#define I_AA_UTC 7
#define I_AA_REFRM 8

static double        radpd;

/* orbitgui_elts_ok_CB() -- callback for when the user hits the ok button
 * - convert user values to internal values
 * - free fldMenu & trailing strings
 */
int 
orbitgui_elts_ok_CB(ORBIT *orbit, FLDMENU *fldMenu)
{

  /* Astronomical Elements format - convert to SPICE orbital elements */
  if ( orbit->_newcenterid == 0) {

  double rmean = KMPERAU * orbit->_newvals[3];
  double radpersec = radpd * orbit->_newvals[4] / 86400.0;

    orbit->_so_inc = radpd * orbit->_newvals[0];
    orbit->_so_lnode = radpd * orbit->_newvals[1];
    orbit->_so_argp = radpd * orbit->_newvals[2];
    orbit->_so_rp = rmean * (1.0 - orbit->_newvals[5]);
    orbit->_so_ecc = orbit->_newvals[5];
    orbit->_so_m0 = radpd * orbit->_newvals[6];
    orbit_tparse( orbit->_newutc, orbit->_so+SO_T0);
    orbit->_so_mu = rmean * pow( rmean * radpersec, 2.0); 
    orbit_getmtx( orbit->_newcenterid, orbit->_newrefrm
                , orbit->_so+SO_T0, orbit->_toj2k);
    orbit->_period = 86400.0 * 360.0 / orbit->_newvals[4];
    orbit->_rho = 0.0;

  /* SPICE orbital elements format - convert degrees to radians, rho to mu, &c*/
  } else {


  SC *orbsc = (SC *) orbit->_sc;
  double r3, eps;

#define GPCC2KGPCK 1e+12  /* convert g/cc to kg/km^3 */
#define ASTERAD(I) orbsc->_asterad[I]
#define ASTERVOL  (240.0 * radpd * ASTERAD(0) * ASTERAD(1) * ASTERAD(2))

    orbit->_so_rp = orbit->_newvals[0];
    orbit->_so_ecc = orbit->_newvals[1];
    orbit->_so_inc = radpd * orbit->_newvals[2];
    orbit->_so_lnode = radpd * orbit->_newvals[3];
    orbit->_so_argp = radpd * orbit->_newvals[4];
    orbit->_so_m0 = radpd * orbit->_newvals[5];
    orbit_utc2et( orbit->_newutc, orbit->_so+SO_T0);

    orbit->_so_mu = GRAVCON * ASTERVOL * orbit->_newvals[6] * GPCC2KGPCK;

    orbit_getmtx( orbit->_newcenterid, orbit->_newrefrm
                , orbit->_so+SO_T0, orbit->_toj2k);

    /* save values for info */
    r3 = pow( orbit->_so_rp/(1 - orbit->_so_ecc), 3.0);
    orbit->_period = (360.0 * radpd) / sqrt( orbit->_so_mu / r3);
    orbit->_rho = orbit->_newvals[6];

    /* make calculations for precessing orbit around non-spherical body */
    /* - assume (spin period << orbital period) => Req ~ sqrt(a*b) */
    /* - set _so_Rpl = Requatorial */
    /* - set m0 to 0.0 */
    /* - get unit vector of orbit at T0 */
    /* - get unit vector of trajectory pole at T0 */

    orbit->_so_precess = orbit->_newprecess;

    if ( orbit->_so_precess) {
    VEC6 v6;
      orbit->_so_Rpl = sqrt( ASTERAD(0) * ASTERAD(1));
      eps = (orbit->_so_Rpl - ASTERAD(2)) / orbit->_so_Rpl;
      orbit->_so_J2 = eps * (2.0 - eps) / 5.0;
      orbit->_so_m0 = orbit->_newvals[5] = 0.0;
      orbit_conics( orbit->_so, orbit->_so+SO_T0, v6);
      orbit->_so_uPA0[0] = v6[0] / orbit->_so_rp;
      orbit->_so_uPA0[1] = v6[1] / orbit->_so_rp;
      orbit->_so_uPA0[2] = v6[2] / orbit->_so_rp;
      orbit->_so_uTP0[0] = sin(orbit->_so_inc) * sin(orbit->_so_lnode);
      orbit->_so_uTP0[1] = - sin(orbit->_so_inc) * cos(orbit->_so_lnode);
      orbit->_so_uTP0[2] = cos(orbit->_so_inc);

    } else {
      orbit->_so_J2 = 0.0;
    }

  }
  orbit->_status = ORB_USESO;

  free( fldMenu);
  return(1);
}

/* orbitgui_elts_cancel_CB() - callback for when the user hits the cancel button * - clean up fldMenu & trailing strings
 */
int 
orbitgui_elts_cancel_CB(ORBIT *orbit, FLDMENU *fldMenu) 
{
  free( fldMenu);
  return(1);
}

void
orbitgui_create_elts_dialog( Widget buttonw, ORBIT *orbit)
{
static int notfirst;
int thisisso, thisisaa;

int i, numflds;
char **labels, **usetheselabels;
char          lclstr[20];
double        *dblptr;
int allocsize;
char          *txt0;

FLDMENU *fldMenu, *fm;

#define LENALLOC 30

  if ( orbit->_newcenterid == 0) {                       
    labels = usetheselabels = aa_labels;       
    thisisaa = 1;
    thisisso = 0;
  } else {          
    labels = usetheselabels = so_labels;
    thisisaa = 0;                              
    thisisso = 1;
  }

  /* allocate the space for the FLDMENU structures plus the
   * ->fld_txt0 strings all at once so it can be freed all at once
   * ***N.B. there is an extra FLDMENU structure allocated as the
   *         ->type FLD_END structure
   */

  for ( numflds=1; *labels; numflds++, labels++) ;
  allocsize = numflds * (LENALLOC + sizeof( FLDMENU));

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  txt0 = (char *)(fldMenu + numflds);

  /* set up labels & saved text
   * ***N.B. the ->fld_txt0 fields point the the area after the FLDMENU area
   * ***N.B. fldMenu[numflds-1].txt0 is a null pointer
   */

  for ( i=0; i<numflds; ++i, ++fm, txt0 += LENALLOC) {
    fm->lbl_txt = usetheselabels[i];
    fm->fld_txt0 = txt0;
  }

  /* thats all the standard stuff, now define the fields */

  if ( !notfirst) {                                    
    radpd = atan(1.0) / 45.0;                           
    notfirst = 1;                                       
  }                                                    

  fm = fldMenu;

  for ( labels=usetheselabels, i=0, dblptr = orbit->_newvals; 
        i < (thisisaa?I_AA_UTC:I_SO_UTC); 
        ++i, ++dblptr, ++labels, ++fm) {
    fm->type = FLD_DBL; fm->fld_dbl = dblptr;
  }

#define ADDTEXT(T,L) \
      fm->type = FLD_TXT; fm->fld_txt = T; \
      fm->fld_txt0 = T; fm->fld_txt_maxlen = L

  for ( ; *labels; ++i, ++labels, ++fm) {
    if ( i == (thisisaa?I_AA_UTC:I_SO_UTC) ) { /* TDT text widget */
      ADDTEXT(orbit->_newutc, UTCLEN);

    } else if ( thisisaa && i==I_AA_REFRM) { /* reference frame for AA */
      ADDTEXT(orbit->_newrefrm,REFRMLEN);

    /* ... and toggle button for precession for SO */
    } else if ( thisisso && i==I_SO_PRECESS) {
      /* trim all but bit 0 */
      orbit->_newprecess = (1 & orbit->_so_precess);
      fm->type = FLD_BITS; fm->fld_lng = &orbit->_newprecess;
      fm->fld_lowbit = 0;
    }
  }

  fm->type = FLD_END;
  fm->client_call = orbitgui_elts_cancel_CB;

  if ( orbit->_newcenterid == 0) {                       
    strcpy( fm->fld_txt0, "*.bodyorbit");
  } else {
    strcpy( fm->fld_txt0, "*.scorbit");
  }

  /* the callback info goes into the FLDMENU structures
   *  - put the ok callback as the (client_call)() of the first structure
   *  - put the orbit structure ptr in the clientd_data of the first structure
   *  - put the cancel callback as the (client_call)() of the ending structure
   *    ***N.B. this was done above
   *  - BOTH callbacks will be called with the client_data first and the
   *    fldMenu pointer second
   */

  fldMenu->client_call = orbitgui_elts_ok_CB;
  fldMenu->client_data = (void *) orbit;

  orbitgui_create_fldmenu_dialog( buttonw
                              , "Orbital Elements Data Entry"
                              , fldMenu);

}
