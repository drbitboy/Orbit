#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#define MAXMACRO(A,B) (((A)>(B))?(A):(B))
#define MINMACRO(A,B) (((A)<(B))?(A):(B))
#include <X11/Xatom.h>
#include <Xmu/Atoms.h>
#include <Xm/FileSB.h>
#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/CutPaste.h>

#include "orbitgui_hc.h"
#include "orbit_spice_names.h"
#include "orbitgui_fieldmenu.h"

#include "spudshap.h"
#include "pointing.h"
#include "rainbow.h"

#include "local.h"
#include "debug.h"

enum { Drawdev_xwin, Drawdev_ps, Drawdev_xfig, Drawdev_gif, Drawdev_gifs };

#define NINT(A) ((int)(0.5+(A)))

#define r2d(A) A 
#define MAX_ARGS	200
#define SimpleV		1
/* #define deg		*3.14159265358979323846/180. /**/
/* #define rad		*180./3.14159265358979323846 /**/
#define deg		*(M_PI)/180. /**/
#define rad		*180./(M_PI)
#define initial_Noraz	90.
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

#define ORBIT_NOREPEAT "ORBIT_NOREPEAT"

#define RHO_FMT( OUTSTR, RHO) sprintf( OUTSTR, "Zoom: %6.2g", RHO)
#define RHO_MAX 1.3010
#define RHO_MIN -1.3010
#define PHI_MAX 90.
#define PHI_MIN -90.
#define PHI_WRAP_HI 90.
#define PHI_WRAP_LO -90.
#define NORAZ_MIN 0.
#define NORAZ_MAX 360.
#define NORAZ_WRAP_HI 0.
#define NORAZ_WRAP_LO 360.
#define THETA_MIN 0.
#define THETA_MAX 360.
#define THETA_WRAP_HI 0.
#define THETA_WRAP_LO 360.

#define YEAR_MIN 1950
#define YEAR_MAX 2100
#define DOY_MIN 1
#define DOY_MAX 366
#define HOUR_MIN 0
#define HOUR_MAX 23
#define MINUTE_MIN 0
#define MINUTE_MAX 59
#define SECOND_MIN 0
#define SECOND_MAX 59

#define TRACK_BODY "Track Body Center"
#define TRACK_FRAME "Track Image Frame"
#define SCALE_BODY "Scale to Body"
#define SCALE_FRAME "Scale to Image Frame"

static char tmpstring100[100];
static fortint lenin, leninmax, lenout;

/* Precede doy date with calender date in utc_label.
 * truncate calender date (YYYY MMM DD HH:MM:SS.CC) after 12 characters
 * year(4 chars), space(1), month(3), space(1), date(2), space(1)
 * then add two spaces - this will work until 100000 AD
 */
#define MAKEUTCXMSTR( UTCDOYSTR, UTCXMSTR) \
  strcpy( tmpstring100, UTCDOYSTR); \
  lenin = strlen( tmpstring100); \
  leninmax = UTCLEN - 1; \
  ospice_utc2cal( &lenin, &leninmax, &lenout, tmpstring100); \
  strcpy( tmpstring100+12, "  "); \
  strcat( tmpstring100, UTCDOYSTR); \
  UTCXMSTR =  XmStringCreateLtoR( tmpstring100, CHARSET)

enum {
  WALKIMGFRM_FIRST=0
, WALKIMGFRM_BACKHALF
, WALKIMGFRM_BACKTIME   /* will be the same Widget as _BACKHALF */
, WALKIMGFRM_BACK1
, WALKIMGFRM_FWD1
, WALKIMGFRM_FWDHALF
, WALKIMGFRM_FWDTIME   /* will be the same Widget as _FWDHALF */
, WALKIMGFRM_LAST
, WALKIMGFRM_COUNT
};
enum { FOV100_VISIBLE=0, FOV100_INVISIBLE };
enum { TEXTSTYLE_NONE=0, TEXTSTYLE_ORIGINAL
     , TEXTSTYLE_RANGE, TEXTSTYLE_RANGE_TRUNC3 };
enum { LINESTYLE_WHITE=0, LINESTYLE_GRAY, LINESTYLE_NONE };
enum { 
  SHADE_MU0=0, SHADE_MU, SHADE_MU0xMU
, SHADE_INCID, SHADE_EMISS, SHADE_MAPDATA
};
enum { COLOR_NONE=0, COLOR_GRAY, COLOR_RAINBOW };
enum { DRAG_NONE=0, DRAG_XLATE, DRAG_STRETCH, DRAG_TIME, DRAG_LAST };

typedef struct item_struct {
struct item_struct *next_item;
Widget _app_shell;
Widget _form;
Widget _instr_cascade;
Widget _minVertsW;
Widget _colorW;
Widget _linestyleW;
Widget _shadeW;
Widget _hideframes3W;
Widget _hiddenW;
Widget _perspW;
Widget _northupW;
Widget _scalebarW;
Widget _textStyleW;
Widget _tryckW;
Widget _scale2frameW;
Widget _centerframeW;

POINTING _bore, _roll;

Orbit _orbitstr;

Widget _Noraz_label;
Widget _Noraz_W;
Widget _rho_label;
Widget _rho_W;
Widget _phi_label;
Widget _phi_W;
Widget _theta_label;
Widget _theta_W;
Widget _drawing_area;
Boolean _initial;
int	_old_Noraz, _old_phi, _old_theta, _old_rho;
int	_Tx, _Ty, _dTx, _dTy, _da_width, _da_height, _da_height_pm;
XImage *_xImage;
unsigned long _lastPixel;
int _lastDN;
double _rho;
double _da_size;
XPoint _saveCtr;
double _savePxlPerKm;
double _R[4][4];
double _V[4][4];
double _Noraz;
double _new_R[4][4];
double _new_dR[4][4];
Display	*_da_dpy;
Window	_da_win;
Pixmap	_da_pm;
unsigned int _da_pm_depth;
GC	_fgc;
GC	_SHAPE_gc, _SHAPE_gray_gc;
GC _bgc;
GC _R_gc;
GC _G_gc;
GC _B_gc;
GC _Y_gc;
GC _M_gc;
GC _C_gc;
GC _GO_gc[5];
GC *_rainbowGc;
GC *_grayGc;
long _nrainbow;
long _ngray;
SPUDV *_spudv;

SC *_sc;
AP *_ap;

ORBIT _scOrbit
    , _asterOrbit
    , _earthOrbit
    ;

UTC _utc, _saved_utc;
int _pointing_solve_ok;

Widget _year_W, _doy_W, _hour_W, _minute_W, _second_W;
Widget _year_lab_W, _doy_lab_W, _hour_lab_W, _minute_lab_W
     , _second_lab_W;
Widget _utc_label;
XmString _utc_Xmstr;

#define XmSF(XMSTR) if ( XMSTR) XmStringFree( XMSTR)

IMGFRM *_imgfrm;
IMGFRM _currentImgfrm;
int _currentImgfrmValid;

int _hideframes3;
#define HIDEFRMIF_FACEAWAY 1                    /* hide frames if facing away */
#define HIDEFRMIF_UNLIT 2                             /* hide frames if unlit */
#define HIDEFRMIF_GT1ORBIT 4/* hide frames if not within one of current orbit */

int _ongoingDrag;
int _hideframes;
int _currentframes;
int _hideUnselectedFrames;  /* hide unselected frames flag */
                            /* ***N.B. no frames hidden by this flag if no */
                            /*         frames are selected */
                            /* ***N.B. This also suppresses display of the */
                            /*         "Current Frame" (dashed magenta lines) */
                            /*         AT ALL TIMES */
int _litframes;
int _minHits4;
int _coloring;
int _linestyle;
int _shading;
double _shadingLo;
double _shadingHi;
int _scalebar;
int _textStyle;
int _metTrunc;
int _scale2frame;

int _fov100Vis;
Widget _fov100VisW;

char *_hcfilename;

#define MAXLENCOMMENT 201
#define MAXWIDCOMMENT 67
#define MAXNUMCOMMENT 10

char _comments[MAXNUMCOMMENT][MAXLENCOMMENT];
long _commentsOn;

unsigned long _numXPixels;
unsigned long _xPixels[256];
int _xPixelsRGB[3][256];


Widget _walkimgfrm_W[WALKIMGFRM_COUNT];
Widget _som_W;
Widget _halfNotTimeW;
int _walkStepSize;
int _walktype, _halfNotTimeSteps;
XtIntervalId _walk_timer_id;
IMGFRM *_matchedimgfrm;
IMGFRM *_fwdhalfimgfrm;
IMGFRM *_backhalfimgfrm;
IMGFRM *_retreatimgfrm;
UTC _retreatUtc;

#define EDITIMGFRM_FIRST    0
#define EDITIMGFRM_UNDEL    EDITIMGFRM_FIRST
#define EDITIMGFRM_DEL      1
#define EDITIMGFRM_SEL1     2
#define EDITIMGFRM_CLRSEL   3
#define EDITIMGFRM_LAST     EDITIMGFRM_CLRSEL

Widget _editImgfrm_Ws[EDITIMGFRM_LAST+1];
IMGFRM *_sel1Imgfrm;
IMGFRM *_sel2Imgfrm;
IMGFRM *_delListImgfrm;
IMGFRM *_delWhereImgfrm;

Widget _jjmode_Ws[JJMODE_LAST];
Widget _jjmodeW;

#if 0
#define EDITIMGFRM_FIRST    0
#define EDITIMGFRM_UNDEL    EDITIMGFRM_FIRST
#define EDITIMGFRM_DEL      1
#define EDITIMGFRM_SEL1     2
#define EDITIMGFRM_CLRSEL   3
#define EDITIMGFRM_LAST     EDITIMGFRM_CLRSEL
#endif

Widget _selectInstr_Ws[SC_NIS2+1];
int _selectInstrActive;

} ITEM;

#define form cur_item->_form
#define instr_cascade cur_item->_instr_cascade
#define minVertsW cur_item->_minVertsW
#define colorW cur_item->_colorW
#define linestyleW cur_item->_linestyleW
#define shadeW cur_item->_shadeW
#define hideframes3W cur_item->_hideframes3W
#define hiddenW cur_item->_hiddenW
#define perspW cur_item->_perspW
#define northupW cur_item->_northupW
#define scalebarW cur_item->_scalebarW
#define textStyleW cur_item->_textStyleW
#define tryckW cur_item->_tryckW
#define scale2frameW cur_item->_scale2frameW
#define centerframeW cur_item->_centerframeW
#define app_shell cur_item->_app_shell
#define bore cur_item->_bore
#define roll cur_item->_roll
#define orbitstr cur_item->_orbitstr
#define Noraz_label cur_item->_Noraz_label
#define Noraz_W cur_item->_Noraz_W
#define rho_label cur_item->_rho_label
#define rho_W cur_item->_rho_W
#define phi_label cur_item->_phi_label
#define phi_W cur_item->_phi_W
#define theta_label cur_item->_theta_label
#define theta_W cur_item->_theta_W
#define drawing_area cur_item->_drawing_area
#define initial cur_item->_initial
#define old_Noraz cur_item->_old_Noraz
#define old_phi cur_item->_old_phi
#define old_theta cur_item->_old_theta
#define old_rho cur_item->_old_rho
#define Tx cur_item->_Tx
#define Ty cur_item->_Ty
#define dTx cur_item->_dTx
#define dTy cur_item->_dTy
#define da_width cur_item->_da_width
#define da_height cur_item->_da_height
#define da_height_pm cur_item->_da_height_pm
#define xImage cur_item->_xImage
#define lastPixel cur_item->_lastPixel
#define lastDN cur_item->_lastDN
#define rho cur_item->_rho
#define da_size cur_item->_da_size
#define saveCtr cur_item->_saveCtr
#define savePxlPerKm cur_item->_savePxlPerKm
#define R cur_item->_R
#define V cur_item->_V
#define Noraz cur_item->_Noraz
#define new_R cur_item->_new_R
#define new_dR cur_item->_new_dR
#define da_dpy cur_item->_da_dpy
#define da_win cur_item->_da_win
#define da_pm cur_item->_da_pm
#define da_pm_depth cur_item->_da_pm_depth
#define fgc cur_item->_fgc
#define bgc cur_item->_bgc
#define SHAPE_gc cur_item->_SHAPE_gc
#define SHAPE_gray_gc cur_item->_SHAPE_gray_gc
#define R_gc cur_item->_R_gc
#define G_gc cur_item->_G_gc
#define B_gc cur_item->_B_gc
#define Y_gc cur_item->_Y_gc
#define M_gc cur_item->_M_gc
#define C_gc cur_item->_C_gc
#define GO_gc cur_item->_GO_gc
#define rainbowGc cur_item->_rainbowGc
#define nrainbow cur_item->_nrainbow
#define grayGc cur_item->_grayGc
#define ngray cur_item->_ngray
#define spudv cur_item->_spudv
#define sc cur_item->_sc
#define ap cur_item->_ap
#define scOrbit cur_item->_scOrbit
#define asterOrbit cur_item->_asterOrbit
#define earthOrbit cur_item->_earthOrbit
#define imgfrm cur_item->_imgfrm
#define currentImgfrm cur_item->_currentImgfrm
#define currentImgfrmValid cur_item->_currentImgfrmValid
#define utc cur_item->_utc
#define saved_utc cur_item->_saved_utc
#define pointing_solve_ok cur_item->_pointing_solve_ok
#define year_W cur_item->_year_W
#define doy_W cur_item->_doy_W
#define hour_W cur_item->_hour_W
#define minute_W cur_item->_minute_W
#define second_W cur_item->_second_W
#define year_lab_W cur_item->_year_lab_W
#define doy_lab_W cur_item->_doy_lab_W
#define hour_lab_W cur_item->_hour_lab_W
#define minute_lab_W cur_item->_minute_lab_W
#define second_lab_W cur_item->_second_lab_W
#define utc_label cur_item->_utc_label
#define utc_Xmstr cur_item->_utc_Xmstr
#define hideframes3 cur_item->_hideframes3
#define ongoingDrag cur_item->_ongoingDrag
#define ongoingNoDrag (ongoingDrag == DRAG_NONE)
#define ongoingXlateDrag (ongoingDrag == DRAG_XLATE)
#define ongoingStretchDrag (ongoingDrag == DRAG_STRETCH)
#define ongoingTimeDrag (ongoingDrag == DRAG_TIME)
#define hideframes cur_item->_hideframes
#define currentframes cur_item->_currentframes
#define hideUnselectedFrames cur_item->_hideUnselectedFrames
#define litframes cur_item->_litframes
#define minHits4 cur_item->_minHits4
#define coloring cur_item->_coloring
#define shading cur_item->_shading
#define shadingLo cur_item->_shadingLo
#define shadingHi cur_item->_shadingHi
#define linestyle cur_item->_linestyle
#define scalebar cur_item->_scalebar
#define textStyle cur_item->_textStyle
#define metTrunc cur_item->_metTrunc
#define scale2frame cur_item->_scale2frame
#define hcfilename cur_item->_hcfilename
#define comments cur_item->_comments
#define commentsOn cur_item->_commentsOn
#define numXPixels cur_item->_numXPixels
#define xPixels cur_item->_xPixels
#define xPixelsRed cur_item->_xPixelsRGB[0]
#define xPixelsGrn cur_item->_xPixelsRGB[1]
#define xPixelsBlu cur_item->_xPixelsRGB[2]

#define fov100Vis cur_item->_fov100Vis
#define fov100VisW cur_item->_fov100VisW

#define editImgfrm_Ws cur_item->_editImgfrm_Ws
#define undelImgfrm_W editImgfrm_Ws[EDITIMGFRM_UNDEL]
#define delImgfrm_W editImgfrm_Ws[EDITIMGFRM_DEL]
#define clearSel_W editImgfrm_Ws[EDITIMGFRM_CLRSEL]
#define selImgfrm1_W editImgfrm_Ws[EDITIMGFRM_SEL1]
#define sel1Imgfrm cur_item->_sel1Imgfrm
#define sel2Imgfrm cur_item->_sel2Imgfrm
#define delListImgfrm cur_item->_delListImgfrm
#define delWhereImgfrm cur_item->_delWhereImgfrm

#define jjmode_Ws cur_item->_jjmode_Ws
#define jjmodeW cur_item->_jjmodeW
#define jjmode_mosaic_W jjmode_Ws[JJMODE_MOSAIC]
#define jjmode_orbit_W jjmode_Ws[JJMODE_ORBIT]
#define jjmode sc->_jjmode

#define walkimgfrm_W cur_item->_walkimgfrm_W
#define walktype cur_item->_walktype
#define halfNotTimeSteps cur_item->_halfNotTimeSteps
#define halfNotTimeW cur_item->_halfNotTimeW
#define walk_timer_id cur_item->_walk_timer_id
#define first_W walkimgfrm_W[WALKIMGFRM_FIRST]
#define backhalf_W walkimgfrm_W[WALKIMGFRM_BACKHALF]
#define backtime_W walkimgfrm_W[WALKIMGFRM_BACKTIME]
#define back1_W walkimgfrm_W[WALKIMGFRM_BACK1]
#define fwd1_W walkimgfrm_W[WALKIMGFRM_FWD1]
#define fwdhalf_W walkimgfrm_W[WALKIMGFRM_FWDHALF]
#define fwdtime_W walkimgfrm_W[WALKIMGFRM_FWDTIME]
#define last_W walkimgfrm_W[WALKIMGFRM_LAST]
#define matchedimgfrm cur_item->_matchedimgfrm
#define fwdhalfimgfrm cur_item->_fwdhalfimgfrm
#define backhalfimgfrm cur_item->_backhalfimgfrm
#define retreatimgfrm cur_item->_retreatimgfrm
#define retreatUtc cur_item->_retreatUtc

#define som_W cur_item->_som_W
#define walkStepSize cur_item->_walkStepSize

#define selectInstr_Ws cur_item->_selectInstr_Ws
#define selectNIS_W cur_item->_selectInstr_Ws[SC_NIS]
#define selectMSI_W cur_item->_selectInstr_Ws[SC_MSI]
#define selectNIS2_W cur_item->_selectInstr_Ws[SC_NIS2]
#define selectInstrActive cur_item->_selectInstrActive

#define spudf spudv->_spudf
#define VR spudv->_VR
#define SR spudv->_SR
#define segstat spudv->_segstat
#define vstat spudv->_vstat
#define pstat spudv->_pstat
#define xyz spudv->_xyz
#define hidden spudv->_hidden
#define persp spudv->_persp
#define centerframe spudv->_track_imager_frame
#define display_northup spudv->_display_northup

#define b0 sc->_b0
#define linedir sc->_linedir
#define sampdir sc->_sampdir
#define vb0 sc->_vb0
#define rb0 sc->_rb0
#define babf sc->_babf
#define vbabf sc->_vbabf
#define rbabf sc->_rbabf
#define scFromAstAbf sc->_scFromAstAbf
#define sunFromAstAbf sc->_sunFromAstAbf
#define sunFromScAbf sc->_sunFromScAbf
#define earthFromScAbf earth->_earthFromScAbf
#define asterad sc->_asterad
#define maxrad sc->_maxrad
#define fovy sc->_fovy
#define fovx sc->_fovx
#define camvert sc->_camvert
#define camabf sc->_camabf

#define scid sc->_scid
#define asterid sc->_asterid
#define sunid sc->_sunid
#define earthid sc->_earthid
#define msctoj2k sc->_msctoj2k
#define mj2ktosc sc->_mj2ktosc
#define mj2ktoabf sc->_mj2ktoabf
#define mabftoj2k sc->_mabftoj2k
#define msctoabf sc->_msctoabf
#define mabftosc sc->_mabftosc
#define mabftocam sc->_mabftocam
#define mcamtoabf sc->_mcamtoabf

#define ba0 ap->_ba0
#define ra0 ap->_ra0

#define et utc._et
#define year utc._year
#define doy utc._doy
#define hour utc._hour
#define minute utc._minute
#define second utc._second
#define utcstr utc._utcstr

ITEM *first_item = (ITEM *) NULL;         /* start of linked list */

/************************/
Widget
orbitgui_findParentForm( Widget w) {
  if ( !w) return w;
  for ( w=XtParent(w); w; w=XtParent(w)) if ( XmIsForm(w)) break;
  return w;
}

/************************/

#define FIND_ITEM_FROM_FORM(WCHILD) \
Widget parentFormW = orbitgui_findParentForm( WCHILD); \
FIND_ITEM( parentFormW, form)

/************************/

#define FIND_ITEM( W, THISW) \
ITEM *cur_item; \
  cur_item = first_item; \
  if ( !cur_item) { \
    fprintf( stderr, "orbit:  Widget (%08lxx) not found!\n", (long) W); \
    fflush( stderr); \
    return; \
  } \
  while ( (THISW != W)) { \
    cur_item = cur_item->next_item; \
    if ( !cur_item) { \
      fprintf( stderr, "orbit:  Widget (%08lxx) not found!\n", (long) W); \
      fflush( stderr); \
      return; \
    } \
  }

/****************************/

void		display_3d();
void solveanddraw( ITEM *);
void orbitgui_setSensitivity( Widget, Widget);

SPUDV *orbit_init(SC *);

#ifdef vms
#define MAXFLOAT (1.0E+29)
char *malloc();
int		exit(), identity_matrix3d();
#else
#include <malloc.h>
#ifdef sun
#include <values.h>
#endif
#endif

#ifndef MAXFLOAT
#define MAXFLOAT FLT_MAX
#endif

void		new_phi_CB(), new_theta_CB(), new_rho_CB()
                    , new_Noraz_CB(), centered_axes_CB();
void		expose_CB(), resize_CB(), drawInput_CB();
void shadgrid_CB();
void update_phi(), set_area(), draw_image();
void exit_CB(), general_help_CB(), scale_help_CB(), opts_help_CB();
void pointing_CB(), elts_CB(), spice_CB(), new_utc_CB();
void apply_utc_CB();
void reset_utc_CB();

void orbitgui_CASmenu_CB();
void orbitgui_entergentimes_CB();

void orbit_enterxyz( void *, Widget, int, char **, char *, double *);
void orbitgui_enterpoint( void *, Widget);

unsigned long	set_color();
void orbitgui_set_rainbow( ITEM *cur_item);

static		char tmpstr[255], fmtstr[255];
static XFontStruct *fontInfo;
static double degpr;

#define CHARSET XmSTRING_DEFAULT_CHARSET

short		decimal_Noraz= 2, decimal_rho= 2, decimal_phi= 2
                    , decimal_theta= 2;
static double		pow_Noraz, pow_rho, pow_phi, pow_theta;
size_t		size_R;
Arg	args[MAX_ARGS];
int	n;

/****************************************************************************/
/* macros defining callbacks for cascade menu buttons that control integers */

/***************************/
/* - front end of callback */

#define INTCBSTART( CBNAME, WIDGET, INT) \
\
void \
CBNAME(Widget w, XtPointer client_data, XtPointer call_data) { \
Widget pw = XtParent( w);     /* parent of button is index to find cur_item */ \
int old; \
  \
  FIND_ITEM( pw, WIDGET)                                    /* get cur_item */ \
  \
  if ( !XmToggleButtonGetState(w)) return; /* only act when toggled to true */ \
  \
  old = INT;                                              /* save old value */ \
  INT = (long) client_data;                                 /* set new value */

/**************************/
/* - back end of callback */

#define INTCBEND( INT) \
  if ( old != INT)                               /* redraw if value changed */ \
    draw_image( cur_item, Drawdev_xwin); \
 return; \
}

/*********************************/
/* - front and back end together
 *   - when nothing special need be done in between
 */

#define INTCB( CBNAME, WIDGET, INT) \
INTCBSTART( CBNAME, WIDGET, INT) \
INTCBEND( INT)

#define SETBUTTONLBL( W, L, X) \
  if ( W) { \
    X = XmStringCreateLocalized(L); \
    XtVaSetValues( W, XmNlabelString, (void *) X, NULL); \
    XmSF(X); \
  }

/*****************************************************************/
/* control action of button Fwd/Back Half/Time buttons
 * change labels on Fwd Half/Back Half buttons
 */
INTCBSTART( halfNotTime_CB, halfNotTimeW, halfNotTimeSteps )
  switch ( halfNotTimeSteps) {
  XmString xmstr;
  case 0:
    SETBUTTONLBL( backhalf_W, "Back Time", xmstr)
    SETBUTTONLBL( fwdhalf_W, "Fwd Time", xmstr)
    break;
  default:
    SETBUTTONLBL( backhalf_W, "Back Half", xmstr)
    SETBUTTONLBL( fwdhalf_W, "Fwd Half", xmstr)
    break;
  }
INTCBEND( old)   /* no need to redraw */

/*****************************************************************/
/* fov points visibility button callback - set vis in between beginning & end */

INTCBSTART( fov100Vis_CB, fov100VisW, fov100Vis)
  switch ( fov100Vis) {
  case FOV100_VISIBLE: DOFOV100_SETGLOBAL( sc); break;
  case FOV100_INVISIBLE: DOFOV100_CLRGLOBAL( sc); break;
  }
INTCBEND( fov100Vis)

/*****************************************************************/
/* linestyle button callback - set GC in between beginning & end */

INTCBSTART( shape_linestyle_CB, linestyleW, linestyle)
  switch ( linestyle) {
  case LINESTYLE_WHITE: SHAPE_gc = fgc; break;
  case LINESTYLE_GRAY: SHAPE_gc = SHAPE_gray_gc; break;
  default: break;
  }
INTCBEND( linestyle)

/*************************************************/
/* hideframes3 button callback 
 * - extract hideframes, currentframes & litframes
 */

INTCBSTART( hideframes3_CB, hideframes3W, hideframes3)
  hideframes = (0 != (hideframes3 & HIDEFRMIF_FACEAWAY)) ? 1 : 0;
  currentframes = (0 != (hideframes3 & HIDEFRMIF_GT1ORBIT)) ? 1 : 0;
  litframes = (0 != (hideframes3 & HIDEFRMIF_UNLIT)) ? 1 : 0;
INTCBEND( hideframes3)

/*************************************************/

INTCBSTART( northup_CB, northupW, display_northup)
  /* only act if state changed */
  if ( display_northup != old) {
  int sav_Noraz, new_Noraz;
  int oldnorthup = old;

    if ( display_northup) {
      /* northup state changed from off to on:
       * - set noraz to 0
       * - clear northup state to enable action in update_phi
       *   - see update_phi()
       */
      sav_Noraz = old_Noraz;
      new_Noraz = 0;
      display_northup = 0;
      XtSetSensitive( Noraz_W, False);
    } else {

      /* northup state changed from on to off:
       * - restore noraz from scalebar scale value, set old_Noraz to 0 so it
       *   will trigger draw_image() in update_phi if necessary
       *   - see update_phi()
       */
      XtSetSensitive( Noraz_W, True);
      XmScaleGetValue(Noraz_W, &new_Noraz);
      old_Noraz = 0;
    }

    update_phi(old_phi, old_theta, new_Noraz, cur_item);

    /* restore northup state & old_Noraz in off-to-on case
     * where we changed it to fool update_phi
     * - see update_phi()
     */
    if ( !oldnorthup) {
      display_northup = 1;
      old_Noraz = sav_Noraz;
    }
    old = display_northup;
  }
INTCBEND( display_northup)

/*************************************************/

/* shading, coloring and min vertices callbacks */

INTCB( hidden_CB, hiddenW, hidden)
INTCB( shade_CB, shadeW, shading)
INTCB( color_CB, colorW, coloring)
INTCB( minVerts_CB, minVertsW, minHits4)
INTCB( jjmode_CB, jjmodeW, jjmode)
INTCB( scalebar_CB, scalebarW, scalebar)
INTCB( textStyle_CB, textStyleW, textStyle)
INTCB( tryck_CB, tryckW, sc->_tryck)
INTCB( scale2frame_CB, scale2frameW, scale2frame)
INTCB( centerframe_CB, centerframeW, centerframe)

/**********************************************************/

void
hideUnselectedFrames_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
ITEM *cur_item = (ITEM *) client_data;
int old=hideUnselectedFrames;

  hideUnselectedFrames = (XmToggleButtonGetState(w)) ? 1 : 0;

  if ( old != hideUnselectedFrames)                      /* redraw if changed */
    draw_image( cur_item, Drawdev_xwin);

  return;
}

/**********************************************************/

void
point4_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
void *ptr[4];
ITEM *cur_item = (ITEM *) client_data;
int i=0;

  ptr[i++] = &bore.aimpt;
  ptr[i++] = &bore.scvec;
  ptr[i++] = &roll.aimpt;
  ptr[i++] = &roll.scvec;
  orbitgui_enterpoint( (void *) cur_item, w);

  return;
}

/**********************************************************/
void
recenter_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
ITEM *cur_item = (ITEM *) client_data;

  Tx = Ty = 0;
  draw_image( cur_item, Drawdev_xwin);
}

/**********************************************************/
/* Widget change instrument */
void
orbitgui_select_instrument( Widget w, ITEM *cur_item, fortint instrument)
{
  if ( !cur_item) return;
  if ( w) if ( !XmToggleButtonGetState(w)) return;

  orbit_set_instrument( sc, instrument);
  if ( w) solveanddraw( cur_item);
  return;
}

void
orbitgui_selectMSI_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
ITEM *cur_item = (ITEM *) client_data;
  if ( selectInstrActive) return;
  orbitgui_select_instrument( w, cur_item, SC_MSI);
  return;
}

/* Make note of the closing of the NIS mirror positioning menus */
void
orbitgui_selnis_done( SC *scArg)
{
ITEM *cur_item = (ITEM *)scArg->_cur_item;
  selectInstrActive = 0;
  XtSetSensitive( instr_cascade, 1);
  return;
}

/* Select one of the NIS instrument widths */
void
orbitgui_selnis_act( SC *scArg, fortint stepnoArg, fortint instrArg)
{
ITEM *cur_item = (ITEM *)scArg->_cur_item;
  sc->_nisStepAct = stepnoArg;
  orbit_set_instrument( sc, instrArg);
  solveanddraw( cur_item);
  return;
}

void orbit_selnis_menu( SC *, Widget, fortint);

#define OGSELINSTR_CB( NAME, SC_INSTR) \
void \
NAME(Widget w, XtPointer client_data, XtPointer call_data) \
{ \
ITEM *cur_item = (ITEM *) client_data; \
XmToggleButtonCallbackStruct *xtcbs=(XmToggleButtonCallbackStruct *)call_data; \
  if ( selectInstrActive) return; \
  if (w) if ( !XmToggleButtonGetState(w)) return; \
  selectInstrActive = 1; \
  XtSetSensitive( instr_cascade, 0); \
  orbit_selnis_menu( sc, app_shell, (fortint) SC_INSTR); \
  return; \
}

OGSELINSTR_CB( orbitgui_selectNIS_CB, SC_NIS)
OGSELINSTR_CB( orbitgui_selectNIS2_CB, SC_NIS2)

/***********************************************************/
void
orbitgui_set_utc(ITEM *cur_item) {
char fmtstr[40], tmpstr[40];
XmString tmpxmstr;

  XmScaleSetValue( year_W, year);
  XmScaleSetValue( doy_W, doy);
  XmScaleSetValue( hour_W, hour);
  XmScaleSetValue( minute_W, minute);
  XmScaleSetValue( second_W, second);

  UTCLOAD( utc);

  MAKEUTCXMSTR( utcstr, utc_Xmstr);
  XtVaSetValues( utc_label, XmNlabelString, utc_Xmstr, NULL);
  XmSF(utc_Xmstr);

#define SETLABVAL( WIDGET, UTCVAL, NDIG) \
  sprintf( fmtstr, "%%0%dd", NDIG); \
  strcpy(fmtstr,"    "); sprintf( fmtstr+4-NDIG, "%%0%dd", NDIG); \
  sprintf( tmpstr, fmtstr, UTCVAL); \
  tmpxmstr =  XmStringCreateLtoR( tmpstr, CHARSET); \
  XtVaSetValues( WIDGET, XmNlabelString, tmpxmstr, NULL); \
  XmSF(tmpxmstr)

  SETLABVAL( year_lab_W, year, 4);
  SETLABVAL( doy_lab_W, doy, 3);
  SETLABVAL( hour_lab_W, hour, 2);
  SETLABVAL( minute_lab_W, minute, 2);
  SETLABVAL( second_lab_W, second, 2);
  return;
}

/**********************************************************/
/* return bore and roll structures */
void
orbitgui_update_BoreRoll( SC *inpSc)
{
ITEM *cur_item = (ITEM *) inpSc->_cur_item;

#define IWOFF( W) \
    if (XmToggleButtonGetState(W) ) XmToggleButtonSetState( W, False, False)

#define CASEIWON( I, W) \
  case I: \
    if (!XmToggleButtonGetState(W) ) XmToggleButtonSetState( W, True, False); \
    break

#define UPDATEPTG( BRAS, I1, I2, I3, I4, I5, I6, W1, W2, W3, W4, W5, W6) \
  IWOFF( BRAS.W1); IWOFF( BRAS.W2); IWOFF( BRAS.W3); \
  IWOFF( BRAS.W4); IWOFF( BRAS.W5); IWOFF( BRAS.W6); \
  switch ( BRAS.type) { \
  CASEIWON( I1, BRAS.W1); CASEIWON( I2, BRAS.W2); CASEIWON( I3, BRAS.W3); \
  CASEIWON( I4, BRAS.W4); CASEIWON( I5, BRAS.W5); CASEIWON( I6, BRAS.W6); \
  }

  UPDATEPTG( bore.aimpt, Iabf, Iaci, Inadir, Ij2k, Ieci, Isci
	   , abf, aci, nadir, j2k, eci, sci)
  UPDATEPTG( bore.scvec, Iinstr, Ipanel, Ix, Iy, Iz, Iuser
	   , instr, panel, x, y, z, user)

  UPDATEPTG( roll.aimpt, Iabf, Iaci, Inadir, Ij2k, Ieci, Isci
	   , abf, aci, nadir, j2k, eci, sci)
  UPDATEPTG( roll.scvec, Iinstr, Ipanel, Ix, Iy, Iz, Iuser
	   , instr, panel, x, y, z, user)

  return;
}

/**********************************************************/
/* return orbit structure given cur_item */
void
orbitgui_return_scorbit_ci( void *ci, ORBIT **scorbitPtr)
{
ITEM *cur_item = (ITEM *) ci;
  *scorbitPtr = &scOrbit;
  return;
}

/**********************************************************/
/* return current utc structures given cur_item */
void
orbitgui_return_utc_ci( void *ci, UTC *outUtc)
{
ITEM *cur_item = (ITEM *) ci;
  *outUtc = utc;
  return;
}

/**********************************************************/
/* return bore and roll structures given cur_item */
void
orbitgui_return_boreroll_ci( void *ci, POINTING **inpBore, POINTING **inpRoll)
{
ITEM *cur_item = (ITEM *) ci;
  *inpBore = &bore;
  *inpRoll = &roll;
  return;
}

/**********************************************************/
/* add comment to cur_item->_comments */

void
orbitgui_add_comment_sc( SC *inpSc, int commentNum, char* inpComment)
{
ITEM *cur_item = (ITEM *) inpSc->_cur_item;

  if ( commentNum < 1 || commentNum > MAXNUMCOMMENT) return;
  commentsOn = 1;
  strncpy( comments[commentNum-1], inpComment, MAXLENCOMMENT);
  comments[commentNum-1][MAXLENCOMMENT-1] = '\0';

  return;
}

/**********************************************************/
/* return bore and roll structures given SC structure */
void
orbitgui_return_boreroll( SC *inpSc, POINTING **inpBore, POINTING **inpRoll)
{
ITEM *cur_item = (ITEM *) inpSc->_cur_item;
  *inpBore = &bore;
  *inpRoll = &roll;
  return;
}

/*********************************************/
/* return spudv structure given SC structure */
void
orbitgui_return_spudv_sc( SC *inpSc, SPUDV **spudvPtr)
{
ITEM *cur_item = (ITEM *) inpSc->_cur_item;
  *spudvPtr = spudv;
  return;
}

/**********************************************************/
/* edit frames */
void
orbitgui_edit_frames_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
ITEM *cur_item = (ITEM *) client_data;
IMGFRM *lclif1, *lclif2;
int i, iedittype;

  if ( !cur_item) return;
  if ( !imgfrm) return;

  for ( i=EDITIMGFRM_FIRST, iedittype = -1;
        i <= EDITIMGFRM_LAST && iedittype == -1; i++) {
    if ( w == editImgfrm_Ws[i]) iedittype = i;
  }

  if ( iedittype == -1) {
    fprintf( stderr, "Huh? Code WSNBATGH-orbitgui_edit_frames_CB(1)\n");
    fflush( stderr);
    return;
  }

  switch ( iedittype) {
  case EDITIMGFRM_SEL1:
    if ( !sel1Imgfrm) {
      sel1Imgfrm = sel2Imgfrm = matchedimgfrm;
    } else {
      for ( lclif1=sel1Imgfrm; lclif1 && lclif1 != matchedimgfrm; ) {
        lclif1 = lclif1->nextif;
      }
      if ( lclif1) {
        sel2Imgfrm = matchedimgfrm;
      } else {
        sel2Imgfrm = sel1Imgfrm;
        sel1Imgfrm = matchedimgfrm;
      }
    }
    break;
  case EDITIMGFRM_CLRSEL:
    if ( sel1Imgfrm) {
      sel1Imgfrm = sel2Imgfrm = (IMGFRM *) 0;
    }
    break;
  case EDITIMGFRM_DEL:
    if ( sel1Imgfrm && sel2Imgfrm) {
      /* irrevocably free previous deleted list */
      if ( delListImgfrm) free_imgfrm_list( &delListImgfrm);
      /* save start of deleted list */
      delListImgfrm = sel1Imgfrm;

      /* check whether partial or whole list is selected */
      if ( sel1Imgfrm->previf != sel2Imgfrm) {

        /* partial deletion */

        if ( sel1Imgfrm == imgfrm) {
          delWhereImgfrm = (IMGFRM *) 0;
          imgfrm = sel2Imgfrm->nextif;
          imgfrm->previf = sel1Imgfrm->previf;

        } else {
          delWhereImgfrm = sel1Imgfrm->previf;
          delWhereImgfrm->nextif = sel2Imgfrm->nextif; /* reconnect fwd link */
          if ( sel2Imgfrm->nextif) {
            delWhereImgfrm->nextif->previf = delWhereImgfrm; /*recon back link*/
          } else {
            imgfrm->previf = delWhereImgfrm;
          }
        }

      /* sel2 == sel1->previf => delete all */

      } else {
        delWhereImgfrm = imgfrm = (IMGFRM *) 0;
      }

      fwdhalfimgfrm = backhalfimgfrm = matchedimgfrm = (IMGFRM *) 0;

      /* make sel1Imgfrm through sel2Imgfrm a self-consistent list */

      sel1Imgfrm->previf = sel2Imgfrm;
      sel2Imgfrm->nextif = (IMGFRM *) 0;      /* put end on deleted list */
      sel2Imgfrm = sel1Imgfrm = (IMGFRM *) 0; /* clear selections */

    } else {
      return;
    }
    break;
  case EDITIMGFRM_UNDEL:
    if ( !delListImgfrm) break;
    sel1Imgfrm = delListImgfrm;
    sel2Imgfrm = sel1Imgfrm->previf;
    if ( delWhereImgfrm) {
      if ( delWhereImgfrm->nextif)
        delWhereImgfrm->nextif->previf = sel2Imgfrm;
      else
        imgfrm->previf = sel2Imgfrm;

      sel2Imgfrm->nextif = delWhereImgfrm->nextif;
      sel1Imgfrm->previf = delWhereImgfrm;
      delWhereImgfrm->nextif = delListImgfrm;
    } else {
      sel2Imgfrm->nextif = imgfrm;
      if ( imgfrm) {
        sel1Imgfrm->previf = imgfrm->previf;
        imgfrm->previf = sel2Imgfrm;
      }
      imgfrm = sel1Imgfrm;
    }
    delListImgfrm = delWhereImgfrm = (IMGFRM *) 0;
    break;
  } /* switch */

  draw_image( cur_item, Drawdev_xwin);

  return;
}

/**********************************************************/
/* get walk type from widget - returns WALKIMGFRM_COUNT if no widget matched */
int
orbitgui_getWalkTypeFromW( ITEM *cur_item, Widget w) {
int iWalkType;

  for ( iWalkType=WALKIMGFRM_FIRST; iWalkType < WALKIMGFRM_COUNT; ++iWalkType) {
    if (w == walkimgfrm_W[iWalkType]) break;
  }

  /* swap -HALF with -TIME depending on halfNotTimeSteps */

# define CASE2( HALF, TIME) \
  case HALF: if ( !halfNotTimeSteps) iWalkType = TIME; break; \
  case TIME: if ( halfNotTimeSteps) iWalkType = HALF; break

  switch (iWalkType) {
  CASE2( WALKIMGFRM_FWDHALF, WALKIMGFRM_FWDTIME);
  CASE2( WALKIMGFRM_BACKHALF, WALKIMGFRM_BACKTIME);
  default: break;
  }

  return iWalkType;
}

/**********************************************************/
/* move along frames */
void
orbitgui_walk_frames( ITEM *cur_item, int iwalktype) {
IMGFRM *chosenimgfrm, *lclif1, *lclif2;
int updatedSel = 0;
int i;

  switch ( iwalktype) {
  case WALKIMGFRM_LAST:
    if ( !imgfrm) return;
    chosenimgfrm = imgfrm->previf;
    fwdhalfimgfrm = imgfrm->previf;
    backhalfimgfrm = imgfrm;
    if ( chosenimgfrm && hideUnselectedFrames && sel1Imgfrm) {
      while ( sel2Imgfrm->nextif) {    /* move selected frames to end of list */
        sel1Imgfrm = sel1Imgfrm->nextif;
        sel2Imgfrm = sel2Imgfrm->nextif;
        updatedSel = 1;
      }
    }
    break;
  case WALKIMGFRM_FIRST:
    if ( !imgfrm) return;
    chosenimgfrm = imgfrm;
    fwdhalfimgfrm = imgfrm->previf;
    backhalfimgfrm = imgfrm;
    if ( chosenimgfrm && hideUnselectedFrames && sel1Imgfrm) {
      while ( sel1Imgfrm != imgfrm) { /* move sel'd frms to start " */
        sel1Imgfrm = sel1Imgfrm->previf;
        sel2Imgfrm = sel2Imgfrm->previf;
        updatedSel = 1;
      }
    }
    break;

  case WALKIMGFRM_FWD1:
    if ( !(chosenimgfrm=matchedimgfrm) ) return;
    for ( i=0; i<walkStepSize; ++i) {
      chosenimgfrm = chosenimgfrm->nextif;
      if ( !chosenimgfrm) chosenimgfrm = imgfrm;                /* wraparound */
    }
    fwdhalfimgfrm = imgfrm->previf;
    backhalfimgfrm = imgfrm;

    /* advance selected frame(s) but not past end of list */
    if ( chosenimgfrm && hideUnselectedFrames && sel1Imgfrm) {
      for ( i=0; i<walkStepSize && sel2Imgfrm->nextif; ++i) {
        sel1Imgfrm = sel1Imgfrm->nextif;
        sel2Imgfrm = sel2Imgfrm->nextif;
        updatedSel = 1;
      }
    }
    break;

  case WALKIMGFRM_BACK1:
    if ( !(chosenimgfrm=matchedimgfrm) ) return;
    for ( i=0; i<walkStepSize; ++i) {
      chosenimgfrm = chosenimgfrm->previf;
    }
    fwdhalfimgfrm = imgfrm->previf;
    backhalfimgfrm = imgfrm;

    /* backup selected frame(s) but not past beginning of list */
    if ( chosenimgfrm && hideUnselectedFrames && sel1Imgfrm) {
      for ( i=0; i<walkStepSize && sel1Imgfrm!=imgfrm; ++i) {
        sel1Imgfrm = sel1Imgfrm->previf;
        sel2Imgfrm = sel2Imgfrm->previf;
        updatedSel = 1;
      }
    }
    break;

#   define MOVEBYTIME( INCDEC) \
    et INCDEC 60.0 * walkStepSize;          /* move by walkStepSize minutes */ \
    orbit_et2utc( et, &utc); \
    apply_utc_CB((Widget) NULL, (XtPointer) cur_item, (XtPointer) NULL); \
    return; \
    break

  case WALKIMGFRM_FWDTIME: MOVEBYTIME( +=);                     

  case WALKIMGFRM_BACKTIME: MOVEBYTIME( -=);

  case WALKIMGFRM_FWDHALF:

    if ( !fwdhalfimgfrm || !matchedimgfrm) return;
    i=0;
    for ( lclif2=lclif1=matchedimgfrm; lclif2 != fwdhalfimgfrm && lclif2; ++i) {
      lclif1 = lclif1->nextif;
      lclif2 = lclif2->nextif;
      if ( lclif2 && lclif2 != fwdhalfimgfrm) lclif2 = lclif2->nextif;
    }
    chosenimgfrm = lclif1;
    backhalfimgfrm = matchedimgfrm;

    /* move selected frame(s) by same amount but not past end of list */

    if ( chosenimgfrm && hideUnselectedFrames && sel1Imgfrm) {
      for ( ; i && sel2Imgfrm->nextif; --i) {
        sel1Imgfrm = sel1Imgfrm->nextif;
        sel2Imgfrm = sel2Imgfrm->nextif;
        updatedSel = 1;
      }
    }
    break;

  case WALKIMGFRM_BACKHALF:

    if ( !backhalfimgfrm || !matchedimgfrm) return;
    i=0;
    for ( lclif2=lclif1=matchedimgfrm; lclif2 != backhalfimgfrm && lclif2; ++i){
      lclif1 = lclif1->previf;
      lclif2 = lclif2->previf;
      if ( lclif2 && lclif2 != backhalfimgfrm) lclif2 = lclif2->previf;
    }
    chosenimgfrm = lclif1;
    fwdhalfimgfrm = matchedimgfrm;

    /* move selected frame(s) by same amount but not past beginning of list */

    if ( chosenimgfrm && hideUnselectedFrames && sel1Imgfrm) {
      for ( ; i && sel1Imgfrm!=imgfrm; --i) {
        sel1Imgfrm = sel1Imgfrm->previf;
        sel2Imgfrm = sel2Imgfrm->previf;
        updatedSel = 1;
      }
    }
    break;
  } /* switch iwalktype */

  if ( !chosenimgfrm) {
    if ( updatedSel) draw_image( cur_item, Drawdev_xwin);
    return;
  }

  matchedimgfrm = chosenimgfrm;

  /* macro to set matchedframe to chosenframe
   * (0) set time to chosen frame - use et to update utc structure & utc sliders
   * (1) copy instrument info from image frame struct to sc struct
   * (2) update instrument toggle buttons
   *     - XmToggleButtonSetState(,,notify=True) => select<Instr>_CB callback
   *       - also => solveanddraw()
   * (3) solveanddraw() if (2) doesn't do it
   */

# define IMGFRM2SC( SC, IMGFRM) \
  et = IMGFRM->_et; \
  orbit_et2utc( et, &utc); \
  orbitgui_set_utc( cur_item); \
  saved_utc = utc; \
  copyInstrument_ImgfrmToSc( IMGFRM, SC); \
  orbit_set_instrument( SC, IMGFRM->_instrument); \
  if (!XmToggleButtonGetState(selectInstr_Ws[IMGFRM->_instrument]) ) { \
  int i_instr; \
    XmToggleButtonSetState( selectInstr_Ws[IMGFRM->_instrument], True, False); \
    for ( i_instr=SC_NIS; i_instr <= SC_NIS2; i_instr++) { \
      if ( i_instr != IMGFRM->_instrument) { \
      Widget instr_W = selectInstr_Ws[i_instr]; \
        if (XmToggleButtonGetState(instr_W)) \
          XmToggleButtonSetState( instr_W, False, False); \
      } \
    } \
  } \
  { \
  char saveUtcChar0 = *utcstr; \
    *utcstr = '\0'; /* fool solveanddraw/orbit_spkez from changing et */ \
    solveanddraw( cur_item); \
    *utcstr = saveUtcChar0; \
  }

  IMGFRM2SC( sc, matchedimgfrm)

  return;
}

/****************************************************************************/
/* orbitgui_change_walk_frames is called each time the timer expires.
 * This function is also used to initiate the timer.  The "id" represents that 
 * timer ID returned from the last call to XtAppAddTimeOut().  If id == 1,
 * the function was called from orbitgui_arm_frames_CB(), not a timeout.
 * If id == 1, this is the first timeout so make it be longer to allow
 * the user to release the button and avoid getting into the "speedy"
 * part of the timeouts.
 */
void
orbitgui_change_walk_frames( XtPointer client_data, XtIntervalId id) {
ITEM *cur_item = (ITEM *) client_data;
XtAppContext appcon =
   XtWidgetToApplicationContext( walkimgfrm_W[walktype]);

  orbitgui_walk_frames( cur_item, walktype);

  /* do timer only if enviroment variable ORBIT_NOREPEAT is not defined */

  if ( !getenv(ORBIT_NOREPEAT)) {
    walk_timer_id = XtAppAddTimeOut (appcon, id==1? 500 : 100
      , (XtTimerCallbackProc)orbitgui_change_walk_frames, cur_item);
  }
  return;
} /* orbitgui_change_walk_frames */

/**********************************************************/
/* handle arm and disarm of Fwd/Back 1/Time buttons */
/* - used to start or stop the incremental changes.
 *   When the button goes down, the reason is
 *   XmCR_ARM and the timer starts.  XmCR_DISARM disables the timer.
 */
void
orbitgui_arm_frames_CB(Widget w, XtPointer client_data, XtPointer call_data) {
ITEM *cur_item = (ITEM *) client_data;
IMGFRM *chosenimgfrm, *lclif1, *lclif2;
XmPushButtonCallbackStruct *cbs =
        (XmPushButtonCallbackStruct *) call_data;
int lclWalkType;

  if ( !cur_item) return;

  lclWalkType = orbitgui_getWalkTypeFromW( cur_item, w);

  switch( lclWalkType) {
  case WALKIMGFRM_FWD1:
  case WALKIMGFRM_BACK1:
    if ( !matchedimgfrm) return;
  case WALKIMGFRM_FWDTIME:
  case WALKIMGFRM_BACKTIME:
    break;
  case WALKIMGFRM_FWDHALF:/*return for Fwd/Back Half w/o err, we are only here*/
  case WALKIMGFRM_BACKHALF: /* because -half_W & -time_W widgets are the same */
    return;
    break;
  default:
    fprintf( stderr, "Huh? Code WSNBATGH-orbitgui_arm_frames_CB(1)\n");
    fflush( stderr);
    return;
    break;
  }

  walktype = lclWalkType;                       /* save walk type in cur_item */

  switch (cbs->reason) {

# define ORBIT_ARMDBG(S) \
  if ( getenv("ORBIT_ARMDBG") ) { \
    fprintf(stderr,"%s:  %ld = reason; %ld = id\n" \
                  , S, (long) cbs->reason, walk_timer_id); \
    fflush(stderr); \
  }

  case XmCR_ARM:                                              /* button press */
    ORBIT_ARMDBG( "arm");

    /* save the current image frame (Fwd/Back 1) or utc (Fwd/Back Time) 
     * so we can retreat if the user moves the mouse outside the button 
     * widget before releasing the mouse button
     */

    switch( walktype) {
    case WALKIMGFRM_FWD1:
    case WALKIMGFRM_BACK1:
      retreatimgfrm = matchedimgfrm;
      break;
    case WALKIMGFRM_FWDTIME:
    case WALKIMGFRM_BACKTIME:
      retreatUtc = utc;
      break;
    } /* switch walktype */

    orbitgui_change_walk_frames( client_data, 1 );   /* timer id of 1 to init */

    break;

  case XmCR_DISARM:                 /* button release with pointer off button */
    ORBIT_ARMDBG( "disarm");

    if ( !getenv(ORBIT_NOREPEAT)) {
      if ( walk_timer_id != 1) {
        XtRemoveTimeOut (walk_timer_id);
        walk_timer_id = 1;
      }
    }
    switch ( walktype) {
    case WALKIMGFRM_FWD1:         /* restore matchedimgfrm from retreatimgfrm */
    case WALKIMGFRM_BACK1:
      if ( !retreatimgfrm || (retreatimgfrm == matchedimgfrm) ) 
        return;                 /* either don't retreat or stay where you are */
      matchedimgfrm = retreatimgfrm;        /* or reset time to retreat frame */
      IMGFRM2SC( sc, matchedimgfrm)
      break;
    case WALKIMGFRM_FWDTIME:    /* restore time from retreatUtc */
    case WALKIMGFRM_BACKTIME:
      if ( utc._et == retreatUtc._et) return; /* don't retreat if times match */
      utc = retreatUtc;                      /* or reset time to retreat time */
      apply_utc_CB((Widget) NULL, (XtPointer) cur_item, (XtPointer) NULL);
      break;
    } /* switch walktype */
    break;

  case XmCR_ACTIVATE:                /* button release with pointer on button */
    ORBIT_ARMDBG( "activate");

  default:
    ORBIT_ARMDBG( "default");

    if ( !getenv(ORBIT_NOREPEAT)) {
      if ( walk_timer_id != 1) {
        XtRemoveTimeOut (walk_timer_id);
        walk_timer_id = 1;
      }
    }

    break;
  } /* switch cbs->reason */
} /* orbitgui_arm_frames_CB */

/**********************************************************/
/* Widget callback to move along frames */
void
orbitgui_walk_frames_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
ITEM *cur_item = (ITEM *) client_data;
int i, iwalktype;

  if ( !cur_item) return;

  iwalktype = orbitgui_getWalkTypeFromW( cur_item, w);

  /* requirements for each typed of walk
   * - return & don't call _walk_frames for fwd/back-1/time 
   *   - arm_frames and the Xt*Timeout do that
   * - return
   * - confirm that we are to stay with the final frame for fwd/back-1/Time
   *   by clearing retreatimgfrm & setting retreatUtc to current utc
   *   so disarm callback will not retreat
   */

  switch ( iwalktype) {
  case WALKIMGFRM_BACK1:     /* needs matchedimgfrm, but let Xt*Timeout do it */
  case WALKIMGFRM_FWD1:
    retreatimgfrm = (IMGFRM *) NULL;                   /* clear retreatimgfrm */
  case WALKIMGFRM_BACKTIME:   /* no req's for -TIME, but let Xt*Timeout do it */
  case WALKIMGFRM_FWDTIME:
    retreatUtc = utc;
    return;
    break;
  case WALKIMGFRM_BACKHALF:                            /* needs matchedimgfrm */
  case WALKIMGFRM_FWDHALF:
    if ( !matchedimgfrm) return;
    break;
  case WALKIMGFRM_FIRST:
  case WALKIMGFRM_LAST:
    if ( !imgfrm) return;                                      /* need imgfrm */
    break;
  default:
    fprintf( stderr, "Huh? Code WSNBATGH-orbitgui_walk_frames_CB(1)\n");
    fflush( stderr);
    return;
  }

  orbitgui_walk_frames( cur_item, iwalktype);

  return;
}

/**********************************************************/
/* add frames from automatic frame generation, set time to last frame */
void
add_gen_frames(ORBIT *inpOrbit, IMGFRM *inpImgfrm)
{
SC *inpSc = (SC *) inpOrbit->_sc;
ITEM *cur_item = inpSc->_cur_item;
IMGFRM **lclimgfrm;
IMGFRM *imgfrm2 = imgfrm;
IMGFRM *lclimgfrm2;
AGEN *agen;
unsigned long i;
int iSeq;
void orbit_calc_imgfrmAv( IMGFRM *);

#define REDUP(S) if (S) S = strdup(S)

  if ( inpOrbit->_agen._addGenFrames) {
    lclimgfrm = (IMGFRM **) inpOrbit->_agen._addGenFrames;
    *lclimgfrm = inpImgfrm;
    return;
  }

  if ( !inpImgfrm) return;

  orbit_calc_imgfrmAv( inpImgfrm);

  /* create AGEN structure, copy automatic frame generation info,
   * set pointer in each IMGFRM
   */
  agen = (AGEN *)malloc( sizeof(AGEN));

  if ( agen) *agen = inpOrbit->_agen;
  for ( lclimgfrm2 = inpImgfrm, i=0; lclimgfrm2; i++) {
    lclimgfrm2->_agen = agen;
    lclimgfrm2->_seqNum = i;
    lclimgfrm2 = lclimgfrm2->nextif;
  }
  if ( agen) {
    agen->_refs = i;
    if ( agen->_typeAgen == AGEN_TYPE_FRAG) {
      REDUP( agen->_fragArgs);
      for ( iSeq=0; iSeq<12; ++iSeq) { REDUP( agen->_seqDefArgs[iSeq]); }
    }
  }

  /* find last frame in current list, save as imgfrm2 */
  for ( lclimgfrm = &imgfrm; *lclimgfrm; ) {
    imgfrm2 = *lclimgfrm;
    lclimgfrm = &imgfrm2->nextif;
  }

  /* append new list of frames to existing list */
  *lclimgfrm = inpImgfrm;

  /* make back links; imgfrm->previf points to last frame */
  if ( lclimgfrm != &imgfrm) {
    imgfrm->previf = inpImgfrm->previf;
    inpImgfrm->previf = imgfrm2;
  }

  /* reset half stepping limits */
  backhalfimgfrm = imgfrm;
  matchedimgfrm = fwdhalfimgfrm = imgfrm->previf;

  /* set time to last frame - use et to update utcstr */
  IMGFRM2SC( sc, imgfrm->previf)

  return;
}

/**********************************************************/
/* Inputs for automatic frame generation */

#define GENFRAMES_CB_BASE( NAME_CB, DIALOG, PTR) \
void \
NAME_CB(Widget w, XtPointer client_data, XtPointer call_data) \
{ \
ITEM *cur_item = (ITEM *) client_data; \
void DIALOG( Widget, void *); \
 \
  DUMMYSTRCPY( (PTR)->_agen._utc, utcstr); \
  DIALOG( w, (void *) PTR); \
  return; \
}

#define GENFRAMES_CB( NAME_CB, DIALOG) \
        GENFRAMES_CB_BASE( NAME_CB, DIALOG, &scOrbit)

/* dummy strcpy sometimes needed in GENFRAMES_CB macro, sometimes not */

#define DUMMYSTRCPY( A, B) strcpy( A, B)
GENFRAMES_CB( gen_frames_inputs_CB, orbitgui_create_gen_dialog)
GENFRAMES_CB( gen_msi0_frames_inputs_CB, orbitgui_create_gen_msi0_dialog)
GENFRAMES_CB( gen_nis0_frames_inputs_CB, orbitgui_create_gen_nis0_dialog)

#undef DUMMYSTRCPY
#define DUMMYSTRCPY( A, B)
GENFRAMES_CB( select_framesout_CB, orbitgui_create_framesout_dialog)

GENFRAMES_CB_BASE( orbitguiShadingScale_CB, orbitguiCreateShadingScale_dialog
                 , cur_item)

/******************************************************/
void
orbitguiGetShading( ITEM *cur_item, double *lo, double *hi, int *resetToData) {
  *lo = shadingLo;
  *hi = shadingHi;
  *resetToData = (*lo == spudf->locolor) && (*hi == spudf->hicolor);
  return;
}

void
orbitguiSetShading( ITEM *cur_item, double lo, double hi
                  , int resetToData, int updateDisplay) {
  shadingLo = resetToData ? spudf->locolor : lo;
  shadingHi = resetToData ? spudf->hicolor : hi;
  if ( updateDisplay) {
    draw_image( cur_item, Drawdev_xwin);
  }
  return;
}

/**********************************************************/
/* Save current pointing into image frame structure */
void
mark_frame_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
ITEM *cur_item = (ITEM *) client_data;
IMGFRM **lclimgfrm;
IMGFRM *imgfrm2 = (IMGFRM *) 0;
char *instrName;

  if ( !currentImgfrmValid) return;

  for ( lclimgfrm = &imgfrm; *lclimgfrm; ) {
    imgfrm2 = *lclimgfrm;
    lclimgfrm = &imgfrm2->nextif;
  }

  *lclimgfrm = loadImageFrame( sc, et, (IMGFRM *) 0 );

  /* make back links; imgfrm->previf points to last frame
   */
  if ( *lclimgfrm ) {

    (*lclimgfrm)->previf = imgfrm2;
    imgfrm->previf = *lclimgfrm;

    copyInstrument_ScToImgfrm( sc, *lclimgfrm);

    /* reset half limits */
    backhalfimgfrm = imgfrm;
    matchedimgfrm = fwdhalfimgfrm = imgfrm->previf;
  }

  draw_image( cur_item, Drawdev_xwin);

  return;
}

/**********************************************************/
/* free imgfrm structures */
void
clear_frames_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
ITEM *cur_item = (ITEM *) client_data;

  if ( delListImgfrm) free_imgfrm_list( &delListImgfrm);

  matchedimgfrm = backhalfimgfrm = fwdhalfimgfrm = (IMGFRM *)0;
  sel1Imgfrm = sel2Imgfrm = delListImgfrm = delWhereImgfrm = (IMGFRM *)0;

  if ( imgfrm) free_imgfrm_list( &imgfrm);
  draw_image( cur_item, Drawdev_xwin);
  return;
}


/***************************************************/
/* routine used by orbit_GIFEncode via orbit_compress() */
/* - return lookup table index for a given position in xImage */

int orbitgui_getPixel( int x, int y, void *ci) {
ITEM *cur_item = (ITEM *) ci;
int lo = 0;
int hi = numXPixels - 1;
int i = (hi+lo) / 2;
unsigned long ipix = XGetPixel( xImage, x, y);

  if ( ipix == lastPixel) return lastDN;

  lastPixel = ipix;

  if ( ipix <= xPixels[lo]) { return (lastDN=lo); }
  if ( ipix >= xPixels[hi]) { return (lastDN=hi); }

  while ( (hi-lo) > 0) {
    if ( xPixels[i] == ipix) break;
    if ( xPixels[i] < ipix) { lo = i; i = (lo+hi+1) / 2; }
    else if ( ipix < xPixels[i]) { hi = i; i = (lo+hi) / 2; }
  }

  return (lastDN=i);
}

#define SUMPXL(I) (xPixelsRed[I] + xPixelsGrn[I] + xPixelsBlu[I])

/**********************************************************/
/* draw da_pm to a GIF file */

void
draw_image_gif( ITEM *cur_item, int drawdev) {
FILE *giffile;
int gw = da_width;
int gh = commentsOn ? da_height_pm : da_height;
int gi = 0;   /* interlace */
int bg = 0;   /* background */
int bgSum = SUMPXL(bg);
int bpp = 8;  /* bits per pixel */
int i, isum;
unsigned long plane_mask;
char *savHCF = hcfilename;
#define DIGRTN hcfilename = savHCF; return
long cou = 0;


  if ( drawdev == Drawdev_gifs) {
  long numlen;
  char *ptr;
    if ( !imgfrm || !matchedimgfrm) {
      perror( "draw_image_gifS:  no IMGFRM");
      DIGRTN;
    }
    if ( !(ptr=strrchr( savHCF, '%')) ) {
      perror( "draw_image_gifS:  no %");
      DIGRTN;
    }
    ptr++;
    if ( 1 != sscanf( ptr, "%ld", &numlen)) {
      perror( "draw_image_gifS:  no number after %");
      DIGRTN;
    }
    if ( numlen < 0 || numlen > 19 ) {
      perror( "draw_image_gifS:  bad field length");
      DIGRTN;
    }
    if ( !(hcfilename = (char *) malloc( strlen(savHCF) + 20)) ) {
      perror( "draw_image_gifS:  malloc failure");
      DIGRTN;
    }

    sprintf( hcfilename, savHCF, cou++);
  }

  draw_image( cur_item, Drawdev_xwin);       /* ensure da_pm is up to date */

  while ( 1) {
  IMGFRM *savIF, *stepIF;

    for ( i=bg+1; i<numXPixels; ++i) {  /* use ~darkest pixel for background */
      if ( SUMPXL(i) < bgSum) {
        bg = i;
        bgSum = SUMPXL(i);
      }
    }
  
    for ( plane_mask=i=0; i<da_pm_depth; ++i) plane_mask |= (1L<<i);
  
    xImage = XGetImage( da_dpy, da_pm, (int) 0, (int) 0
                      , (unsigned int) da_width, (unsigned int) da_height_pm
                      , plane_mask, XYPixmap);
  
    if ( !xImage) { DIGRTN; }
    if ( !(giffile=fopen( hcfilename, "w"))) { DIGRTN; }
  
    fprintf( stderr, "Saving %dx%d image to %s ...", gw, gh, hcfilename);
    fflush( stderr);
  
    GIFEncode( giffile, gw, gh, gi, bg, bpp, xPixelsRed, xPixelsGrn, xPixelsBlu
             , orbitgui_getPixel, (void *) cur_item);
  
    fclose( giffile);
    fprintf( stderr, " done\n");
    fflush( stderr);
    XDestroyImage( xImage);

    if ( drawdev != Drawdev_gifs) break;

    /* walk to next imgfrm */

    savIF = matchedimgfrm;
    orbitgui_walk_frames( cur_item, WALKIMGFRM_FWD1);  /* does draw_image */

    /* test if we walked past the end */

    while ( (savIF != matchedimgfrm) && savIF) savIF = savIF->nextif;
    if ( !savIF) break;

    sprintf( hcfilename, savHCF, cou++);
  }

  DIGRTN;
}

/**********************************************************/
/* callback to write this view's graphics to a hardcopy file */

#define WRITE_VIEW( CB, DD) WRITE_VIEW_BASE( CB, DD, draw_image)

#define WRITE_VIEW_BASE( CBNAME, DRAWDEV, DRAW_IMAGE) \
void \
CBNAME(Widget w, XtPointer client_data, XtPointer call_data) { \
ITEM *cur_item = (ITEM *) client_data; \
Widget fsdShell = w; \
char *check_writeable(); \
  \
  hcfilename = check_writeable( w, client_data, call_data); \
  if ( !hcfilename) return; \
  \
  while ( fsdShell && !XtIsShell(fsdShell)) fsdShell = XtParent( fsdShell); \
  if ( fsdShell) orbitgui_watchCursor( fsdShell, True); \
  orbitgui_watchCursor( app_shell, True); \
  DRAW_IMAGE( cur_item, DRAWDEV); \
  orbitgui_watchCursor( app_shell, False); \
  if ( fsdShell) orbitgui_watchCursor( fsdShell, False); \
  XtFree( hcfilename); \
  MyDW( w); \
  return; \
}

void MyDW( w) Widget w; {
  DPR1( "In MyDW //");
  XtDestroyWidget(w);
  return;
}

/**********************************************************/
/* Postscript & Xfig callbacks */

WRITE_VIEW( write_view_ps_CB, Drawdev_ps)
WRITE_VIEW( write_view_xfig_CB, Drawdev_xfig)

/**********************************************************/
/* GIF callbackss */

WRITE_VIEW_BASE( write_view_gif_CB, Drawdev_gif, draw_image_gif)
WRITE_VIEW_BASE( write_view_gifs_CB, Drawdev_gifs, draw_image_gif)

/**********************************************************/
/* callback to save imgfrm structures to file */
void
write_frames_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
ITEM *cur_item = (ITEM *) client_data;

char *check_writeable();
void write_imgfrm_list();
char *filnam;
IMGFRM *lclimgfrm;
IMGFRM *saveImgfrm = (IMGFRM *) 0;

  filnam = check_writeable( w, client_data, call_data);
  if ( !filnam) return;

  /* make sel1Imgfrm to sel2Imgfrm be a null-terminated linked list */

  if ( sel1Imgfrm && hideUnselectedFrames) {
    lclimgfrm = sel1Imgfrm;
    saveImgfrm = sel2Imgfrm->nextif;           /* save pointer at end of list */
    sel2Imgfrm->nextif = (IMGFRM *) 0;           /* sel2Imgfrm is end of list */
  } else lclimgfrm = imgfrm;

  write_imgfrm_list( lclimgfrm, filnam, spudf->eastlon, litframes
                   , minHits4 , scOrbit._framesoutBits, spudf);

  /* restore list */

  if ( sel1Imgfrm && hideUnselectedFrames) {
    sel2Imgfrm->nextif = saveImgfrm;        /* restore pointer at end of list */
  }

  XtFree( filnam);
  MyDW( w);
  return;
}

/**********************************************************/
/* file selection dialog callback to do some action with a file */

#define FILESELDLG_PART1( CBNAME, ISREAD) \
void \
CBNAME(Widget w, XtPointer client_data, XtPointer call_data) \
{ \
ITEM *cur_item = (ITEM *) client_data; \
char *filename; \
 \
char *check_writeable(); \
char *check_readable(); \
 \
  filename = ISREAD ? check_readable( w, client_data, call_data) \
                      : check_writeable( w, client_data, call_data); \
  if ( !filename) return;

/* action to perform using filename
 * e.g.
 *         do_something( cur_item, filename);
 *         draw_image( cur_item, Drawdev_xwin);
 */

#define FILSELDLG_PART2( FNNAME, LABELTEXT, PATTERN, ISREAD, CBNAME, KEEPUP) \
  if ( filename) XtFree( filename); \
  filename = (char *) 0; \
  if ( !(KEEPUP)) { MyDW( w); } \
  return; \
} \
 \
/**********************************************************/ \
/* setup to bring up file sel dialog */ \
 \
void \
FNNAME(w, client_data, call_data) \
Widget	w; \
XtPointer	client_data; \
XtPointer	call_data; \
{ \
ITEM *cur_item = (ITEM *) client_data; \
Widget dialog; \
void do_writeable_search(); \
void do_readable_search(); \
XmString title_string= NULL; \
XmString pattern_string= NULL; \
 \
  title_string=XmStringCreateLtoR(LABELTEXT, CHARSET); \
  pattern_string=XmStringCreateLtoR(PATTERN, CHARSET); \
  n = 0; \
  XtSetArg (args[n], XmNpattern, pattern_string); n++; \
  XtSetArg (args[n], XmNdialogTitle, title_string); n++; \
  XtSetArg (args[n], XmNfileSearchProc, (ISREAD ? do_readable_search \
                                                : do_writeable_search)); n++; \
  dialog = XmCreateFileSelectionDialog (app_shell, "Files", args, n); \
  XtSetSensitive ( \
      XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False); \
  XtAddCallback (dialog, XmNokCallback, CBNAME, client_data); \
  XtAddCallback (dialog, XmNcancelCallback, MyDW, NULL); \
 \
  XtManageChild (dialog); \
 \
  XmSF( title_string); \
  XmSF( pattern_string); \
 \
  return; \
}

/* putenv() is more universal than setenv, but I like the arguments of 
 * setenv() & the option whether or not to overwrite better
 */
int
orbitgui_setenv( char *envname, char *envval, int overwrite) {
char *oldval = (char *) 0;
char *eev = malloc( strlen(envname) + strlen(envval) + 2);    /* "NAME=VAL\0" */
int rtnval = 0; /* assume success */

  if ( !overwrite) oldval = getenv( envname);
  if ( !oldval) {
    sprintf( eev, "%s=%s", envname, envval);
    rtnval = putenv( eev);
    if ( rtnval != 0) {
      fprintf( stderr, "***orbitgui_setenv( %s, %s, %d) putenv(%s) failed\n"
                     , envname, envval, overwrite, eev);
      fflush( stderr);
    }
    free( eev);
  }
  return rtnval;
}

/***************************************************************/
/* orbitgui_newSpudrFSD & orbitgui_newSpudrFSD_CB
 * - load new spudr from plate model file
 */

#define ERRRTN( A) \
  fprintf A; fflush( stderr); if ( filename) XtFree( filename); return;

FILESELDLG_PART1( orbitgui_newSpudrFSD_CB, 1)

  {
  SPUDR tmpSpudr;
  SPUDV *tmpSpudv;
  SPUDF *tmpSpudf;
  int se;
  Widget fsdShell = w;
 
    while ( fsdShell && !XtIsShell(fsdShell)) fsdShell = XtParent( fsdShell);
    if ( fsdShell) orbitgui_watchCursor( fsdShell, True);
    orbitgui_watchCursor( app_shell, True);

    tmpSpudr.nlatR = SPUDRNLAT; 
    tmpSpudr.nlonR = SPUDRNLON; 
    tmpSpudr.eastlon = 
      getviewByname( tmpSpudr.Rmodel[0], &tmpSpudr.nlatR, &tmpSpudr.nlonR
                   , filename, (char **) 0);

    orbitgui_watchCursor( app_shell, False);
    if ( fsdShell) orbitgui_watchCursor( fsdShell, False);

    if ( tmpSpudr.eastlon == -1) {
      ERRRTN(( stderr, "***orbitgui_NEWSpudrFSD_CB:  getview failed\n"));
    }
    if ( !tmpSpudr.eastlon) tmpSpudr.eastlon = -1;
    if ( !(tmpSpudf = (SPUDF *) malloc( sizeof(SPUDF))) ) {
      ERRRTN(( stderr, "***orbitgui_NEWSpudrFSD_CB:  %s\n"
                     , "Failed to allocate space for face structure\n"));
    }
    rmod2face( &tmpSpudr, tmpSpudf);

    tmpSpudv = newspudv( tmpSpudf);
    if ( tmpSpudv) {
    SPUDF *oldSpudf = spudf;

      /* save settings */

      tmpSpudv->_hidden = hidden;
      tmpSpudv->_track_imager_frame = centerframe;
      tmpSpudv->_display_northup = display_northup;

      /* free old structures */

      spud_freeSpudv( spudv);
      spud_freeSpudf( oldSpudf, 1); 

      /* load new structure */

      spudv = tmpSpudv;

      /* set default scaling to data range */
      orbitguiSetShading( cur_item, 0.0, 1.0, (int) 1, (int) 1);

    } else {
      spud_freeSpudf( tmpSpudf, 1);
      ERRRTN(( stderr, "***orbitgui_newSpudrFSD_CB:  newspudv() failed\n"));
    }
  }
  draw_image( cur_item, Drawdev_xwin);

FILSELDLG_PART2( orbitgui_newSpudrFSD, "Read SPUD model File Selection"
               , "view*", 1, orbitgui_newSpudrFSD_CB, 0)

/***************************************************************/
/* orbitgui_newSpudvFSD & orbitgui_newSpudvFSD_CB
 * - load new spudv from plate model file
 */


FILESELDLG_PART1( orbitgui_newSpudvFSD_CB, 1)

  {
  SPUDV *tmpSpudv;
  SPUDF *tmpSpudf;
  int se;
  Widget fsdShell = w;
 
    while ( fsdShell && !XtIsShell(fsdShell)) fsdShell = XtParent( fsdShell);
    if ( fsdShell) orbitgui_watchCursor( fsdShell, True);
    orbitgui_watchCursor( app_shell, True);

    tmpSpudf = getplatByname( (SPUDR *) 0, filename, (char **) 0);

    orbitgui_watchCursor( app_shell, False);
    if ( fsdShell) orbitgui_watchCursor( fsdShell, False);

    if ( tmpSpudf) {
      tmpSpudv = newspudv( tmpSpudf);
      if ( tmpSpudv) {
      SPUDF *oldSpudf = spudf;

        /* save settings */

        tmpSpudv->_hidden = hidden;
        tmpSpudv->_track_imager_frame = centerframe;
        tmpSpudv->_display_northup = display_northup;

        /* free old structures */

        spud_freeSpudv( spudv);
        spud_freeSpudf( oldSpudf, 1); 

        /* load new structure */

        spudv = tmpSpudv;

        /* set default scaling to data range */
        orbitguiSetShading( cur_item, 0.0, 1.0, (int) 1, (int) 1);

      } else {
        spud_freeSpudf( tmpSpudf, 1);
        ERRRTN(( stderr, "***orbitgui_newSpudvFSD_CB:  newspudv() failed\n"));
      }
    } else {
      ERRRTN(( stderr
        , "***orbitgui_newSpudvFSD_CB:  failed to read plate model from %s\n"
        , filename));
    }
  }
  draw_image( cur_item, Drawdev_xwin);

FILSELDLG_PART2( orbitgui_newSpudvFSD, "Read PLATE model File Selection"
               , "*.pl*", 1, orbitgui_newSpudvFSD_CB, 1)

/***************************************************************/
/* orbitgui_saveVrmlFSD & orbitgui_saveVrmlFSD_CB
 * - save view as VRML
 */


FILESELDLG_PART1( orbitgui_saveVrmlFSD_CB, 0)

  {
  SPUDV *tmpSpudv;
  SPUDF *tmpSpudf;
  IMGFRM *lclimgfrm;
  Widget fsdShell = w;
  FILE *pVrml;
  char cmd[1024];
  extern char **orbit_argv;
  double *vXyz, *uvn;
  double *v0, *v1, *v2;
  VEC v01, v02, vc;
  unsigned long i, iv0, iv1, iv2, nFrmDone;
  MTX vrmlToSc;
  /* base SC frame is Y up, Z right => looking from -X
   * base VRML frame is Y up, X right => looking from +Z
   * - therefore, VRML=>SC matrix rotates vector -90 deg around Y
   * - BUT, equivalent SPICE & ORBIT matrices are transposed, so reverse 
   *   rotation = -90 = 270 => q[0] = cos(270/2) = -sqrt(2)/2
   */
  double qVrmlToSc[4] = { -sqrt(2.0)/2.0, 0.0, -sqrt(2.0)/2.0, 0.0 };
                       /* cos(h/2)        X    -Y              Z   */

    q2m( qVrmlToSc, vrmlToSc);  /* from VRML base frame to ABF base frame */

    /* open pipe - command "orbit_wrl" */
 
    sprintf( cmd, "%s_wrl - %s", *orbit_argv, filename);
    if ( !(pVrml = popen( cmd, "w"))) {
      XtFree( filename); filename = (char *) 0; return;
    }

    while ( fsdShell && !XtIsShell(fsdShell)) fsdShell = XtParent( fsdShell);
    if ( fsdShell) orbitgui_watchCursor( fsdShell, True);
    orbitgui_watchCursor( app_shell, True);

    /* header stuff (VRMLHDR becomes "#VRML V2.0 ...")
     * default Viewpoint
     * start of Shape
     */
    fprintf( pVrml, "VRMLHDR\n#include \"orbit_wrl.h\"\n");
    fprintf( pVrml, "  Shape { geometry IndexedFaceSet {\n");

    fprintf( pVrml, "    coord Coordinate { point [\n");    /* vertices's XYZ */
    for ( i=0, (vXyz=spudf->Rxyz); i<spudf->nv; ++i) {
      fprintf( pVrml, "      %17.10lg", *(vXyz++));
      fprintf( pVrml, " %17.10lg", *(vXyz++));
      fprintf( pVrml, " %17.10lg\n", *(vXyz++));
    }
    fprintf( pVrml, "    ] }\n  coordIndex [\n");


    for ( i=0, (uvn=spudf->uvnorms)    /* plates - ensure "ccw TRUE" is valid */
        ; i<spudf->nface; ++i) {
      v0 = spudf->Rxyz+(3*(iv0= spudf->faceapices[i]));
      v1 = spudf->Rxyz+(3*(iv1= spudf->oe[spudf->faceoeidx[i]]));
      v2 = spudf->Rxyz+(3*(iv2= spudf->oe[spudf->faceoeidx[i]+1]));
      VMINUS2( v1, v0, v01);
      VMINUS2( v2, v0, v02);
      vcrss( v01, v02, vc);
      if ( VDOT(vc,uvn) >= 0.0 ) {
        fprintf( pVrml, "      %ld %ld %ld -1\n", iv0, iv1, iv2);
      } else {
        fprintf( pVrml, "      %ld %ld %ld -1\n", iv0, iv2, iv1);
      }
      uvn += 3;
    }
    fprintf( pVrml, "  ] }\n");
    fprintf( pVrml, "  appearance Appearance { %s\n}\n"
                  , "material Material { diffuseColor 1 1 1 } }\n");

    /* unhidden image frames */
    nFrmDone = 0;
    for ( lclimgfrm=imgfrm; lclimgfrm; lclimgfrm = lclimgfrm->nextif) {
      if ( !lclimgfrm->_isHidden) {
      VEC uvP5, scVec, camptVec, loc, fromSun;
      double dblTmp, scale, rotat[4];
      double *camptPtr = lclimgfrm->vec3;
      int i;
      char sclkch[UTCLEN];
      char *sclkslash;
      fortint utclenm1 = UTCLEN-1;
      fortint sclkchLenout;
      MTX scToJ2k, j2kToAbf, scToAbf, vrmlToAbf;

        /* S/C & unit boresight vectors - use boresight len as shortest scale */
        VADD2( lclimgfrm->_boreVec, lclimgfrm->_boretoscVec, scVec);
        dblTmp = -1.0 / (scale=VLEN(lclimgfrm->_boretoscVec));
        VSCAL2( dblTmp, lclimgfrm->_boretoscVec, uvP5);

        /* DirectionalLight => -(sun direction, abf) */
        VADD2( lclimgfrm->_boreVec, lclimgfrm->_boretosunVec, fromSun);
        VNEG( fromSun);
        vunit( fromSun, fromSun, (double *) 0);

        /* FOV scale & offset:  
         * shortest distance along boresight to FOV vertices
         */
        for ( i=0; i<lclimgfrm->_ncampts && i<4; ++i, camptPtr += 3) {
          VMINUS2( camptPtr, scVec, camptVec);               /* S/C to vertex */
          dblTmp = VDOT( camptVec, uvP5);
          if ( dblTmp < scale) scale = dblTmp;
        }
        vmxpb( scale, uvP5, scVec, loc);    /* offset */

        /* frame rotation => S/C rotation wrt VRML
         * => [S/C rotation wrt ABF] x [ABF wrt VRML]
         */

        bodmat( &asterid, &lclimgfrm->_et, j2kToAbf);   /* SPICE j2k->abf ... */
        MT( j2kToAbf);                     /* ... convert to ORBIT convention */
        q2m( lclimgfrm->_scQuat, scToJ2k);              /* SPICE j2k->s/c ... */
        mxm( scToJ2k, j2kToAbf, scToAbf);                 /* combine matrices */
        mxm( vrmlToSc, scToAbf, vrmlToAbf);                            /* ... */
        MT( vrmlToAbf);                     /* ORBIT to SPICE convention flip */
        m2q( vrmlToAbf, rotat);                            /* convert to quat */

        if ( abs(rotat[0]) < 1.0) {               /* convert to VRML rotation */
          vunit( rotat+1, rotat+1, (double *) 0);
          rotat[0] = 2 * acos( rotat[0]);
        } else { rotat[0] = rotat[2] = rotat[3] = 0.0; rotat[1] = 1.0; }

        /* spacecraft clock - drop last 3 characters, convert / to _ */
        ospice_scdecd( &lclimgfrm->_scid, &lclimgfrm->_sclk
                     , &utclenm1, &sclkchLenout, sclkch);
        sclkch[(sclkchLenout>3)?(sclkchLenout-3):sclkchLenout] = '\0';
        while ( (sclkslash=strchr(sclkch,'/'))) *sclkslash = '_';

        /* output arguments to MSI/NIS/NIS2Fov() macro */

        fprintf( pVrml
        , "%sFov(%lg %lg %lg,%lg %lg %lg %lg,%lg %lg %lg,%lg"
        , lclimgfrm->_instrName                    /* MSI, NIS, NIS2 */
        , loc[0], loc[1], loc[2]                   /* offset to center of FOV */
        , rotat[1], rotat[2], rotat[3], rotat[0]   /* VRML rot:  AXIS+ROT */
        , scVec[0], scVec[1], scVec[2]             /* S/C position, ABF */
        , scale                                    /* S/C range to FOV */
        );
        fprintf( pVrml, ",%lg %lg %lg, \"%s\", \"#%s\",Dl_%s,Vp_%s)\n"
        , fromSun[0], fromSun[1], fromSun[2]       /* DirectionalLight */
        , sclkch                    /* Frame Text to display    "SCLK" */
        , sclkch                    /* URL of Viewpoint         "#SCLK" */
        , sclkch                    /* DirectionalLight NodeID  Dl_SCLK */
        , sclkch                    /* Viewpoint NodeID         Vp_SCLK */
        );

        nFrmDone++;
      } /* if !lclimgfrm->_isHidden ... */
    } /* for lclimgfrm ... */

    /* default viewpoint */
    if ( !nFrmDone) {
      fprintf( pVrml, "DEF Vp_Dflt Viewpoint { position 0 0 %lg %s }\n",maxrad*3
             , "description \"viewptDefault\" orientation 0 1 0 0");
      fprintf( pVrml
             , "DEF Dl_Dflt DirectionalLight { direction 1 1 1 on FALSE }\n");
      fprintf( pVrml, "ROUTE Vp_Dflt.isBound TO Dl_Dflt.on\n");
    }

    pclose( pVrml);

    orbitgui_watchCursor( app_shell, False);
    if ( fsdShell) orbitgui_watchCursor( fsdShell, False);

  }

FILSELDLG_PART2( orbitgui_saveVrmlFSD, "Save VRML File Selection"
               , "*.wrl", 0, orbitgui_saveVrmlFSD_CB, 0)

/**********************************************************/
/* save view to file in some format - bring up file sel dialog */

#define SAVE_VIEW( FNNAME, LABELTEXT, PATTERN, WRITE_VIEW_CALLBACK) \
void \
FNNAME(w, client_data, call_data) \
Widget	w; \
XtPointer	client_data; \
XtPointer	call_data; \
{ \
ITEM *cur_item = (ITEM *) client_data; \
Widget dialog; \
void do_writeable_search(); \
XmString title_string= NULL; \
XmString pattern_string= NULL; \
 \
  title_string=XmStringCreateLtoR(LABELTEXT, CHARSET); \
  pattern_string=XmStringCreateLtoR(PATTERN, CHARSET); \
  n = 0; \
  XtSetArg (args[n], XmNpattern, pattern_string); n++; \
  XtSetArg (args[n], XmNdialogTitle, title_string); n++; \
  XtSetArg (args[n], XmNfileSearchProc, do_writeable_search); n++; \
  dialog = XmCreateFileSelectionDialog (app_shell, "Files", args, n); \
  XtSetSensitive ( \
      XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False); \
  XtAddCallback (dialog, XmNokCallback, WRITE_VIEW_CALLBACK, client_data); \
  XtAddCallback (dialog, XmNcancelCallback, MyDW, NULL); \
 \
  XtManageChild (dialog); \
 \
  XmSF( title_string); \
  XmSF( pattern_string); \
 \
  return; \
}

/***********************************/
/* Postscript & Xfig save view */

SAVE_VIEW( save_view_ps_CB, "'Save View to Postscript' File Name Selection"
         , "*.ps", write_view_ps_CB)

SAVE_VIEW( save_view_xfig_CB, "'Save View to Xfig' File Name Selection"
         , "*.fig", write_view_xfig_CB)

SAVE_VIEW( save_view_gif_CB, "'Save View to GIF' File Name Selection"
         , "*.gif", write_view_gif_CB)

SAVE_VIEW( save_view_gifs_CB, "'Save View to GIF' File Name Selection"
         , "*.gif", write_view_gifs_CB)

/**************************************/

int orbitgui_comment_fldMenu_ok_CB( ITEM *cur_item, FLDMENU *fldMenu) {
  return 0;
}

int orbitgui_comment_fldMenu_cancel_CB( ITEM *cur_item, FLDMENU *fldMenu) {
  free( fldMenu);
  return 1;
}

/**************************************/
/* build comment field menus */

void
orbitgui_comments_CB( Widget w, XtPointer client_data, XtPointer call_data) {
ITEM *cur_item = (ITEM *) client_data;
static char lblBit[] = { "Comment Control\0In => On\0" };
static char lblTxt[] = { "Comment (200 char limit)" };
static char wildCard[] = { "*.CMT" };
int i;
FLDMENU *fldMenu, *fm;
char *txt0;
long allocsize;

  allocsize = ( MAXNUMCOMMENT + 2) * sizeof( FLDMENU);
  allocsize += (MAXNUMCOMMENT * MAXLENCOMMENT);

  fm = fldMenu = (FLDMENU *) malloc( allocsize);
  if ( !fm) return;

  txt0 = (char *) (fm + MAXNUMCOMMENT + 2);

  i = 0;

  commentsOn &= 1L;

  fm->type = FLD_BITS; fm->fld_lng = &commentsOn;
  fm->lbl_txt = lblBit;
  fm->fld_lowbit = 0;
  ++i; ++fm;


  while ( (i-1) < MAXNUMCOMMENT) {
    fm->lbl_txt = lblTxt;
    fm->type = FLD_TXT; fm->fld_txt = comments[i-1]; fm->fld_txt0 = txt0;
    fm->fld_txt_maxlen = MAXLENCOMMENT;
    strncpy( fm->fld_txt0, comments[i-1], MAXLENCOMMENT);
    fm->fld_txt0[MAXLENCOMMENT-1] = '\0';
    ++i; ++fm; txt0 += MAXLENCOMMENT;
  }

  /* last fm struct:  add cancel callback & filetype */
  fm->type = FLD_END;
  fm->client_call = orbitgui_comment_fldMenu_cancel_CB;
  fm->fld_txt0 = wildCard;

  /* add ok callback & cur_item pointer to first fm struct */
  fldMenu->client_call = orbitgui_comment_fldMenu_ok_CB;
  fldMenu->client_data = cur_item;

  orbitgui_create_fldmenu_dialog( w, "Comments", fldMenu);

  return;
}

/****************************/
/* flip toggle button state */

void
orbitguiFunTimer_CB( XtPointer client_data, XtIntervalId id) {
Boolean newState = XmToggleButtonGetState((Widget) client_data) ? False : True;
  XmToggleButtonSetState( (Widget) client_data, newState, False);
}

/***************************************************************************/
/* react to changed toggle button state by setting a timer to flip it back */

void
orbitguiFun_CB(Widget w, XtPointer client_data, XtPointer call_data) {
static count;
XtAppContext appcon = XtWidgetToApplicationContext( w);
XtIntervalId id;
Boolean fixedState = client_data ? False : True;

  if ( !(fixedState ^ XmToggleButtonGetState(w))) {
    /* force a segmentation fault the third time Yes is pressed */
    if ( !fixedState) {
      if ( ++count > 5) strcpy( (char *) 0, "hahaha");  /* segm. fault */
    }
    id = XtAppAddTimeOut( appcon, 1000
                        , (XtTimerCallbackProc)orbitguiFunTimer_CB, w);
  }
  
  return;
}

/**********************************************************/
/* save imgfrm structures to file - bring up file selection dialog */
void
save_frames_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
ITEM *cur_item = (ITEM *) client_data;
Widget dialog;
void do_writeable_search();
XmString title_string= NULL;
XmString pattern_string= NULL;

  if ( !imgfrm) return;

  title_string=XmStringCreateLtoR("'Save Frames' File Name Selection", CHARSET);
  pattern_string=XmStringCreateLtoR("*.f", CHARSET);
  n = 0;
  XtSetArg (args[n], XmNpattern, pattern_string); n++;
  XtSetArg (args[n], XmNdialogTitle, title_string); n++;
  XtSetArg (args[n], XmNfileSearchProc, do_writeable_search); n++;
  dialog = XmCreateFileSelectionDialog (app_shell, "Files", args, n);
  orbitgui_setSensitivity( w, dialog);
  XtSetSensitive (
      XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
  XtAddCallback (dialog, XmNokCallback, write_frames_CB, client_data);
  XtAddCallback (dialog, XmNcancelCallback, MyDW, NULL);

  XtManageChild (dialog);

  XmSF( title_string);
  XmSF( pattern_string);

  return;
}

/****************************************************************/
int 
orbitgui_wSSFldmenu_ok_CB( ITEM *cur_item, FLDMENU *fldMenu) {
  return(0);              /* nothing to do; return 0 to keep field menu alive */
}

/****************************************************************/
int 
orbitgui_wSSFldmenu_cancel_CB( ITEM *cur_item, FLDMENU *fldMenu) {
  free( fldMenu);                     /* free malloc'ed field menu structures */
  return(1);
}

/****************************************************************/
void
orbitgui_walkStepSize_CB( Widget w, XtPointer client_data, XtPointer call_data){
Widget pw = w ? XtParent( w) : (Widget) NULL;
Widget ppw = pw ? XtParent( pw) : (Widget) NULL;
Widget pppw = ppw ? XtParent( ppw) : (Widget) NULL;
Widget ppppw = pppw ? XtParent( pppw) : (Widget) NULL;
Widget pppppw = ppppw ? XtParent( ppppw) : (Widget) NULL;

Widget pFW = orbitgui_findParentForm( w);

#if 0
int tmp = fprintf( stderr
                 , "%s    %08xx %08xx %08xx %08xx %08xx %08xx %08xx\n"
                 , "w, pw, ppw, pppw, ppppw, pppppw, pFW = \n"
                 , (long) w, (long) pw, (long) ppw, (long) pppw
                 , (long) ppppw, (long) pppppw, (long) pFW);

int tmp2 = fprintf( stderr, "first_item->_som_W, ->_form = %08xx %08xx\n"
                  , first_item ? first_item->_som_W : (Widget) NULL
                  , first_item ? first_item->_form : (Widget) NULL
                  );
#  ifndef _DPR_SOM_W_
#  define _DPR_SOM_W_
#  endif
#else
#  ifdef _DPR_SOM_W_
#  undef _DPR_SOM_W_
#  endif
#endif
int wSS = ((long) client_data) + 1;
FLDMENU *fm, *fldMenu;
static char wSSFldmenuWildcard[] = { "*.orbitStepsize" };
/* extra spaces will make slider wider */
static char wSSFldmenuLbl[] = { "Adjust Fwd/Back N step size            " };

FIND_ITEM_FROM_FORM( w)

# ifdef _DPR_SOM_W_
  fprintf( stderr, "client data = %d\n", (int) client_data); /**/
# endif

  switch ( wSS) {
  case 13:  /* Other ... */
    fm = fldMenu = (FLDMENU *) malloc(2 * sizeof( FLDMENU));
    fm->type = FLD_SLIDER_UPRIGHT;
    fm->subtype = FLD_INT;
    fm->lbl_txt = "Fwd/Back N Step Size, frames";
    fm->fld_loend = 1;
    fm->fld_hiend = 150;
    fm->fld_txt_maxlen = 0;  /* number of decimal points */
    fm->fld_int = &walkStepSize;

    fm++;
    fm->type = FLD_END;
    fm->client_call = orbitgui_wSSFldmenu_cancel_CB;
    fm->fld_txt0 = wSSFldmenuWildcard;

    fldMenu->client_call = orbitgui_wSSFldmenu_ok_CB;
    fldMenu->client_data = cur_item;  /* client_data not used; do this anyway */

    orbitgui_create_fldmenu_dialog( app_shell, wSSFldmenuLbl
                                  , fldMenu);
    break;

  default:                 /* 0 < wSS < 13; set walkStepSize & get on our way */
    if ( walkStepSize > 0 && walkStepSize < 13) walkStepSize = wSS;
    break;
  }

  return;
}

void
display_3d(Display *dpy)
{
int i;
int buildFromRight;
ITEM		*cur_item;
Widget		sub_window;
Widget		menu_bar, menu_file, menu_pnt, menu_orb, menu_instr
			, menu_frames
			, menu_opts
			, menu_auto
                        , menu_help;
Widget		button, cascade, sep1, sep2, vSepW, tmp_W, tmpRC_W, form2;

static XmString phi_str, theta_str, rho_str, Noraz_str;

XmString lclxmstr;

XGCValues xgcv;

XWindowAttributes xwa;

  if ( first_item) {
    for ( cur_item=first_item; cur_item->next_item;
      cur_item=cur_item->next_item) ;
    cur_item->next_item = (ITEM *) malloc( sizeof( ITEM));
    cur_item=cur_item->next_item;

  } else {
    cur_item = first_item = (ITEM *) malloc( sizeof( ITEM));

    phi_str= XmStringCreateLtoR("Bore lat", CHARSET);
    theta_str= XmStringCreateLtoR("Bore lon", CHARSET);
    Noraz_str= XmStringCreateLtoR("NORAZ", CHARSET);
    size_R= (size_t)(16*sizeof(double));
    pow_Noraz=  pow(10., -(double)decimal_Noraz);
    pow_phi=   pow(10., -(double)decimal_phi);
    pow_theta= pow(10., -(double)decimal_theta);

    pow_rho=   pow(10., -(double)decimal_rho);

    fontInfo = XLoadQueryFont(dpy,"Rom14.500");
    if (fontInfo == NULL) fontInfo = XLoadQueryFont(dpy,"fixed");

    if (fontInfo == NULL) {
      if ( cur_item) free( cur_item);
      fprintf( stderr, "display_3d:  problem loading font\n");
      LOCAL_LONGRETURN;
    }

    degpr = 45.0 / atan(1.0);

    if ( !cur_item) {
      fprintf( stderr, "display_3d:  can't alloc item\n");
      LOCAL_LONGRETURN;
    }

    cur_item->next_item = (ITEM *) NULL;
  }

  initial= (Boolean)True;
  Tx = Ty = dTx = dTy = 0;
  da_pm= (Pixmap) 0;
  sc = (SC *) malloc( sizeof(SC));
  ap = (AP *) malloc( sizeof(AP));

  sc->_cur_item = (void *) cur_item;
  /* 20000204.btc sc->_doFov100 = 0; /**/
  /* 20000204.btc sc->_tryck = 0; /**/
  /* 20000204.btc orbit_set_instrument( sc, SC_MSI); /**/

  sc->_scOrbit = &scOrbit;
  sc->_asterOrbit = &asterOrbit;
  sc->_earthOrbit = &earthOrbit;
  scOrbit._status = ORB_USESPK;
  scOrbit._newutc[0] = '\0';

  scOrbit._agen._addGenFrames = NULL;
  scOrbit._agen._MaxOrbits = 0.0;
  scOrbit._agen._MaxTime = 1400.0;
  scOrbit._agen._FrmOverlap = 0.01;
  scOrbit._agen._FrmOverlap2 = scOrbit._agen._FrmOverlap;
  scOrbit._agen._FrmOverlapTypeBits = (1L << OVERLAP_TYPE_ANN_BIT);

  scOrbit._ckOutBit = 0;  /* do not write C Kernel file by default */

  for ( i=FRAMESOUT_BIT_TIME; i<FRAMESOUT_BIT_LAST; i++) {
  long ibit, mask, *bits;

    ibit = i % FRAMESOUT_BITSPERLONG;
    mask = 1L << ibit;
    bits = scOrbit._framesoutBits + (i / FRAMESOUT_BITSPERLONG);

    if ( !ibit ) *bits = 0;

    switch( i) {
    case FRAMESOUT_BIT_TIME:
    case FRAMESOUT_BIT_P5PHOTOM:
    case FRAMESOUT_BIT_P5VEC:
    case FRAMESOUT_BIT_SCVEC:
    case FRAMESOUT_BIT_SUNVEC:
    case FRAMESOUT_BIT_BOREJ2K:
    case FRAMESOUT_BIT_SC2SUNJ2K:
    case FRAMESOUT_BIT_OFFSUN:
    case FRAMESOUT_BIT_CAMPTVECS:
    case FRAMESOUT_BIT_TIME_ALT:
    case FRAMESOUT_BIT_P5PLATE:
      *bits |= mask;
      break;
    case FRAMESOUT_BIT_GENINFO:
    case FRAMESOUT_BIT_NISINFO:
    case FRAMESOUT_BIT_POINTING:
    case FRAMESOUT_BIT_AGENINFO:
    case FRAMESOUT_BIT_EARTHVEC:
    case FRAMESOUT_BIT_TARGVECSBF:
    case FRAMESOUT_BIT_ALLINFO:
    case FRAMESOUT_BIT_SCLKQUAT:
    case FRAMESOUT_BIT_OTHERBODIES:
      break;
    default:
      fprintf( stderr, "Huh? Code WSNBATGH-display_3d(1); i=%d\n", i);
      break;
    }
  }

  earthOrbit._status = ORB_USESPK;
  earthOrbit._newutc[0] = '\0';
  asterOrbit._status = ORB_USESPK;
  asterOrbit._newutc[0] = '\0';

  scOrbit._sc = (void *) sc;
  earthOrbit._sc = (void *) sc;
  asterOrbit._sc = (void *) sc;

  /* initialize comments to null strings BEFORE orbit_init, which 
   * may load comments from SPICESPEC file
   */
  for ( n=0; n<MAXNUMCOMMENT; ++n) comments[n][0] = '\0';
  commentsOn = 0;

  ongoingDrag = DRAG_NONE;

  spudv = orbit_init( sc);   /* load up initial s/c structure */
  sc->_doFov100 = 0;                 /* 20000204.btc from above */
  sc->_tryck = 0;                    /* 20000204.btc from above */
  orbit_set_instrument( sc, SC_MSI); /* 20000204.btc from above */

  /* set default scaling to data range, don't update display */
  orbitguiSetShading( cur_item, 0.0, 1.0, (int) 1, (int) 0);

  scOrbit._agen._NISFOV = 1;
  scOrbit._agen._StartingStep = 72;
  scOrbit._agen._EndingStep = 79;
  scOrbit._agen._DeltaStep = 1;
  scOrbit._agen._TimePerStep = 10;
  scOrbit._agen._DelayPerStep = 2;
  scOrbit._agen._DelayPerRow = 5;

  matchedimgfrm = fwdhalfimgfrm = backhalfimgfrm = imgfrm = (IMGFRM *) 0;
  sel1Imgfrm = sel2Imgfrm = delListImgfrm = delWhereImgfrm = (IMGFRM *)0;
  walkStepSize = 1;
  /* halfNotTimeSteps = 1; */

  currentImgfrmValid = 0;
  currentImgfrm.imgfrmAlloced = 0;
  currentImgfrm._otherBodies = (OTHERBODIES *) 0;

  /* n = sc->_nmsipts;
  n = MAXMACRO( n, sc->_nnispts); */

  currentImgfrm._ncampts = sc->_nVert[0];
  currentImgfrm._nClosedPts = sc->_nInstrClosedPtsPtr[0];
  for ( i=1; i<sc->_numInstr; ++i) {
    if ( currentImgfrm._ncampts < sc->_nVert[i]) {
      currentImgfrm._ncampts = sc->_nVert[i];
      currentImgfrm._nClosedPts = sc->_nInstrClosedPtsPtr[i];
    }
  }
  currentImgfrm.vec3 = 
    (double *) malloc( currentImgfrm._ncampts*3*sizeof(double));

  hcfilename = (char *) 0;

  app_shell= XtVaAppCreateShell( "NEAR Orbit Viewer", "Display3d"
                               , topLevelShellWidgetClass, dpy
                               , XmNdeleteResponse, XmDESTROY
                               , NULL);
  XtAddCallback( app_shell, XmNdestroyCallback, exit_CB, NULL);

  n = 0;
  sub_window= XmCreateMainWindow(app_shell, "sub_window", args, n);
  XtManageChild(sub_window);
  n= 0;
  XtSetArg(args[n], XmNallowShellResize, True), n++;
  XtSetValues(sub_window, args, n);

  n= 0;
  form= XtVaCreateManagedWidget("form", xmFormWidgetClass, sub_window
        , XmNwidth, 780
        , XmNheight, 580
        , NULL);

  n= 0;
  menu_bar= XmCreateMenuBar(form, "menu_bar", args, n);
  XtManageChild(menu_bar);

  n= 0;
  XtSetArg(args[n], XmNtearOffModel, XmTEAR_OFF_ENABLED), n++;
  menu_file= XmCreatePulldownMenu(menu_bar, "menu_file", args, n);

  n= 0;
  menu_frames= XmCreatePulldownMenu(menu_bar, "menu_frames", args, n);

  n= 0;
  menu_pnt= XmCreatePulldownMenu(menu_bar, "menu_pnt", args, n);

  n= 0;
  XtSetArg(args[n], XmNradioBehavior, True), n++;
  XtSetArg(args[n], XmNradioAlwaysOne, True), n++;
  menu_instr= XmCreatePulldownMenu(menu_bar, "menu_instr", args, n);

  n= 0;
  XtSetArg(args[n], XmNradioBehavior, True), n++;
  XtSetArg(args[n], XmNradioAlwaysOne, True), n++;
  menu_orb= XmCreatePulldownMenu(menu_bar, "menu_orb", args, n);

  n= 0;
  XtSetArg(args[n], XmNtearOffModel, XmTEAR_OFF_ENABLED), n++;
  menu_opts= XmCreatePulldownMenu(menu_bar, "menu_opts", args, n);

  n= 0;
  menu_auto= XmCreatePulldownMenu(menu_bar, "menu_auto", args, n);

/*	n= 0;
/*	menu_help= XmCreatePulldownMenu(menu_bar, "menu_help", args, n);
/**/

  n= 0;
  XtSetArg(args[n], XmNsubMenuId, menu_file), n++;
  cascade= XtCreateManagedWidget("File", xmCascadeButtonWidgetClass
                                      , menu_bar, args, n);

	/* File menu pulldown */

	n= 0;
	button= XtCreateManagedWidget("Save View as Postscript(tm) ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback(button, XmNactivateCallback, save_view_ps_CB, cur_item);

	n= 0;
	button= XtCreateManagedWidget("Save View as Xfig ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback(button, XmNactivateCallback, save_view_xfig_CB, cur_item);

	n= 0;
	button= XtCreateManagedWidget("Save View as GIF ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback(button, XmNactivateCallback, save_view_gif_CB, cur_item);

	n= 0;
	button= XtCreateManagedWidget("Save Views as GIFs ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback(button, XmNactivateCallback, save_view_gifs_CB, cur_item);

	n= 0;
	button= XtCreateManagedWidget("Save View as VRML ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback( button, XmNactivateCallback, orbitgui_saveVrmlFSD
                     , cur_item);

	n= 0;
	button= XtCreateManagedWidget("Modify Comments for Hardcopy ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback(button,XmNactivateCallback,orbitgui_comments_CB,cur_item);

	n= 0;
	button= XtCreateManagedWidget("Load new SPUD model ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback( button, XmNactivateCallback, orbitgui_newSpudrFSD
                     , cur_item);

	n= 0;
	button= XtCreateManagedWidget("Load new PLATE model ..."
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback( button, XmNactivateCallback, orbitgui_newSpudvFSD
                     , cur_item);

	n= 0;
	button= XtCreateManagedWidget("Exit"
                    , xmPushButtonWidgetClass, menu_file, args, n);
	XtAddCallback(button, XmNactivateCallback, exit_CB, NULL);

	/* Frames menu pulldown */

	n= 0;
	XtSetArg(args[n], XmNsubMenuId, menu_frames), n++;
	cascade= XtCreateManagedWidget("Frames", xmCascadeButtonWidgetClass
                                      , menu_bar, args, n);

	n= 0;
	button= XtCreateManagedWidget("Select Frames Data ..."
                    , xmPushButtonWidgetClass, menu_frames, args, n);
	XtAddCallback(button, XmNactivateCallback,select_framesout_CB,cur_item);

	n= 0;
	button= XtCreateManagedWidget("Save Frames ..."
                    , xmPushButtonWidgetClass, menu_frames, args, n);
	XtAddCallback(button, XmNactivateCallback, save_frames_CB, cur_item);

	n= 0;
	button= XtCreateManagedWidget("Clear Frames"
                    , xmPushButtonWidgetClass, menu_frames, args, n);
	XtAddCallback(button, XmNactivateCallback, clear_frames_CB, cur_item);

	/* menus for spacecraft pointing */

	n= 0;
	XtSetArg(args[n], XmNsubMenuId, menu_pnt), n++;
	cascade= XtCreateManagedWidget("  Pointing  "
                    , xmCascadeButtonWidgetClass, menu_bar, args, n);

  /* - macros for spacecraft pointing */

# define XAIMPT(ST,P,S,S2) \
  n= 0; \
  XCPDM( ST.aimpt.menu, P, S, S2); \
  n= 0; \
  XCMW( ST.aimpt, abf, "Asteroid Body Fixed ..."); \
  XCMW( ST.aimpt, aci, "Asteroid Centered J2000 ..."); \
  XCMW( ST.aimpt, nadir, "Nadir-Sun Angle ..."); \
  XCMW( ST.aimpt, j2k, "J2000 Inertial ..."); \
  XCMW( ST.aimpt, eci, "Earth Centered J2000 ..."); \
  XCMW( ST.aimpt, sci, "Solar Centered J2000...")

# define XSCVEC(ST,P,S,S2) \
  n= 0; \
  XCPDM( ST.scvec.menu, P, S, S2); \
  n= 0; \
  XCMW( ST.scvec, instr, "Instrument Boresight"); \
  XCMW( ST.scvec, panel, "Solar Panel/Antenna"); \
  XCMW( ST.scvec, x, "S/C X Axis"); \
  XCMW( ST.scvec, y, "S/C Y Axis"); \
  XCMW( ST.scvec, z, "S/C Z Axis"); \
  XCMW( ST.scvec, user, "S/C Vector ...")

# define XCPDM(C,P,S,S2) \
  XtSetArg(args[n], XmNradioBehavior, True), n++; \
  XtSetArg(args[n], XmNradioAlwaysOne, True), n++; \
  C= XmCreatePulldownMenu(P, S, args,  n); \
  n= 0; \
  XtSetArg(args[n], XmNsubMenuId, C), n++; \
  cascade= XtCreateManagedWidget(S2, xmCascadeButtonWidgetClass, P, args, n)

# define XCMW(ST,C,SS) \
  ST.C= XtCreateManagedWidget(SS, xmToggleButtonWidgetClass, ST.menu, args, n);\
  XtAddCallback( ST.C, XmNvalueChangedCallback, pointing_CB, cur_item)

  /* - create pointing menus */

  XAIMPT( bore, menu_pnt, "menu_boreaimpt", "Aimpoint");
  XmToggleButtonSetState( bore.aimpt.nadir, True, False);
  bore.aimpt.vec[0] = bore.aimpt.vec[1] = bore.aimpt.vec[2] = 0.0;
  bore.aimpt.type = Inadir;

  XSCVEC( bore, menu_pnt, "menu_borescvec", "S/C Virtual Boresight");
  XmToggleButtonSetState( bore.scvec.instr, True, False);
  bore.scvec.vec[0] = bore.scvec.vec[1] = bore.scvec.vec[2] = 0.0;
  bore.scvec.type = Iinstr;

  n = 0;
  sep1 = XtCreateManagedWidget("sep1",xmSeparatorWidgetClass, menu_pnt,args,n);

  XAIMPT( roll, menu_pnt, "menu_rollaimpt", "External Roll Reference");
  XmToggleButtonSetState( roll.aimpt.eci, True, False);
  roll.aimpt.vec[0] = roll.aimpt.vec[1] = roll.aimpt.vec[2] = 0.0;
  roll.aimpt.type = Ieci;

  XSCVEC( roll, menu_pnt, "menu_rollscvec", "S/C Roll Vector");
  XmToggleButtonSetState( roll.scvec.panel, True, False);
  roll.scvec.vec[0] = roll.scvec.vec[1] = roll.scvec.vec[2] = 0.0;
  roll.scvec.type = Ipanel;

  n = 0;
  sep1 = XtCreateManagedWidget("sep1",xmSeparatorWidgetClass, menu_pnt,args,n);

  button = XtVaCreateManagedWidget( "Point4", xmPushButtonWidgetClass, menu_pnt
                                  , NULL);
  XtAddCallback(button, XmNactivateCallback, point4_CB, cur_item);

  /* Instrument menu */

  n= 0;
  XtSetArg(args[n], XmNsubMenuId, menu_instr), n++;
  instr_cascade= XtCreateManagedWidget(" Instrument "
                    , xmCascadeButtonWidgetClass, menu_bar, args, n);

# ifdef XCMWToggle
# undef XCMWToggle
# endif
# define XCMWToggle( BUTTON, LABEL, CALLBACK, INITSTATE) \
  n= 0; \
  BUTTON= XtCreateManagedWidget( LABEL \
               , xmToggleButtonWidgetClass, menu_instr, args, n); \
  XtAddCallback(BUTTON, XmNvalueChangedCallback, CALLBACK, cur_item); \
  XmToggleButtonSetState( BUTTON, INITSTATE, False)

  XCMWToggle( selectMSI_W, "Select MSI", orbitgui_selectMSI_CB, True);
  XCMWToggle( selectNIS_W, "Select NIS ...", orbitgui_selectNIS_CB, False);
  XCMWToggle( selectNIS2_W, "Select Wide NIS ...",orbitgui_selectNIS2_CB,False);
  selectInstrActive = 0;

  /* Ephemeris menu */

  n= 0;
  XtSetArg(args[n], XmNsubMenuId, menu_orb), n++;
  cascade= XtCreateManagedWidget(" Ephemeris "
                    , xmCascadeButtonWidgetClass, menu_bar, args, n);

  n= 0;
  orbitstr.elts_menu = XmCreatePulldownMenu( menu_orb, "elts_menu", args,  n);
  orbitstr.spice_menu = XmCreatePulldownMenu( menu_orb, "spice_menu", args,  n);
  n= 0;
  XtSetArg(args[n], XmNsubMenuId, orbitstr.elts_menu), n++;
  cascade= XtCreateManagedWidget( "Elements-based trajectories"
         , xmCascadeButtonWidgetClass, menu_orb, args, n);
  n= 0;
  XtSetArg(args[n], XmNsubMenuId, orbitstr.spice_menu), n++;
  cascade= XtCreateManagedWidget( "SPICE-based trajectories"
         , xmCascadeButtonWidgetClass, menu_orb, args, n);

# ifdef XCMW
# undef XCMW
# endif
# define XCMW( BUTTON, SS, PARENT, CB, ORBITSTR, INITSTATE) \
  n= 0; \
  orbitstr.BUTTON = XtCreateManagedWidget( SS, xmToggleButtonWidgetClass \
               , orbitstr.PARENT, args, n); \
  XtAddCallback( orbitstr.BUTTON, XmNvalueChangedCallback, CB, &ORBITSTR); \
  XmToggleButtonSetState( orbitstr.BUTTON, INITSTATE, False)

  XCMW( elts_aster, "Asteroid ...", elts_menu, elts_CB, asterOrbit, False);
  XCMW( elts_sc, "Spacecraft ...", elts_menu, elts_CB, scOrbit, False);
  XCMW( elts_earth, "Earth ...", elts_menu, elts_CB, earthOrbit, False);

  XCMW( spice_aster, "Asteroid ...", spice_menu, spice_CB, asterOrbit, True);
  XCMW( spice_sc, "Spacecraft ...", spice_menu, spice_CB, scOrbit, True);
  XCMW( spice_earth, "Earth ...", spice_menu, spice_CB, earthOrbit, True);

  scOrbit._status = ORB_USESPK;
  asterOrbit._status = ORB_USESPK;
  earthOrbit._status = ORB_USESPK;

  /* Options menu */

  n= 0;
  XtSetArg(args[n], XmNsubMenuId, menu_opts), n++;
  cascade= XtCreateManagedWidget("Options", xmCascadeButtonWidgetClass
                    , menu_bar, args, n);

  sunFromAstAbf[0] = 1.0;
  sunFromAstAbf[1] = sunFromAstAbf[2] = 0.0;

  /*******************************************************/

# ifdef XCMWCascade2
# undef XCMWCascade2
# endif
# define XCMWCascade2( LABEL, WNAME, WID) \
  n= 0; \
  XtSetArg(args[n], XmNradioBehavior, True), n++; \
  XtSetArg(args[n], XmNradioAlwaysOne, True), n++; \
  WID = XmCreatePulldownMenu( menu_opts, WNAME, args, n); \
  \
  n= 0; \
  XtSetArg(args[n], XmNsubMenuId, WID ), n++; \
  cascade= XtCreateManagedWidget( LABEL \
                    , xmCascadeButtonWidgetClass, menu_opts, args, n)

# ifdef XCMWToggle2
# undef XCMWToggle2
# endif
# define XCMWToggle2( LABEL, INITSTATE, VAL, INT, WID, CB) \
  n= 0; \
  button = XtCreateManagedWidget( LABEL \
               , xmToggleButtonWidgetClass, WID, args, n); \
  XtAddCallback(button,XmNvalueChangedCallback, CB, (XtPointer) (VAL)); \
  if ( INITSTATE) INT = (VAL); \
  XmToggleButtonSetState( button, INITSTATE, INITSTATE)

# define SEP sep1 = XtVaCreateManagedWidget( "sep1" \
                                          , xmSeparatorWidgetClass, menu_opts \
                                          , NULL)

  /****************************************************/

  button = XtVaCreateManagedWidget( "Re-center tracking"
                                  , xmPushButtonWidgetClass, menu_opts
                                  , NULL);
  XtAddCallback(button, XmNactivateCallback, recenter_CB, cur_item);

  SEP; /**/

  XCMWCascade2( "Scale View ...", "menu_scale2frame", scale2frameW);

  XCMWToggle2( "to body", True, 0, scale2frame, scale2frameW, scale2frame_CB);
  XCMWToggle2( "to FOV", False, 1, scale2frame, scale2frameW, scale2frame_CB);

  XtManageChild( scale2frameW);

  /**/

  XCMWCascade2( "Center View ...", "menu_centerframe", centerframeW);

  XCMWToggle2( "on body", True, 0, centerframe, centerframeW, centerframe_CB);
  XCMWToggle2( "on FOV", False, 1, centerframe, centerframeW, centerframe_CB);

  XtManageChild( centerframeW);

  /**/

  XCMWCascade2( "Define Viewing UP as ...", "menu_northup", northupW);

  XCMWToggle2( "Frame Up", True, 0, display_northup, northupW, northup_CB);
  XCMWToggle2( "Body North", False, 1, display_northup, northupW, northup_CB);

  XtManageChild( northupW);

  /**/

  XCMWCascade2( "Set Scalebar Visibility ...", "menu_scalebar", scalebarW);

  XCMWToggle2( "Hidden", False, 0, scalebar, scalebarW, scalebar_CB);
  XCMWToggle2( "Shown", True, 1, scalebar, scalebarW, scalebar_CB);

  XtManageChild( scalebarW);

  /**/

  XCMWCascade2( "Set Text Style ...", "menu_scalebar", textStyleW);

  XCMWToggle2( "None", False, TEXTSTYLE_NONE
                                       , textStyle, textStyleW, textStyle_CB);
  XCMWToggle2( "Original (UTC, orbital info)", False, TEXTSTYLE_ORIGINAL
                                       , textStyle, textStyleW, textStyle_CB);
  XCMWToggle2( "UTC(MET) Range", False, TEXTSTYLE_RANGE
                                       , textStyle, textStyleW, textStyle_CB);
  XCMWToggle2( "UTC(kMET) Range", True, TEXTSTYLE_RANGE_TRUNC3
                                       , textStyle, textStyleW, textStyle_CB);

  XtManageChild( textStyleW);

  SEP; /*******************************************************/

  XCMWCascade2( "Model Gridpoints hidden ...", "menu_hidden", hiddenW);

  XCMWToggle2( "Never", False, 0, hidden, hiddenW, hidden_CB);
  XCMWToggle2( "if Hidden (from S/C)", False
             , SPUDV_HID
             , hidden, hiddenW, hidden_CB);
  XCMWToggle2( "if Shadowed (from Sun)", False
             , SPUDV_SHAD
             , hidden, hiddenW, hidden_CB);
  XCMWToggle2( "if Hidden OR Shadowed", True
             , SPUDV_SHADHID
             , hidden, hiddenW, hidden_CB);

  XtManageChild( hiddenW);

  /**/

  XCMWCascade2( "Model Grid Linestyle", "menu_linestyle", linestyleW);

  XCMWToggle2( "None", False, LINESTYLE_NONE
             , linestyle, linestyleW, shape_linestyle_CB);
  XCMWToggle2( "White Solid", False, LINESTYLE_WHITE
             , linestyle, linestyleW, shape_linestyle_CB);
  XCMWToggle2( "Gray Dashed", True, LINESTYLE_GRAY
             , linestyle, linestyleW, shape_linestyle_CB);

  XtManageChild( linestyleW);

  /**/

  XCMWCascade2( "FOV points ...", "menu_fov100Vis", fov100VisW);

  XCMWToggle2( "Visible", False, FOV100_VISIBLE
             , fov100Vis, fov100VisW, fov100Vis_CB);
  XCMWToggle2( "Invisible", True, FOV100_INVISIBLE
             , fov100Vis, fov100VisW, fov100Vis_CB);

  switch ( fov100Vis) {
  case FOV100_VISIBLE: DOFOV100_SETGLOBAL( sc); break;
  case FOV100_INVISIBLE: DOFOV100_CLRGLOBAL( sc); break;
  }

  XtManageChild( fov100VisW);

  /**/

  XCMWCascade2( "Model shading palette", "menu_colors", colorW);

  XCMWToggle2( "Off (black)", True, COLOR_NONE, coloring, colorW, color_CB);
  XCMWToggle2( "Grayscale", False, COLOR_GRAY, coloring, colorW, color_CB);
  XCMWToggle2( "Rainbow", False, COLOR_RAINBOW, coloring, colorW, color_CB);

  XtManageChild( colorW);

  /**/

  button = XtVaCreateManagedWidget( "Set shading scale ..."
                                  , xmPushButtonWidgetClass, menu_opts
                                  , NULL);
  XtAddCallback(button, XmNactivateCallback, orbitguiShadingScale_CB, cur_item);

  /**/

  XCMWCascade2( "Model shading Data Source", "menu_shading", shadeW);

  XCMWToggle2( "Mu0 [cos(i)]", True, SHADE_MU0, shading, shadeW, shade_CB);
  XCMWToggle2( "Mu  [cos(e)]", False, SHADE_MU, shading, shadeW, shade_CB);
  XCMWToggle2( "Mu0xMu", False, SHADE_MU0xMU, shading, shadeW, shade_CB);
  XCMWToggle2( "Incidence", False, SHADE_INCID, shading, shadeW, shade_CB);
  XCMWToggle2( "Emission", False, SHADE_EMISS, shading, shadeW, shade_CB);
  XCMWToggle2( "Map Data", False, SHADE_MAPDATA, shading, shadeW, shade_CB);

  XtManageChild( shadeW);

  SEP; /*******************************************************/

  XCMWCascade2( "FOV Frames invisible", "menu_hideframes3", hideframes3W);

  XCMWToggle2( "Never", False, 0, hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If Facing Away", True
             , HIDEFRMIF_FACEAWAY
             , hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If Unlit", False
             , HIDEFRMIF_UNLIT
             , hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If > 1 orbit away", False
             , HIDEFRMIF_GT1ORBIT
             , hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If Facing Away OR Unlit", False
             , HIDEFRMIF_FACEAWAY|HIDEFRMIF_UNLIT
             , hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If Facing Away OR > 1 orbit away", False
             , HIDEFRMIF_FACEAWAY|HIDEFRMIF_GT1ORBIT
             , hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If Unlit OR > 1 orbit away", False
             , HIDEFRMIF_UNLIT|HIDEFRMIF_GT1ORBIT
             , hideframes3, hideframes3W, hideframes3_CB);
  XCMWToggle2( "If Facing Away OR Unlit OR > 1 orbit away", False
             , HIDEFRMIF_FACEAWAY|HIDEFRMIF_UNLIT|HIDEFRMIF_GT1ORBIT
             , hideframes3, hideframes3W, hideframes3_CB);

  XtManageChild( hideframes3W);

  /**/

  XCMWCascade2( "FOV Frames ignored if"
              , "menu_hits4", minVertsW);

  XCMWToggle2( "# verts on body < 0", True, 0
             , minHits4, minVertsW, minVerts_CB);
  XCMWToggle2( "# verts on body < 1", False, 1
             , minHits4, minVertsW, minVerts_CB);
  XCMWToggle2( "# verts on body < 2", False, 2
             , minHits4, minVertsW, minVerts_CB);
  XCMWToggle2( "# verts on body < 3", False, 3
             , minHits4, minVertsW, minVerts_CB);
  XCMWToggle2( "# verts on body < 4", False, 4
             , minHits4, minVertsW, minVerts_CB);

  XtManageChild( minVertsW);

  /**/

  XCMWCascade2( "FOV Frame style (JJ Mode)"
              ,"menu_jjmode", jjmodeW);

  XCMWToggle2( "Normal", True, JJMODE_NORMAL, jjmode, jjmodeW, jjmode_CB);
  XCMWToggle2( "Mosaic", False, JJMODE_MOSAIC, jjmode, jjmodeW, jjmode_CB);
  XCMWToggle2( "Orbit", False, JJMODE_ORBIT, jjmode, jjmodeW, jjmode_CB);

  XtManageChild( jjmodeW);

  SEP; /*******************************************************/

  XCMWCascade2( "Pointing/C-kernel Control", "menu_tryck", tryckW);

  XCMWToggle2( "Use aimpt/bore", True, 0, sc->_tryck, tryckW, tryck_CB);
  XCMWToggle2( "Use C-kernels", False, 1, sc->_tryck, tryckW, tryck_CB);

  XtManageChild( tryckW);

  SEP; /*******************************************************/

  XCMWCascade2( "Step by Time or Halves", "menu_halfNotTime", halfNotTimeW);

  /* ***N.B. fwdhalf_W & backhalf_W creation must com after this */

  fwdhalf_W = backhalf_W = (Widget) NULL;      /* to avoid callback seg fault */

  XCMWToggle2( "Step By Minutes", True, 0, halfNotTimeSteps
             , halfNotTimeW, halfNotTime_CB);
  XCMWToggle2( "Step by \"Halves\"", False, 1, halfNotTimeSteps
             , halfNotTimeW, halfNotTime_CB);

  XtManageChild( halfNotTimeW);

  /*******************************************************/
  /* Automatic frame generation menus */

  cascade= XtVaCreateManagedWidget("Auto"
               , xmCascadeButtonWidgetClass, menu_bar
               , XmNsubMenuId, menu_auto
               , NULL);

  button= XtVaCreateManagedWidget("Generate Multiple Frames from Overlap ..."
              , xmPushButtonWidgetClass, menu_auto
              , NULL);
  XtAddCallback( button, XmNactivateCallback, gen_frames_inputs_CB, cur_item);

  button= XtVaCreateManagedWidget("Generate MSI Frames from Timing ..."
              , xmPushButtonWidgetClass, menu_auto
              , NULL);
  XtAddCallback( button, XmNactivateCallback, gen_msi0_frames_inputs_CB
               , cur_item);

  button= XtVaCreateManagedWidget("Generate NIS Frames from Timing ..."
              , xmPushButtonWidgetClass, menu_auto
              , NULL);
  XtAddCallback( button, XmNactivateCallback, gen_nis0_frames_inputs_CB
               , cur_item);

  button= XtVaCreateManagedWidget("Generate Frames from a file ..."
              , xmPushButtonWidgetClass, menu_auto
              , NULL);
  XtAddCallback( button, XmNactivateCallback, orbitgui_entergentimes_CB
               , sc);

  button= XtVaCreateManagedWidget("CAS/Fragment menu ..."
              , xmPushButtonWidgetClass, menu_auto
              , NULL);
  XtAddCallback( button, XmNactivateCallback, orbitgui_CASmenu_CB
               , cur_item);
/**/

  /* Help menus */

/*  n= 0;
/*  XtSetArg(args[n], XmNsubMenuId, menu_help), n++;
/*  cascade= XtCreateManagedWidget("Help", xmCascadeButtonWidgetClass,
/*                    menu_bar, args, n);
/*
/*  n= 0;
/*  button= XtCreateManagedWidget("general info", xmPushButtonWidgetClass,
/*                    menu_help, args, n);
/*  XtAddCallback(button, XmNactivateCallback, general_help_CB, NULL);
/*
/*  n= 0;
/*  button= XtCreateManagedWidget("scale bar info",
/*                    xmPushButtonWidgetClass, menu_help, args, n);
/*  XtAddCallback(button, XmNactivateCallback, scale_help_CB, NULL);
/*
/*  n= 0;
/*  button= XtCreateManagedWidget("Opts info",
/*                    xmPushButtonWidgetClass, menu_help, args, n);
/*  XtAddCallback(button, XmNactivateCallback, opts_help_CB, NULL);
/*
/*  n= 0;
/*  XtSetArg(args[n], XmNmenuHelpWidget, cascade), n++;
/*  XtSetValues(menu_bar, args, n);
/**/

#if 0
	/* calling this does nasty things!
	*/
	XmMainWindowSetAreas(sub_window, menu_bar, NULL, NULL, NULL, form);
#endif

	n= 0;
	XtSetArg(args[n], XmNlabelString, phi_str), n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNtopWidget, menu_bar), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM), n++;
	phi_label= XtCreateManagedWidget("phi_label",
                    xmLabelWidgetClass, form, args, n);

        /* noraz label */

	n= 0;
	XtSetArg(args[n], XmNlabelString, Noraz_str), n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, menu_bar), n++;
	Noraz_label= XtCreateManagedWidget("Noraz_label"
                        , xmLabelWidgetClass, form, args, n);

        /* noraz slider */

	n= 0;
	XtSetArg(args[n], XmNlabelType, XmSTRING), n++;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL), n++;
	XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_RIGHT), n++;
	XtSetArg(args[n], XmNminimum, (int)floor((NORAZ_MIN/pow_Noraz) + .5))
                        , n++;
	XtSetArg(args[n], XmNmaximum, (int)floor((NORAZ_MAX/pow_Noraz) + .5))
                        , n++;
#       define	SCALESTEP1 XtSetArg(args[n], XmNscaleMultiple, 1), n++
	SCALESTEP1;
	XtSetArg(args[n], XmNdecimalPoints, decimal_Noraz), n++;
	XtSetArg(args[n], XmNshowValue, True), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, Noraz_label), n++;
	Noraz_W= XtCreateManagedWidget("Noraz"
                    , xmScaleWidgetClass, form, args, n);
	XtAddCallback(Noraz_W, XmNvalueChangedCallback, new_Noraz_CB, NULL);
	XtAddCallback(Noraz_W, XmNdragCallback, new_Noraz_CB, NULL);
	old_Noraz= 0;
        Noraz = initial_Noraz;

        /* rho (zoom) label */

	rho= 1.0;
	RHO_FMT( tmpstr, rho);
        rho_str= XmStringCreateLtoR( tmpstr, CHARSET);
	n= 0;
	XtSetArg(args[n], XmNlabelString, rho_str), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, phi_label), n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNtopWidget, menu_bar), n++;
	rho_label= XtCreateManagedWidget("rho_label", xmLabelWidgetClass,
                    form, args, n);
        XmSF( rho_str);

        /* rho (zoom) slider - slide bar rho is actually log10(rho) */

	n= 0;
	XtSetArg(args[n], XmNlabelType, XmSTRING), n++;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL), n++;
	XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_RIGHT), n++;
	XtSetArg(args[n], XmNvalue
                        , old_rho= (int)floor((log10(rho)/pow_rho) + .5)), n++;
	XtSetArg(args[n], XmNminimum, (int)floor((RHO_MIN/pow_rho) + .5))
                    , n++;
	XtSetArg(args[n], XmNmaximum, (int)floor((RHO_MAX/pow_rho) + .5))
                    , n++;
	XtSetArg(args[n], XmNdecimalPoints, decimal_rho), n++;
	XtSetArg(args[n], XmNshowValue, False), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, phi_label), n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNtopWidget, rho_label), n++;
	rho_W= XtCreateManagedWidget("rho", xmScaleWidgetClass, form, args, n);
	XtAddCallback(rho_W, XmNvalueChangedCallback, new_rho_CB, NULL);
	XtAddCallback(rho_W, XmNdragCallback, new_rho_CB, NULL);

# ifdef XCB
# undef XCB
# endif
# ifdef XCB2
# undef XCB2
# endif

  /* build up to draw area from bottom */

  /* select start, end frames, delete, undelete */

# define XCB( XCBW, XCBL) \
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM), n++; \
  lclxmstr = XmStringCreateLtoR(XCBL, CHARSET); \
  XtSetArg(args[n], XmNlabelString, lclxmstr), n++; \
  XCBW= XtCreateManagedWidget( XCBL, xmPushButtonWidgetClass, form, args, n); \
  XtAddCallback(XCBW, XmNactivateCallback, orbitgui_edit_frames_CB, cur_item); \
  XmSF( lclxmstr)

# define XCB2( XCBW, XCBL, XCBWRT) \
  n= 0; \
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET), n++; \
  XtSetArg(args[n], XmNrightWidget, XCBWRT), n++; \
  XCB( XCBW, XCBL)

  n= 0;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
  XCB( undelImgfrm_W, "Undelete\nFrame(s)\n ");

  XCB2( delImgfrm_W, "Delete\nSelected\nFrames", undelImgfrm_W);
  XCB2( clearSel_W, "Deselect\nSelected\nFrame(s)", delImgfrm_W);
  XCB2( selImgfrm1_W, "Select\nFrame\n", clearSel_W);

  sep1 = XtVaCreateManagedWidget( "sep1", xmSeparatorWidgetClass, form
       , XmNorientation, XmHORIZONTAL
       , XmNleftAttachment, XmATTACH_FORM
       , XmNrightAttachment, XmATTACH_FORM
       , XmNbottomAttachment, XmATTACH_WIDGET, XmNbottomWidget, undelImgfrm_W
       , NULL);

  sep2 = XtVaCreateManagedWidget( "sep2", xmSeparatorWidgetClass, form
       , XmNorientation, XmVERTICAL
       , XmNbottomAttachment, XmATTACH_FORM
       , XmNtopAttachment, XmATTACH_WIDGET , XmNtopWidget, sep1
       , XmNrightAttachment, XmATTACH_WIDGET, XmNrightWidget, selImgfrm1_W
       , XmNrightOffset, (int) 5
       , NULL);

  lclxmstr = XmStringCreateLtoR("Hide Un-\nselected\nFrames", CHARSET);
  n = 0;
  XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM), n++;
  XtSetArg(args[n], XmNlabelString, lclxmstr), n++;
  XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET), n++;
  XtSetArg(args[n], XmNrightWidget, sep2), n++;

  button = XtCreateManagedWidget( "hideUnselectedFrames"
                                , xmToggleButtonWidgetClass, form, args, n);
  XtAddCallback( button, XmNvalueChangedCallback
               , hideUnselectedFrames_CB, cur_item);
  XmSF( lclxmstr);

  hideUnselectedFrames = 0;
  XmToggleButtonSetState( button, False, False);

  sep2 = XtVaCreateManagedWidget( "sep2", xmSeparatorWidgetClass, form
       , XmNorientation, XmVERTICAL
       , XmNbottomAttachment, XmATTACH_FORM
       , XmNtopAttachment, XmATTACH_WIDGET , XmNtopWidget, sep1
       , XmNrightAttachment, XmATTACH_WIDGET, XmNrightWidget, button
       , XmNrightOffset, (int) 5
       , NULL);


  {
  Widget funRowcol, funButton, funLabel;
    funRowcol = XtVaCreateManagedWidget( "funRowcol"
                                       , xmRowColumnWidgetClass, form
                                       , XmNpacking, XmPACK_COLUMN
                                       , XmNnumColumns, 2
                                       , XmNorientation, XmHORIZONTAL
                                       , XmNbottomAttachment, XmATTACH_FORM
                                       , XmNrightAttachment, XmATTACH_WIDGET
                                       , XmNrightWidget, sep2
                                       , XmNradioBehavior, True
                                       , NULL);

    funButton = XtVaCreateManagedWidget( "Yes"
                                       , xmToggleButtonWidgetClass, funRowcol
                                       , NULL);
    XmToggleButtonSetState( funButton, False, False);

    XtAddCallback( funButton, XmNvalueChangedCallback
                 , orbitguiFun_CB, (XtPointer) 0);

    funButton = XtVaCreateManagedWidget( "No"
                                       , xmToggleButtonWidgetClass, funRowcol
                                       , NULL);
    XtAddCallback( funButton, XmNvalueChangedCallback
                 , orbitguiFun_CB, (XtPointer) 1);
    XmToggleButtonSetState( funButton, True, False);

    funButton = XtVaCreateManagedWidget( "Not Sure"
                                       , xmToggleButtonWidgetClass, funRowcol
                                       , NULL);

    lclxmstr = XmStringCreateLtoR( "Do you want\nto view the\nAsteroid?"
                                 , CHARSET);
    funLabel = XtVaCreateManagedWidget( "funLabel", xmLabelWidgetClass, form
                                      , XmNlabelString, lclxmstr
                                      , XmNbottomAttachment, XmATTACH_FORM
                                      , XmNrightAttachment, XmATTACH_WIDGET
                                      , XmNrightWidget, funRowcol
                                      , NULL);
    XmSF( lclxmstr);
                                       
  }

# ifdef XCB
# undef XCB
# endif

# define XCB(XCBW, XCBL, X_CB) \
  n = 0; \
  XtSetArg( args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++; \
  XtSetArg( args[n], XmNbottomWidget, sep1), n++; \
  XtSetArg( args[n], buildFromRight ? XmNrightAttachment : XmNleftAttachment \
                   , tmp_W ? XmATTACH_WIDGET : XmATTACH_FORM), n++; \
  if ( tmp_W) { \
    XtSetArg( args[n], buildFromRight ? XmNrightWidget : XmNleftWidget \
                     , tmp_W), n++; \
  } \
  tmp_W = XCBW = XtCreateManagedWidget( XCBL, xmPushButtonWidgetClass, form \
                                      , args, n); \
  XtAddCallback( XCBW, XmNactivateCallback, X_CB, cur_item)

  /**********************************/
  /* buttons to walk through frames */

  /*************************************/
  /* utc widgets - scale bars + labels */

  year = 2000; doy = 44; hour = minute = second = 0;
  orbit_init_utc( &utc);
  /* 19990916 UTCLOAD( utc); */
  saved_utc = utc;

  /* horizontal scale bar conglomeration of widgets:
   * - constant text label on left
   * - value label on right
   */

#	define ADDUTCWIDGET( WIDGET, W_NAME, W_MIN, W_MAX, W_BOTTOM, UTCVAL \
			   , LABWIDG, NDIGITS) \
        /* Left of scale label: unchanging text */ \
        sprintf( tmpstr, "%6s", W_NAME); \
	utc_Xmstr= XmStringCreateLtoR(tmpstr, CHARSET); \
	n= 0; \
	XtSetArg(args[n], XmNlabelString, utc_Xmstr), n++; \
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++; \
	XtSetArg(args[n], XmNbottomWidget, W_BOTTOM), n++; \
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM), n++; \
        sprintf( tmpstr, "%s%s", W_NAME, "_leftlabel"); \
	tmp_W = XtCreateManagedWidget(tmpstr,  \
                    xmLabelWidgetClass, form, args, n); \
        XmSF( utc_Xmstr); \
        \
        /* Right of scale label: changing Value */ \
        sprintf( fmtstr, "%%0%dd", NDIGITS); \
        strcpy(fmtstr,"    "); sprintf( fmtstr+4-NDIGITS, "%%0%dd", NDIGITS); \
        sprintf( tmpstr, fmtstr, UTCVAL); \
	utc_Xmstr= XmStringCreateLtoR(tmpstr, CHARSET); \
	n= 0; \
	XtSetArg(args[n], XmNlabelString, utc_Xmstr), n++; \
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++; \
	XtSetArg(args[n], XmNbottomWidget, W_BOTTOM), n++; \
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++; \
        sprintf( tmpstr, "%s%s", W_NAME, "_rightlabel"); \
	LABWIDG = XtCreateManagedWidget( tmpstr,  \
                    xmLabelWidgetClass, form, args, n); \
        XmSF( utc_Xmstr); \
        \
	n= 0; \
	SCALESTEP1; \
	XtSetArg(args[n], XmNlabelType, XmSTRING), n++; \
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL), n++; \
	XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_RIGHT), n++; \
	XtSetArg(args[n], XmNminimum, W_MIN), n++; \
	XtSetArg(args[n], XmNmaximum, W_MAX), n++; \
	XtSetArg(args[n], XmNdecimalPoints, 0), n++; \
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;  \
	XtSetArg(args[n], XmNleftWidget, tmp_W), n++; \
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_WIDGET), n++; \
	XtSetArg(args[n], XmNrightWidget, LABWIDG), n++; \
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++; \
	XtSetArg(args[n], XmNbottomWidget, W_BOTTOM), n++; \
	WIDGET= XtCreateManagedWidget(W_NAME,  \
                    xmScaleWidgetClass, form, args, n); \
	XtAddCallback(WIDGET, XmNvalueChangedCallback, new_utc_CB, cur_item); \
	XtAddCallback(WIDGET, XmNdragCallback, new_utc_CB, cur_item); \
        XmScaleSetValue( WIDGET, UTCVAL); \
	XmUpdateDisplay(WIDGET);

        ADDUTCWIDGET( second_W, "second", SECOND_MIN, SECOND_MAX, sep1
                    , second, second_lab_W, 2);
        ADDUTCWIDGET( minute_W, "minute", MINUTE_MIN, MINUTE_MAX, second_lab_W
                     , minute, minute_lab_W, 2);
        ADDUTCWIDGET( hour_W, "hour", HOUR_MIN, HOUR_MAX, minute_lab_W
                    , hour, hour_lab_W, 2);
	n= 0;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, hour_W), n++;
	sep1 = XtCreateManagedWidget("sep1",
                    xmSeparatorWidgetClass, form, args, n);

        ADDUTCWIDGET( doy_W, "doy", DOY_MIN, DOY_MAX, sep1
                    , doy, doy_lab_W, 3);
        ADDUTCWIDGET( year_W, "year", YEAR_MIN, YEAR_MAX, doy_lab_W
                    , year, year_lab_W, 4);

	MAKEUTCXMSTR( utcstr, utc_Xmstr);
	n= 0;
	XtSetArg(args[n], XmNlabelString, utc_Xmstr), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, year_lab_W), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM), n++;
	utc_label= XtCreateManagedWidget("utc_label"
                    , xmLabelWidgetClass, form, args, n);
        XmSF( utc_Xmstr);

	n= 0;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, utc_label), n++;
	sep1= XtCreateManagedWidget("sep1"
                    , xmSeparatorWidgetClass, form, args, n);

  /**********************************/
  /* buttons to walk through frames */

  buildFromRight = 1;
  tmp_W = (Widget) 0;

  XCB( last_W, "Last", orbitgui_walk_frames_CB);
  XCB( backhalf_W, "Back Time", orbitgui_walk_frames_CB);
  XCB( back1_W, "Back N", orbitgui_walk_frames_CB);

  /* SimpleOptionMenu to determine size of step for Fwd/Back 1/Time */

  {
  XmString s[13], *sptr;
  XmString n;
  int i;

    n = XmStringCreateLocalized("N:");
    for ( (sptr=s), i=1; i<=12; ++i, ++sptr) {
    char ss[10];
      sprintf( ss, "N=%d", i);
      *sptr = XmStringCreateLocalized( ss);
    }
    *sptr = XmStringCreateLocalized( "Other..." );

#   define XMVPB(I,K) XmVaPUSHBUTTON, s[I], K, NULL, NULL

    som_W = XmVaCreateSimpleOptionMenu( form, "option_menu"
      , n, 'N', 0, orbitgui_walkStepSize_CB
      , XMVPB(0,'1'), XMVPB(1,'2'), XMVPB(2,'3'), XMVPB(3,'4')
      , XMVPB(4,'5'), XMVPB(5,'6'), XMVPB(6,'7'), XMVPB(7,'8')
      , XMVPB(8,'9'), XMVPB(9,'0'), XMVPB(10,'B'), XMVPB(11,'C')
      , XMVPB(12,'O')
      , XmNbottomAttachment, XmATTACH_WIDGET
      , XmNbottomWidget, sep1
      , XmNrightAttachment, XmATTACH_WIDGET
      , XmNrightWidget, tmp_W
      , NULL);

#   ifdef _DPR_SOM_W_
    fprintf( stderr, "som_W = %08xx\n", som_W);
#   endif

    tmp_W = som_W;              /* for placement of next button via XCB macro */

    for ( (sptr=s), i=1; i<=12; ++i, ++sptr) { XmSF( *sptr); }
    XmSF( *sptr);
    XmSF( n);
    XtManageChild( som_W);
  }

  walk_timer_id = 1;
  XCB( fwd1_W, "Fwd N", orbitgui_walk_frames_CB);
  XCB( fwdhalf_W, "Fwd Time", orbitgui_walk_frames_CB);
  XCB( first_W, "First", orbitgui_walk_frames_CB);

  /* duplicate *half widgets as *time widgets in walkimgfrm_W[]
   * ***N.B. This means fwd/backtime_W Widgets are not really separate 
   *         widgets from fwd/backhalf_W Widgets, and the flag halfNotTimeSteps
   *         is needed to distinguish between -half & -time
   */
  fwdtime_W = fwdhalf_W;
  backtime_W = backhalf_W;

  /* put arm & disarm callbacks on fwd1_W & back1_W so stepping rate can be
   * somewhat controlled; also on fwd/backtime_W
   * ***N.B. this also puts these callbacks on fwd/backhalf_W
   */

# define SETARMCB(W) \
  XtAddCallback(W, XmNarmCallback, orbitgui_arm_frames_CB, cur_item); \
  XtAddCallback(W, XmNdisarmCallback, orbitgui_arm_frames_CB, cur_item)

  SETARMCB( fwd1_W);
  SETARMCB( back1_W);
  SETARMCB( fwdtime_W);
  SETARMCB( backtime_W);

  /************************************************/
  /* buttons to apply utc, reset utc & mark frame */

  buildFromRight = 0;
  tmp_W = (Widget) 0;

  XCB( button, "Apply UTC", apply_utc_CB);
  XCB( button, "Mark Frame", mark_frame_CB);
  XCB( button, "Reset UTC", reset_utc_CB);

  sep2= XtVaCreateManagedWidget( "sep2", xmSeparatorWidgetClass, form
                               , XmNorientation, XmHORIZONTAL
                               , XmNrightAttachment, XmATTACH_FORM
                               , XmNleftAttachment, XmATTACH_FORM
                               , XmNbottomAttachment, XmATTACH_WIDGET
                               , XmNbottomWidget, som_W
                               , NULL);

  vSepW = XtVaCreateManagedWidget( "sep2", xmSeparatorWidgetClass, form
        , XmNorientation, XmVERTICAL
        , XmNbottomAttachment, XmATTACH_WIDGET , XmNbottomWidget, sep1
        , XmNtopAttachment, XmATTACH_WIDGET , XmNtopWidget, sep2
        , XmNleftAttachment, XmATTACH_WIDGET, XmNleftWidget, button
        , XmNrightAttachment, XmATTACH_WIDGET, XmNrightWidget, first_W
        , NULL);

  sep1 = sep2;

  /************************************************/
  /* widgets to control s/c lat, s/c lon, & noraz */

	n= 0;
	SCALESTEP1;
	XtSetArg(args[n], XmNlabelType, XmSTRING), n++;
	XtSetArg(args[n], XmNorientation, XmVERTICAL), n++;
	XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_BOTTOM), n++;
	XtSetArg(args[n], XmNminimum, (int)floor((PHI_MIN/pow_phi) + .5)), n++;
	XtSetArg(args[n], XmNmaximum, (int)floor((PHI_MAX/pow_phi) + .5)), n++;
	XtSetArg(args[n], XmNdecimalPoints, decimal_phi), n++;
	XtSetArg(args[n], XmNshowValue, True), n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNtopWidget, phi_label), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, sep1), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM), n++;
	phi_W= XtCreateManagedWidget("phi", xmScaleWidgetClass, form, args, n);
	XtAddCallback(phi_W, XmNvalueChangedCallback, new_phi_CB, NULL);
	XtAddCallback(phi_W, XmNdragCallback, new_phi_CB, NULL);
	old_phi= 0;

	n= 0;
	XtSetArg(args[n], XmNlabelString, theta_str), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, sep1), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, phi_W), n++;
	theta_label= XtCreateManagedWidget("theta_label"
                     , xmLabelWidgetClass, form, args, n);

	n= 0;
	SCALESTEP1;
	XtSetArg(args[n], XmNlabelType, XmSTRING), n++;
	XtSetArg(args[n], XmNorientation, XmHORIZONTAL), n++;
	XtSetArg(args[n], XmNprocessingDirection, XmMAX_ON_RIGHT), n++;
	XtSetArg(args[n], XmNminimum, (int)floor((THETA_MIN/pow_theta) + .5))
                        , n++;
	XtSetArg(args[n], XmNmaximum, (int)floor((THETA_MAX/pow_theta) + .5))
                        , n++;
	XtSetArg(args[n], XmNdecimalPoints, decimal_theta), n++;
	XtSetArg(args[n], XmNshowValue, True), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, theta_label), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, sep1), n++;
	theta_W= XtCreateManagedWidget("theta"
                    , xmScaleWidgetClass, form, args, n);
	XtAddCallback(theta_W, XmNvalueChangedCallback, new_theta_CB, NULL);
	XtAddCallback(theta_W, XmNdragCallback, new_theta_CB, NULL);
	old_theta= 0;

  /********************/
  /* the drawing area */

	n= 0;
	drawing_area= XmCreateDrawingArea(form, "drawing_area", args, n);
	XtManageChild(drawing_area);
	n= 0;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET), n++;
	/* XtSetArg(args[n], XmNtopWidget, Noraz_W), n++; */
	XtSetArg(args[n], XmNtopWidget, rho_W), n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNleftWidget, phi_W), n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM), n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET), n++;
	XtSetArg(args[n], XmNbottomWidget, theta_W), n++;
	XtSetValues(drawing_area, args, n);
        /* callbacks:  button 1 down & up; button 3 drag */
        {
        XtAppContext appcon = XtWidgetToApplicationContext( drawing_area);
        XtActionsRec actions;
        String trans
          = "<Btn2Motion>: drawInput_CB(2) ManagerGadgetButtonMotion()\n\
             <Btn2Up>: drawInput_CB(2) ManagerGadgetActivate()\n\
             <Btn2Down>: drawInput_CB(2) ManagerGadgetActivate()\n\
             <Btn3Up>: drawInput_CB(3) ManagerGadgetActivate()\n\
             <Btn3Down>: drawInput_CB(3) ManagerGadgetActivate()\n\
             <Btn1Up>: drawInput_CB(1) ManagerGadgetActivate()\n\
             <Btn1Motion>: drawInput_CB(1) ManagerGadgetActivate()\n\
             <Btn1Down>: drawInput_CB(1) ManagerGadgetActivate()\n\
            ";
          actions.string = "drawInput_CB";
          actions.proc = drawInput_CB;
          XtAppAddActions( appcon, &actions, 1);
	  XtVaSetValues( drawing_area
            , XmNtranslations, XtParseTranslationTable( trans)
            , NULL);
        }

	XtAddCallback(drawing_area, XmNexposeCallback, expose_CB, NULL);
	XtAddCallback(drawing_area, XmNresizeCallback, resize_CB, NULL);

	XtRealizeWidget(app_shell);

	da_dpy= XtDisplay(drawing_area);
	da_win= XtWindow(drawing_area);

#	define XCGC(XGC) XGC = XCreateGC(da_dpy, da_win, 0, (XGCValues *) NULL )

	XCGC( fgc);
	XCGC( bgc);
	XCGC( R_gc);
	XCGC( G_gc);
	XCGC( B_gc);
	XCGC( Y_gc);
	XCGC( C_gc);

        if ( getenv( "ORBIT_CYANWIDTH") ) {
          if ( 1 == sscanf( getenv("ORBIT_CYANWIDTH"), "%d", &xgcv.line_width)){
            xgcv.join_style = JoinRound;
            XChangeGC(da_dpy, C_gc, GCLineWidth|GCJoinStyle, &xgcv );
            xgcv.line_width = 0;
            xgcv.join_style = 0;
          }
        }

	for ( n=0; n<5; n++) {
	  XCGC( GO_gc[n]);
	}

        /* dashed lines for magenta (current frame) & gray (shape segments) */

	xgcv.line_style = LineOnOffDash;
	M_gc= XCreateGC(da_dpy, da_win, GCLineStyle, &xgcv );
	SHAPE_gray_gc= XCreateGC(da_dpy, da_win, GCLineStyle, &xgcv );

#	define XSFG( XGC, GCCOLOR) \
	XSetForeground(da_dpy, XGC, set_color(GCCOLOR, cur_item))

	XSFG( fgc, "white");
	XSFG( bgc, "black");
	XSFG( R_gc, "red");
	XSFG( G_gc, "green");
	XSFG( B_gc, "blue");
	XSFG( Y_gc, "yellow");
	XSFG( M_gc, "magenta");
	XSFG( C_gc, "cyan");

#	if XtSpecificationRelease < 5
#	define GOLD      "gold"
#	define GOLD4     "gold4"
#	define GROD      "goldenrod"
#	define GROD4     "goldenrod4"
#	define LTGROD    "light goldenrod"
#	define GRAY      "gray"
#	else
#	define GOLD      "#d4a017" /* #d4a017 212 160  23   gold */
#	define GOLD4     "#806517" /* #806517 128 101  23   gold4 */
#	define GROD      "#edda74" /* #edda74 237 218 116   goldenrod */
#	define GROD4     "#805817" /* #805817 128  88  23   goldenrod4 */
#	define LTGROD    "#ecd872" /* #ecd872 236 216 114   light goldenrod */
#	define GRAY      "#736f6e" /* #736f6e 115 111 110   gray */
#	endif

	XSFG( SHAPE_gray_gc, GRAY);

        switch( linestyle) {
        case LINESTYLE_WHITE: SHAPE_gc = fgc; break;
        case LINESTYLE_GRAY: SHAPE_gc = SHAPE_gray_gc; break;
        default: break;
        }

	XSFG( GO_gc[0], GOLD);
	XSFG( GO_gc[1], GOLD4);
	XSFG( GO_gc[2], GROD);
	XSFG( GO_gc[3], GROD4);
	XSFG( GO_gc[4], LTGROD);

        orbitgui_set_rainbow( cur_item);

        /* clean up */

        XmSF( phi_str);
        XmSF( theta_str);
        XmSF( Noraz_str);

        /* ensure R set properly before any draw_image calls */
	identity_matrix3d(R);
        /* rotate -90 theta so 0 longitude points at viewer */
        R[0][0] = R[1][1] = 0.;
        R[1][0] = -1.;
	R[0][1] = 1.;

	return;
}

/**********************************************************/
void
new_Noraz_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
int	new_Noraz;
FIND_ITEM( w, Noraz_W)

  XmScaleGetValue(w, &new_Noraz);
  if ( display_northup) old_Noraz = new_Noraz; /* see update_phi() */
  else update_phi( old_phi, old_theta, new_Noraz, cur_item);
  return;
}

/**********************************************************/
void
new_phi_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
int		new_phi;
double		phi;
Dimension	highlight, height, width;
Display		*dpy;
Window		win, root, child;
int		dy, root_x, root_y, win_x, win_y;
unsigned int	mask;
FIND_ITEM( w, phi_W)

  XmScaleGetValue(w, &new_phi);
  update_phi( new_phi, old_theta, old_Noraz, cur_item);
  return;

/* Note: The Motif 1.1 scale slider seems to have a hardwired "length" of 28
   pixels. The y-offsets of 33 and 34 pixels used below were determined
   empirically. The x-offset of 7 pixels depends on the details of the scale
   widget and may need to changed. */

/*	if ((phi= (double)new_phi*pow_phi) > PHI_WRAP_HI) {
/*		old_phi= (int)(floor(0.5 + PHI_WRAP_LO +
/*			phi - PHI_WRAP_HI)/pow_phi);
/*		XQueryPointer(dpy= XtDisplay(w), win= XtWindow(w), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
/*		n= 0;
/*		XtSetArg(args[n], XmNhighlightThickness, &highlight), n++;
/*		XtSetArg(args[n], XmNheight, &height), n++;
/*		XtSetArg(args[n], XmNwidth, &width), n++;
/*		XtGetValues(w, args, n);
/*		dy= (int)floor((double)(height - 2*highlight - 33)*18./19. + .5);
/*
/*		XWarpPointer(dpy, None, win, 0, 0, 0, 0, width-7-highlight, win_y-dy);
/*		XmScaleSetValue(w, old_phi);
/*		XmUpdateDisplay(w);
/*		XSync(dpy, True), XFlush(dpy);
/*	}
/*	else if (phi < PHI_WRAP_LO) {
/*		old_phi= (int)(floor( 0.5 + PHI_WRAP_HI +
/*			phi - PHI_WRAP_LO)/pow_phi);
/*		XQueryPointer(dpy= XtDisplay(w), win= XtWindow(w), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
/*		n= 0;
/*		XtSetArg(args[n], XmNhighlightThickness, &highlight), n++;
/*		XtSetArg(args[n], XmNheight, &height), n++;
/*		XtSetArg(args[n], XmNwidth, &width), n++;
/*		XtGetValues(w, args, n);
/*		dy= (int)floor((double)(height - 2*highlight - 34)*18./19. + .5);
/*
/*		XWarpPointer(dpy, None, win, 0, 0, 0, 0, width-7-highlight, win_y+dy);
/*		XmScaleSetValue(w, old_phi);
/*		XmUpdateDisplay(w);
/*		XSync(dpy, True), XFlush(dpy);
/*	}
*/

}

/*****************************************************************
 * update_phi() - update old_{theta,phi,Noraz} & redraw if changed
 *****************************************************************/

void
update_phi( new_phi, new_theta, new_Noraz, cur_item)
int	new_phi, new_theta, new_Noraz;
ITEM *cur_item;
{
short		i, j, k;
double		phi, theta, lNoraz;
int		doimg=0;

  if (new_theta - old_theta) {
    doimg = 1;
    old_theta = new_theta;
  }

  if (new_phi - old_phi) {
    doimg = 1;
    old_phi= new_phi;
  }

  /* disable setting doimg via Noraz test if northup state is set
   * i.e. assume Noraz does not change as long as northup state is set
   * ***N.B. this means that when SETTING northup state, northup_CB()
   *           (the callback that sets northup state) should
   *           (1) temporarily *CLEAR* northup state if/when it calls
   *               update_phi() with Noraz set to 0, otherwise the change
   *               in Noraz will be ignored, and
   *           (2) save old_Noraz because update_phi() will reset it
   * ***N.B. in like fashion, when CLEARING northup state, northup_CB()
   *           should set old_Noraz to 0 or else the now-noticed Noraz
   *           may be ignored anyway
   * ***N.B. old_Noraz will not be updated to new_Noraz if northup state
   *         is set, so new_Noraz_CB() should do that itself
   */

  if ((new_Noraz - old_Noraz) && !display_northup) {
    doimg = 1;
    old_Noraz = new_Noraz;
  }

  if ( !doimg) return;

  /* set Noraz to 0 if northup state is set */
  if ( display_northup) new_Noraz = 0;

  /* make new_theta absolute,
   * adjust for east/west longitude,
   * and subtract 90 degrees from value */

  identity_matrix3d(R);
  theta = ((-spudf->eastlon * new_theta * pow_theta) - 90.) deg;

  /* this is the "theta" rotation: rotates about the local vertical z-axis */
  identity_matrix3d(new_dR);
  new_dR[0][0]= new_dR[1][1]= cos(theta);
  new_dR[1][0]= sin(theta), new_dR[0][1]= -new_dR[1][0];
  for (i= 0; i < 3; i++) {
    for (j= 0; j < 3; j++) {
      new_R[i][j]= 0.;
      for (k= 0; k < 3; k++) new_R[i][j]+= new_dR[i][k]*R[k][j];
    }
  }

  memcpy((void *)R, (void *)new_R, size_R);

  /* make new_phi absolute */
  phi= new_phi deg;
  phi*= pow_phi;

  /* this is the "phi" rotation: rotates about a "horizontal" x-axis */
	identity_matrix3d(new_dR);
  /* this is the "phi" rotation: rotates about a "horizontal" x-axis */
  new_dR[1][1]= new_dR[2][2]= cos(phi);
  new_dR[2][1]= sin(phi), new_dR[1][2]= -new_dR[2][1];

  for (i= 0; i < 3; i++) {
    for (j= 0; j < 3; j++) {
      new_R[i][j]= 0.;
      for (k= 0; k < 3; k++) new_R[i][j]+= new_dR[i][k]*R[k][j];
    }
  }
  memcpy((void *)R, (void *)new_R, size_R);

  /* make new_Noraz absolute */
  lNoraz= new_Noraz deg;
  lNoraz*= pow_Noraz;

  /* "Noraz" rotation:  rotates about an "into-the-image" y-axis
                        *** N.B. Clockwise Rotation, CW from UP */
  identity_matrix3d(new_dR);
  new_dR[0][0]= new_dR[2][2]= cos(lNoraz);
  new_dR[0][2]= sin(lNoraz), new_dR[2][0]= -new_dR[0][2];

  for (i= 0; i < 3; i++) {
    for (j= 0; j < 3; j++) {
      new_R[i][j]= 0.;
      for (k= 0; k < 3; k++) new_R[i][j]+= new_dR[i][k]*R[k][j];
    }
  }
  memcpy((void *)R, (void *)new_R, size_R);

  draw_image( cur_item, Drawdev_xwin);
        return;
}
/**********************************************************/
void
new_theta_CB(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
int new_theta;
double theta;
Dimension highlight, height, width;
Display *dpy;
Window win, root, child;
int dx, root_x, root_y, win_x, win_y;
unsigned int mask;

FIND_ITEM( w, theta_W)

  XmScaleGetValue(w, &new_theta);
  update_phi( old_phi, new_theta, old_Noraz, cur_item);

/*  if (!XmToggleButtonGetState(wrap_around_TB)) */
    return;

/*  /* Note: The Motif 1.1 scale slider seems to have a hardwired "length" of 28 pixels.
/*     The x-offsets of 35 and 36 pixels used below were determined empirically.
/*     The y-offset of 7 pixels depends on the details of the scale widget and may need to changed.
/*  */
/*  if ((theta= (double)new_theta*pow_theta) > THETA_WRAP_HI) {
/*    old_theta= (int)(floor(THETA_WRAP_LO + theta - THETA_WRAP_HI + 0.5)/pow_theta);
/*    XQueryPointer(dpy= XtDisplay(w), win= XtWindow(w), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
/*    n= 0;
/*    XtSetArg(args[n], XmNhighlightThickness, &highlight), n++;
/*    XtSetArg(args[n], XmNheight, &height), n++;
/*    XtSetArg(args[n], XmNwidth, &width), n++;
/*    XtGetValues(w, args, n);
/*    dx= (int)floor((double)(width - 2*highlight - 35)*18./19. + .5);
/*
/*    XWarpPointer(dpy, None, win, 0, 0, 0, 0, win_x-dx, height-7-highlight);
/*    XmScaleSetValue(w, old_theta);
/*    XmUpdateDisplay(w);
/*    XSync(dpy, True), XFlush(dpy);
/*  }
/*  else if (theta < THETA_WRAP_LO) {
/*    old_theta= (int)(floor(theta + THETA_WRAP_HI + 0.5 - THETA_WRAP_LO)/pow_theta);
/*    XQueryPointer(dpy= XtDisplay(w), win= XtWindow(w), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
/*    n= 0;
/*    XtSetArg(args[n], XmNhighlightThickness, &highlight), n++;
/*    XtSetArg(args[n], XmNheight, &height), n++;
/*    XtSetArg(args[n], XmNwidth, &width), n++;
/*    XtGetValues(w, args, n);
/*    dx= (int)floor((double)(width - 2*highlight - 36)*18./19. + .5);
/*
/*    XWarpPointer(dpy, None, win, 0, 0, 0, 0, win_x+dx, height-7-highlight);
/*    XmScaleSetValue(w, old_theta);
/*    XmUpdateDisplay(w);
/*    XSync(dpy, True), XFlush(dpy);
/*  }
*/

}

/**********************************************************/
void
new_rho_CB(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
int new_rho, d_rho;
double frac_rho;
Dimension highlight, height, width;
Display *dpy;
Window win, root, child;
int dx, root_x, root_y, win_x, win_y;
unsigned int mask;
XmString rho_str;

FIND_ITEM( w, rho_W)

  /* do a pointer query as soon as possible (in case geometric size rescaling 
   * needed)
   */
  XQueryPointer(dpy= XtDisplay(w), win= XtWindow(w), &root, &child, &root_x, &root_y, &win_x, &win_y, &mask);
  XmScaleGetValue(w, &new_rho);
	if (new_rho == old_rho)
		return;

	d_rho= new_rho-old_rho;
	old_rho= new_rho;
	rho= pow( 10.0, (double)new_rho*pow_rho);

        RHO_FMT( tmpstr, rho);
        rho_str =  XmStringCreateLtoR( tmpstr, XmSTRING_DEFAULT_CHARSET);
        XtVaSetValues( rho_label, XmNlabelString, rho_str, NULL);
        XmUpdateDisplay( rho_label);
        XFlush( da_dpy);
        XmSF(rho_str);

	draw_image( cur_item, Drawdev_xwin);

/* 	if (!XmToggleButtonGetState(geometric_size_TB)) */
		return;

/*	/* Note: The Motif 1.1 scale slider seems to have a hardwired "length" of 28 pixels.
/*	   The y-offset of 7 pixels depends on the details of the scale widget and may need to changed.
/*	*/
/*	if (d_rho > 0 && rho > .95*rho_max) {
/*		rho_max*= 2.;
/*		frac_rho= sqrt(.95)*rho/rho_max;
/*		n= 0;
/*		XtSetArg(args[n], XmNhighlightThickness, &highlight), n++;
/*		XtSetArg(args[n], XmNheight, &height), n++;
/*		XtSetArg(args[n], XmNwidth, &width), n++;
/*		XtGetValues(w, args, n);
/*		n= 0;
/*		XtSetArg(args[n], XmNmaximum, (int)floor((rho_max/pow_rho) + .5)), n++;
/*		/* XmNscaleMultiple apparently does not reset; may want to change that too
/*		*/
/*		XtSetValues(rho_W, args, n);
/*		dx= min(win_x, width - 2*highlight) - (int)floor((double)(width - 2*highlight - 28)*frac_rho + .5);
/*
/*		XWarpPointer(dpy, None, win, 0, 0, 0, 0, dx, height-7-highlight);
/*		XmScaleSetValue(w, old_rho);
/*		XmUpdateDisplay(w);
/*	}
/*	else if (d_rho < 0 && rho < .25*rho_max) {
/*		frac_rho= rho/rho_max;
/*		rho_max/= 2.;
/*		n= 0;
/*		XtSetArg(args[n], XmNhighlightThickness, &highlight), n++;
/*		XtSetArg(args[n], XmNheight, &height), n++;
/*		XtSetArg(args[n], XmNwidth, &width), n++;
/*		XtGetValues(w, args, n);
/*		n= 0;
/*		XtSetArg(args[n], XmNmaximum, (int)floor((rho_max/pow_rho) + .5)), n++;
/*		/* XmNscaleMultiple apparently does not reset; may want to change that too
/*		*/
/*		XtSetValues(rho_W, args, n);
/*		dx= max(win_x, 1+highlight) + (int)floor((double)(width - 2*highlight - 28)*frac_rho + .5);
/*
/*		XWarpPointer(dpy, None, win, 0, 0, 0, 0, dx, height-7-highlight);
/*		XmScaleSetValue(w, old_rho);
/*		XmUpdateDisplay(w);
/*	}
*/

}

/**********************************************************/
void
exit_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
	XtCloseDisplay(XtDisplay(w));
	exit(0);
}

/**********************************************************/
void
general_help_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
	Widget	create_help();

	XtManageChild(create_help(w, 0));
}

/**********************************************************/
void
scale_help_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
	Widget	create_help();

	XtManageChild(create_help(w, 1));
}

/**********************************************************/
void
opts_help_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
	Widget	create_help();

	XtManageChild(create_help(w, 2));
}

/**********************************************************/
Widget
create_help(parent, help_op)
Widget	parent;
short	help_op;
{
	Widget		button, message_box;
	static char	message[BUFSIZ];
	XmString	title_string= NULL, message_string= NULL, button_string= NULL;

	switch (help_op) {
	case 0:
	sprintf(message, "\
This application draws a wire-frame icosahedron with\n\
right-handed coordinate axes, where:\n\
        +x axis = red,\n\
        +y axis = green,\n\
        +z axis = blue.\n\
Initially, the +x axis is to your right, the +y axis is\n\
into the screen, and the +z axis is up.\n\n\
The image can be rotated about the current horizontal\n\
or current verical axis, resized, and have a perspective\n\
viewing transformation applied.\n\n\
The image can be translated by using a mouse button press\n\
in the drawing area for the image, followed by a mouse\n\
translate, and then a mouse button release.  (This trans-\n\
lation is initially set to be zero).\n\n\
  Note: A hidden-surface algorithm is NOT applied.\0");
		break;
	case 1:
	sprintf(message, "\
The \"size\" scale bar changes the apparent size.\n\
The \"phi\" scale bar rotates about the horizontal axis.\n\
The \"theta\" scale bar rotates about the vertical axis.\n\
The \"1/D\" scale bar changes the perspective distortion.\0");
		break;
	case 2:
	sprintf(message, "\
\"centered axes\": with a non-zero translation, the axes\n\
   are toggled to be centered/non-centered\n\n\
\"perspective\": perspective view toggled on/off\n\n\
\"double buffering\": pseudo-\"double buffering\"\n\
   toggled on/off\n\n\
\"geometric size rescaling\": geometric\n\
   contraction/expansion of the \"size\"\n\
   scale bar toggled on/off  (Note: When using the\n\
   \"size\" scale bar in this mode, move the cursor\n\
   relatively slowly or it will not function correctly.)\n\n\
\"wrap around rotations\": wrap-around of rotational\n\
   scale bars toggled on/off\0");
		break;
	}

	message_string= XmStringCreateLtoR(message, CHARSET);
	button_string= XmStringCreateLtoR("Continue...", CHARSET);
	title_string= XmStringCreateLtoR("3d help", CHARSET);

	n= 0;
	XtSetArg(args[n], XmNdialogTitle, title_string), n++;
	XtSetArg(args[n], XmNokLabelString, button_string), n++;
	XtSetArg(args[n], XmNmessageString, message_string), n++;
	message_box= XmCreateMessageDialog(parent, "helpbox", args, n);

	button= XmMessageBoxGetChild(message_box, XmDIALOG_CANCEL_BUTTON);
	XtUnmanageChild(button);
	button= XmMessageBoxGetChild(message_box, XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(button);

	XmSF(title_string);
	XmSF(message_string);
	XmSF(button_string);
	return (message_box);
}

/**********************************************************/
/* XmDrawingAreaCallbackStruct defined in /usr/include/Motif1.1/Xm/Xm.h */
void
expose_CB(Widget w, XtPointer client_data, XmDrawingAreaCallbackStruct *_draw)
{

FIND_ITEM( w, drawing_area)

  /* _draw->reason should be XmCR_EXPOSE */
  if (initial) {
    set_area(_draw->window, cur_item);
    initial= (Boolean)False;
    draw_image( cur_item, Drawdev_xwin);
  } else {
    XCopyArea(da_dpy, da_pm, da_win, fgc, 0, 0, da_width, da_height, 0, 0);
    XSync(da_dpy, 0);
    XFlush(da_dpy);
  }
}

/**********************************************************/
void
resize_CB(w, client_data, _draw)
Widget	w;
XtPointer	client_data;
XmDrawingAreaCallbackStruct	*_draw;
{
	int	old_width, old_height;

FIND_ITEM( w, drawing_area)


	/* _draw->reason should be XmCR_RESIZE
	*/
/*	expose_CB( w, client_data, _draw);  /* just for fun */
	if (!initial && _draw->reason == XmCR_RESIZE) {
		old_width= da_width;
		old_height= da_height;
		set_area(_draw->window, cur_item);
		draw_image( cur_item, Drawdev_xwin);

		/* update any size-dependent drawing area parameters; how one 
                 * decides to update Tx and Ty really depends on what one wants
		 * (i.e., there really is no "right" way to do this)
		 */
		Tx= (int)floor((double)(Tx*da_width)/(double)old_width + .5);
		Ty= (int)floor((double)(Ty*da_height)/(double)old_height + .5);
	}
}

/**********************************************************/
void
set_area(window, cur_item)
Window	window;
ITEM *cur_item;
{
	XWindowAttributes	xwa;

	if (da_pm)
		XFreePixmap(da_dpy, da_pm);
	XGetWindowAttributes(da_dpy, window, &xwa);

	if (!(da_width= xwa.width)) {
		fprintf(stderr, "drawing area has zero width!\n");
		exit(-1);
	}
	if (!(da_height= xwa.height)) {
		fprintf(stderr, "drawing area has zero height!\n");
		exit(-1);
	}

	/* the divisor below controls the relation between da_size and rho;
	   a divisor of 4. means that at rho == 1., 4 unit lengths will span the
	   minimum dimension of the drawing area
	   *** changed 4 to 2.2 1.Nov, 1996
	*/
	da_size= (double)((da_width < da_height) ? da_width : da_height)
                  / ( 2.2 * maxrad) ;

        /* add extra space at bottom of da_pm for comments */

#define DYY (fontInfo->ascent + fontInfo->descent)

        da_height_pm = da_height + (DYY*3)/2 + (DYY*15) + fontInfo->descent;
        da_pm_depth = DefaultDepth(da_dpy, DefaultScreen(da_dpy));
	da_pm= XCreatePixmap( da_dpy, da_win
                            , da_width, da_height_pm, da_pm_depth);
}

/**********************************************************/
static char saveAbfText[256];
static long saveAbfTextLength;

/**********************************************************/
/* placeholder to ensure caller does not try to XtFree pointers
 * from drawingAreaConvertProc
 */
static void
drawingAreaSelectionDoneProc( Widget w, Atom *sel, Atom *targ) {
  return;
}

/**********************************************************/
static Boolean /**/
drawingAreaConvertProc( Widget w, Atom *sel, Atom *targ
                      , Atom *typOut, XtPointer *valOut, unsigned long *lOut
                      , int *formatOut) {
Display *d = XtDisplay(w);

  if ( (*lOut = ((long)strlen(saveAbfText))) == 0L ) return False;

  if (  *targ == XA_TARGETS(XtDisplay(w)) ) {
  static Atom rtnAtom[2];
    rtnAtom[0] = XA_STRING;
    rtnAtom[1] = XA_LENGTH(d);
    *typOut = XA_ATOM;
    *valOut = rtnAtom;
    *lOut = 2L;
    *formatOut = 8 * sizeof(Atom);
    
  } else if ( *targ == XA_STRING) {
    *typOut = XA_STRING;
    *valOut = (XtPointer *) saveAbfText;
    *formatOut = 8 * sizeof(char);

  } else if ( *targ == XA_LENGTH(d)) {
    saveAbfTextLength = strlen( saveAbfText);
    *typOut = XA_INTEGER;
    *valOut = (XtPointer *) &saveAbfTextLength;
    *lOut = 1L;
    *formatOut = 8 * sizeof(long);
  } else {

    return False;
  }
  return True;
}

/**********************************************************/
#define OVERLAPRECT( DEL, SRC, DST, SIZ, SRCLIM, DSTLIM) \
  SRC = 0; DST = (DEL); SIZ = SRCLIM;        /* assume whole thing can move */ \
  if ( DST < 0) { SRC -= DST; DST = 0; } \
  if ( (SRC+SIZ) > SRCLIM) SIZ = SRCLIM - SRC; \
  if ( (DST+SIZ) > DSTLIM) SIZ = DSTLIM - DST
/**********************************************************/
void
drawInput_CB(Widget w, XEvent *event, String *args, int *argc)
{
XButtonEvent *evt = (XButtonEvent *) event;

FIND_ITEM( w, drawing_area)

  if ( *argc == 1 && event != (XEvent *)NULL) {

    /* Button1 press & release - translate display */
    if ( **args == '1' ) {

      if ( event->type == ButtonPress && ongoingNoDrag) {

        dTx= evt->x, dTy= evt->y;
        ongoingDrag = DRAG_XLATE;

      } else if ( ongoingXlateDrag && event->type == ButtonRelease) {

        Tx+= evt->x - dTx, Ty+= evt->y - dTy;
        draw_image( cur_item, Drawdev_xwin);
        ongoingDrag = DRAG_NONE;

      } else if ( ongoingXlateDrag && event->type == MotionNotify) {
      int xSrc, ySrc, dx, dy, xDst, yDst;

        OVERLAPRECT( evt->x - dTx, xSrc, xDst, dx, da_width, da_width);
        OVERLAPRECT( evt->y - dTy, ySrc, yDst, dy, da_height_pm, da_height);

        if ( dx <= 0 || dy <= 0) {             /* blank da_win for no overlap */

          XFillRectangle( da_dpy, da_win, bgc, 0, 0, da_width, da_height);

        } else {                           /* blank non-overlap, copy overlap */

          if ( xDst > 0) 
            XFillRectangle( da_dpy, da_win, bgc, 0, 0, xDst, da_height);
          else if ( (xDst+dx) <= da_width)
            XFillRectangle( da_dpy, da_win, bgc
                          , xDst+dx, 0, da_width-(xDst+dx), da_height);

          if ( yDst > 0) 
            XFillRectangle( da_dpy, da_win, bgc, 0, 0, da_width, yDst);
          else if ( (yDst+dy) <= da_height)
            XFillRectangle( da_dpy, da_win, bgc
                          , 0, yDst+dy, da_width, da_height-(yDst+dy));

          XCopyArea(da_dpy, da_pm, da_win, fgc, xSrc, ySrc, dx, dy, xDst, yDst);
        }
        XSync( da_dpy, 0);
      }

    /* Button 2 press, release, and motion
     * - adjust UTC members' values
     * - which member depends on vertical position of cursor
     *   - drawing_area split up into 5 horizontal bands
     *     - top band is year
     *     - bottom band is seconds
     */

    } else if ( **args == '2') {
    int i;
    int dstx;
    int zero = 0;
    unsigned int uzero = 0;

    /* pointers to UTC members & outer limits (1 past ends of valid range)
     */
    int *ints[5] = { &year, &doy, &hour, &minute, &second };
    int lims[5][2] = { 1949, 2101, 0, 367, -1, 24, -1, 60, -1, 60 };

    /* UTC member index, value in limits
     */
    int iwhichcurr, ivalcurr;

#define LOLIM lims[iwhichcurr][0]
#define HILIM lims[iwhichcurr][1]
#define SCALEWID (1+HILIM-LOLIM)


      /* convert from relative position in display to UTC member RELATIVE value
       *  ->y => iwhichcurr i.e. which UTC member
       *                    i.e. ints/lims index (y, doy, h, min, s)
       *  ->x => ivalcurr i.e. value in range of lims[iwhichcurr][]
       * - RELATIVE - (ivalcurr - UTC-member-value) is dependent on both
       *              - initial ivalcurr when cursor enters band
       *              - initial UTC member value
       */
      iwhichcurr = (evt->y*5) / da_height;
      iwhichcurr = (iwhichcurr<0)?0:((iwhichcurr>4)?4:iwhichcurr);
      ivalcurr = LOLIM + ( (evt->x * SCALEWID) / da_width );
      ivalcurr = (ivalcurr<LOLIM)?LOLIM:((ivalcurr>HILIM)?HILIM:ivalcurr);

      /* if we're at the limit, warp cursor from one side to the other */

      for ( i=0; i<2; ++i) {
        if ( ivalcurr == lims[iwhichcurr][i]) {

          dTx = lims[iwhichcurr][1-i]; /* set last value to limit */
          ivalcurr = dTx + (2*i) - 1;  /* set current value 1 inside limit */

          /* convert to window x & warp
           * - resultant MotionNotify event should not cause a problem because
           *   dTx will be reset to warped value at end of all of this
           */
          dstx = (int) ( ((ivalcurr+0.5-LOLIM) * da_width / SCALEWID) + 0.5);
          XWarpPointer( da_dpy
                      , None, da_win, zero, zero, uzero, uzero
                      , dstx, evt->y);
        }
      }

      /* change UTC member value iff
       * - this is not initial button 2 down event
       * - cursor moves within band
       */
      if ( event->type != ButtonPress &&
           dTy == iwhichcurr && dTx != ivalcurr) {
      int *ival = ints[iwhichcurr];

        *ival += (ivalcurr - dTx);    /* HERE'S THE BEEF */

        /* handle rollover/rollunder */
        for ( i=iwhichcurr; i>0; --i) {
        int dlim;
          ival = ints[i];
          dlim = lims[i][1] - (lims[i][0] + 1);
          while ( *ival >= lims[i][1]) { *ival -= dlim; (*ints[i-1])++; }
          while ( *ival <= lims[i][0]) { *ival += dlim; (*ints[i-1])--; }
        }
        orbitgui_set_utc( cur_item);
      }

      dTx = ivalcurr, dTy = iwhichcurr;

    /* end of else if **args == '2' */

    } else if ( **args == '3') {                    /* button 3 - find vector */
    double kmX, kmZ;
    VEC viewer, viewDir, viewed;
    double dist;
    unsigned long ipHit;
    MTX lclmtx;
    long i, j, k;

      /* Xpixels = increase right
       * Ypixels = increase down
       * Zpixels = increase into screen (right hand rule)
       */
      if ( event->type == ButtonPress) {

        LOADVEC(  (evt->x - saveCtr.x) / savePxlPerKm       /* Xpixels -> Xkm */
               ,  -2.0 * maxrad                             /* Zpixels -> Ykm */
               ,  (saveCtr.y - evt->y) / savePxlPerKm      /* -Ypixels -> Zkm */
               ,  viewer);
        LOADVEC( 0.0, maxrad, 0.0, viewDir);

        for ( k=i=0; i<3; ++i) for ( j=0; j<3; ++j, ++k) lclmtx[k] = R[j][i];
        vxm( viewer, lclmtx, viewer);
        vxm( viewDir, lclmtx, viewDir);
        spudf_intersect( spudf, viewer, viewDir, &dist, &ipHit);
        if ( ipHit < spudf->nface) {
        XmString clip_lbl;
        int status;
        unsigned long item_id = 0;
        double incid, emiss;
        VEC uvToSun, uvToSc;
          vmxpb( dist, viewDir, viewer, viewed);
          VMINUS2( sunFromAstAbf, viewed, uvToSun);
          vhat( uvToSun, uvToSun);
          VMINUS2( scFromAstAbf, viewed, uvToSc);
          vhat( uvToSc, uvToSc);
          incid = VDOT( uvToSun, spudf->uvnorms+(3*ipHit));
          emiss = VDOT( uvToSc, spudf->uvnorms+(3*ipHit));
#         define ACOSWLIM(B) ((B>1.0) ?0.0 :((B<-1.0) ? 180.0 : (acos(B) rad)))
          incid = ACOSWLIM(incid);
          emiss = ACOSWLIM(emiss);
          sprintf( saveAbfText
                 , "%lf %lf %lf ;plt,color,i,e=%ld %.4lf %.1lf %.1lf"
                 , viewed[0], viewed[1], viewed[2]
                 , spudf->platenum[ipHit], spudf->platecolor[ipHit]
                 , incid, emiss
                 );
          saveAbfTextLength = strlen( saveAbfText);
          XDrawString( da_dpy, da_win, fgc, evt->x, evt->y
                     , saveAbfText, saveAbfTextLength);
          fprintf( stderr, "%s\n", saveAbfText);

          XtOwnSelection( drawing_area, XA_PRIMARY, evt->time
                        , drawingAreaConvertProc
                        , NULL
                        , drawingAreaSelectionDoneProc);
          /*
#         define WHILOCKED( A) do {status = A;} while (status==ClipboardLocked)
          clip_lbl = XmStringCreateLocalized( "ABF_Coordinates");
          WHILOCKED( XmClipboardStartCopy( da_dpy, da_win, clip_lbl
                                         , CurrentTime, NULL, NULL, &item_id) );
          WHILOCKED( XmClipboardCopy( da_dpy, da_win, item_id, "STRING"
                                    , saveAbfText, saveAbfTextLength, 0, NULL));
          WHILOCKED( XmClipboardEndCopy( da_dpy, da_win, item_id) );
          XmStringFree( clip_lbl);
           */

        } else {
          fprintf( stderr, "no intersection\n");
        }

      } else if ( event->type == ButtonRelease) {
      }

    /* end of else if **args == '3' */
    }

  }
}

/**********************************************************/
/* macro to draw X stuff or write to hardcopy file */
#define X_HC( A, B, C) \
switch( draw_dev) { \
case Drawdev_xwin: \
  A; \
  break; \
case Drawdev_ps: \
  B; \
  break; \
case Drawdev_xfig: \
  C; \
  break; \
default: \
  break; \
}

#define X_FINIS( DPY, PM, WIN, GC, WID, HGT, HCFILE) \
  X_HC( (XCopyArea( DPY, PM, WIN, GC \
                   , 0, 0, WID, HGT, 0, 0), XSync(DPY, 0)) \
      , PSClosePage( HCFILE) \
      , XFIGClosePage( HCFILE) \
  )

#define X_DRAWSEG( DPY, DRAWABLE, GC, SEG, ISEG, HCFILE, HCCOLOR) \
  X_HC( XDrawSegments( DPY, DRAWABLE, GC, SEG, ISEG) \
      , PSDrawSegments( HCFILE, HCCOLOR, SEG, ISEG) \
      , XFIGDrawSegments( HCFILE, HCCOLOR, SEG, ISEG) \
      )

#define X_DRAWPTS( DPY, DRAWABLE, GC, PTS, IPT, HCFILE, HCCOLOR) \
  X_HC( XDrawPoints( DPY, DRAWABLE, GC, PTS, IPT, CoordModeOrigin) \
      , 0 /* PSDrawPoints( HCFILE, HCCOLOR, PTS, IPT) */ \
      , 0 /* XFIGDrawPoints( HCFILE, HCCOLOR, PTS, IPT) */ \
      )

#define X_FILLRECT( DPY, DRAWABLE, GC, XX, YY, DXX, DYY, HCFILE, HCCOLOR) \
  X_HC( XFillRectangle( DPY, DRAWABLE, GC, XX, YY, DXX, DYY) \
      , PSFillRectangle( HCFILE, HCCOLOR, XX, YY, DXX, DYY) \
      , XFIGFillRectangle( HCFILE, HCCOLOR, XX, YY, DXX, DYY) \
      )

#define X_FILLPOLY( DPY, DRAWABLE, GC, XPT, NPT, HCFILE, HCCOLOR) \
  X_HC( XFillPolygon( DPY, DRAWABLE, GC, XPT, NPT, Convex, CoordModeOrigin) \
      , PSFillPoly( HCFILE, HCCOLOR, XPT, NPT) \
      , ; \
      )

#define X_DRAWSTRING( DPY, DRAWABLE, GC, XX, YY, TMPSTR, DXX, HCFILE, HCCOLOR) \
  X_HC( XDrawString( DPY, DRAWABLE, GC, XX, YY, TMPSTR, DXX) \
      , PSDrawString( HCFILE, HCCOLOR, XX, YY, TMPSTR) \
      , XFIGDrawString( HCFILE, HCCOLOR, XX, YY, TMPSTR) \
      )

#define X_START( DPY, DRAWABLE, GC, DX, DY, HCFILE, FILENAME) \
  X_HC( XFillRectangle( DPY, DRAWABLE, GC, 0, 0, DX, DY) \
      , HCFILE = PSOpenPage( FILENAME, DX, DY) \
      , HCFILE = XFIGOpenPage( FILENAME, DX, DY) \
      )

/**********************************************************/
#define SEGSIZ 500
void
draw_image( ITEM *cur_item, int draw_dev) {
int		dx, dy;
XSegment	seg[SEGSIZ];
XSegment	selSeg[2];
XPoint          xPts[MAXFOV100PTS];
long		S;
short		i, j, k;
/* short		S, i, j, k; */
double		size, vec[3];
double		h, orbitnum;

/*	Boolean		double_buffering;
*/
Drawable	drawable;
long		Sorted[3];
/* short		Sorted[3]; */
int		iseg, iselseg = 2;
int		selectedFrames;
GC gccurr;
int pscurr;

IMGFRM *lclimgfrm;
MTX lclmtx;

VEC lclvec;
MTX m1, m2, m3;

FILE *hcfile;

#define ICO(S,I) xyz[(S)*3+(I)]

  memcpy((void *)VR, (void *)R, size_R);

  /* solve for SR matrix - rotates sun to -Y for shadowed gridpoint check */

  /* - rotate sun to +X first ... */

  Rot2AbyB( 'x', 'z', sunFromAstAbf, m1);
  vxm( sunFromAstAbf, m1, lclvec);
  Rot2AbyB( 'x', 'y', lclvec, m2);
  mxm( m1, m2, m3);

  /* ... then rotate X/Y/Z to -Y/+X/Z ... */

  m2[0] = m2[2] = m2[4] = m2[5] = m2[6] = m2[7] = 0.0;
  m2[1] = m2[8] = 1.0;
  m2[3] = -1.0;
  mxm( m3, m2, m1);

  /* copy sun matrix to cur_item->sc->SR */

  for (i=k=0; i<3; i++) for (j=0; j<3; j++,k++) SR[i][j] = m1[k];

  /* rotate body */

  orbitgui_watchCursor( app_shell, True);
  spudview( spudv);

  /********************************************/
  /* draw rotated shape - X STUFF STARTS HERE */

  dx= da_width;
  dy= da_height;

  XSetFont( da_dpy, fgc, fontInfo->fid);

  if ( draw_dev == Drawdev_xwin) drawable= (Drawable)da_pm;

  /* allow extra space for comments */

  X_START( da_dpy, drawable, bgc, dx, da_height_pm, hcfile, hcfilename)

  dx= dx/2 + Tx;
  dy= dy/2 + Ty;

  for (i=k=0; i<3; i++)
    for (j=0; j<3; j++,k++)
      lclmtx[k] = R[i][j];

  size= (double)(rho*da_size); /* scale to body size */

  /* optional scale to image frame size */
  if ( scale2frame && currentImgfrmValid) {
  VEC vec0[4], dvec;
  double d1;
  double dmax = 0.0;
  int i, j;
  int lastpt = (4<currentImgfrm._ncampts) ? 4 : currentImgfrm._ncampts;
    /* rotate first 3 or 4 corners of current image frame */
    for ( i=0; i<lastpt; i++) vxm( currentImgfrm.vec3+(i*3), lclmtx, vec0[i]);
    /* get distance between corners */
    for ( i=0; i<(lastpt-1); i++)
    for ( j=i+1; j<lastpt; j++) {
        VMINUS2( vec0[i], vec0[j], dvec);
        d1 = VDOT(dvec,dvec);
        if ( d1 > dmax) dmax = d1;
    }
    if ( dmax > 0.0) size *= (2.0 * maxrad / sqrt(dmax));
  }

  /* put center of frame (p5) at center of display */

  if ( centerframe && currentImgfrmValid) {
    vxm( currentImgfrm._boreVec, lclmtx, lclvec);
    dx -= size * lclvec[0];
    dy += size * lclvec[2];
  }

  saveCtr.x = dx; saveCtr.y = dy; savePxlPerKm = size;

  /* draw polygons of proper color */

# define DRAWPOLY( IFA) \
  for ( S = 0; S < spudf->nface; ++S) { \
  XPoint xpt[3]; \
  double *v123[3]; \
  int i; \
    \
    IFA \
    plateUvn = spudf->uvnorms+(3*S); \
    colorVal = -VDOT(emissneg,plateUvn); \
    if ( colorVal > 0) { \
    VEC uvBore2Sc; \
    double r; \
      switch ( shading) { \
      case SHADE_MU0: \
      case SHADE_INCID: \
        colorVal = -VDOT(incidneg,plateUvn); \
        if ( shading == SHADE_INCID) colorVal = ACOSWLIM(colorVal); \
        break; \
      case SHADE_EMISS: \
        colorVal = ACOSWLIM(colorVal); \
        break; \
      case SHADE_MU: \
      case SHADE_MU0xMU: \
        if ( currentImgfrmValid) { \
          if ( (r=VLEN( currentImgfrm._boretoscVec)) > 0.0) { \
            r = 1.0 / r; \
            VSCAL2( r, currentImgfrm._boretoscVec, uvBore2Sc); \
            colorVal = VDOT( uvBore2Sc, plateUvn); \
          } else colorVal = lo; \
        }  /* if imgfrm not valid, colorVal = VDOT(emissneg,plateUvn) above */ \
        \
        if ( shading == SHADE_MU) break; \
        \
        colorVal *= -VDOT(incidneg,plateUvn);                   /* Mu0 x Mu */ \
        break; \
      default: /* SHADE_MAPDATA */ \
        colorVal = spudf->platecolor[S]; \
        break; \
      } \
      if ( colorVal > lo) { \
        colorNum = (long) (ncolor * (colorVal-lo) / del); \
        if ( colorNum >= ncolor) colorNum = ncolor - 1; \
        v123[0] = xyz + (3 * spudf->faceapices[S]); \
        v123[1] = xyz + (3 * spudf->oe[spudf->faceoeidx[S]]); \
        v123[2] = xyz + (3 * spudf->oe[spudf->faceoeidx[S]+1]); \
        for ( i=0; i<3; ++i) { \
          xpt[i].x = size*v123[i][0] + dx; \
          xpt[i].y = -size*v123[i][2] + dy; \
        } \
        X_FILLPOLY( da_dpy, drawable, colorGc[colorNum] \
                  , xpt, i, hcfile, colorNum+rbOffset) \
      } /* if colorVal > lo */ \
    } /* if colorVAL ... */ \
    } /* IFA */ \
  }

#define CBFR( COLOR, X, DX) /* color bar fill rect */ \
  X_FILLRECT( da_dpy, drawable, colorGc[COLOR], X, baseLineY, DX, cbHeight \
            , hcfile, COLOR+rbOffset)

#define CBDV( VAL, X, Y) /* color bar draw value */ \
  sprintf( tmpstr, "%1lg", VAL); \
  X_DRAWSTRING( da_dpy, drawable, fgc, X, Y, tmpstr, strlen(tmpstr) \
              , hcfile, PSWHITE)


#define CBDVS( X, Y, D) /* color bar - draw seg vert of length = descent */ \
  seg[iseg].x1 = seg[iseg].x2 = X; \
  seg[iseg].y1 = Y; \
  seg[iseg].y2 = seg[iseg].y1 + (fontInfo->descent D);/* draw down from top */ \
  ++iseg;  /* don't actually draw it yet */

  if ( coloring != COLOR_NONE) {
  int ncolor = (coloring == COLOR_RAINBOW) ? nrainbow : ngray;

  /* scale Mu0 0 to 1 if shadowing is on, -1 to 1 if it's off */
  double hi = (shading!=SHADE_MAPDATA) ? 1.0 : shadingHi;
  double lo = (shading==SHADE_MU0) ? ((hidden&SPUDV_SHAD)?0.0:-1.0) : shadingLo;
    switch ( shading) {
    case SHADE_MAPDATA: hi = shadingHi; lo = shadingLo;                   break;
    case SHADE_MU0:     hi = 1.0;   lo = (hidden&SPUDV_SHAD) ?0.0 :-1.0;  break;
    case SHADE_MU0xMU:  hi = 1.0;   lo = 0.0;                             break;
    case SHADE_INCID:
    case SHADE_EMISS:   hi = 90.0;  lo = 0.0;                             break;
    default:            hi = 1.0;   lo = 0.0;  /* SHADE_MU */             break;
    }
    if ( ncolor && (hi > lo)) {
    double del = hi - lo;
    int rbOffset = (coloring==COLOR_RAINBOW) ? RB_RAINBOWOFFSET : RB_GRAYOFFSET;
    GC *colorGc = (coloring == COLOR_RAINBOW) ? rainbowGc : grayGc;
    double *incidneg = spudv->_SR[1];
    double *emissneg = spudv->_VR[1];
    double *plateUvn;
    double colorVal;
    long colorNum;
    int cbWidth = da_width / 3;
    int cbHeight = (3 * (fontInfo->descent+fontInfo->ascent)) / 2;
    double del5 = del / 5.0;
    double logdel5 = log(del5) / log(10.0);
    double expon = floor( logdel5);
    double tenexp = pow( 10.0, expon);
    double mantissa5 = del5 / tenexp;
    double colorRate, valRate;
    static double xBreaks[4] = { -1.0, 0.0, 0.0, 0.0};
    static double xVals[5] =   { 1.0, 2.0, 5.0, 10.0, 20.0 };
    double newVal, nextBreak;
    int i, newColorNum, txtLen, x, w;
    int txtWidth, txtHeight;
    int baseUpperTxt, baseLowerTxt;
    int baseLineX, baseLineY;

      /* draw the polygons */

      if (hidden) { DRAWPOLY( if (pstat[S]) { ) } else { DRAWPOLY( { ) }

      /* draw the color bar & associated text in upper right corner */

      /*
       *                                                 da_width-2
       *                                        ->|      |<-XTextWidth(hiValue)
       *       |<---------cbWidth---------------->|      |
       *       V                                  |      V ___1
       *       loValue                            hiValue  ___baseUpperTxt
       *       |                                  |        ___baseLineY
       *       |111112|222333334444|55555666667777|
       *       |111112|222333334444|55555666667777|
       *              |            |
       *       ^      midValue1    midValue2               ___baseLowerTxt
       *       |
       *       baseLineX
       */

      /* - initialize breakpoints */

      if ( xBreaks[0] < 0.0) {
        for ( i=0; i<4; ++i) xBreaks[i] = sqrt(xVals[i]*xVals[i+1]);
      }

      /* - find which is the next breakpoint */
      for ( i=0; mantissa5 > xBreaks[i]; ++i) ;
      del5 = xVals[i] * tenexp;
      nextBreak = (1.0 + floor( lo / del5)) * del5;

      sprintf( tmpstr, "%1lg", hi);
      txtLen = strlen( tmpstr);
      txtWidth = XTextWidth( fontInfo,tmpstr,txtLen);
      txtHeight = (fontInfo->ascent+fontInfo->descent) + 1;
      baseUpperTxt = 1 + fontInfo->ascent;
      baseLineY = 1 + txtHeight;
      baseLowerTxt = baseLineY + cbHeight + txtHeight;
      baseLineX = da_width - (2 + cbWidth);

      /* - right & left values + hash marks above color bar */
      CBDV( hi, da_width-(2+txtWidth), baseUpperTxt)
      CBDV( lo, baseLineX, baseUpperTxt)

      iseg = 0;
      CBDVS( da_width-2, baseUpperTxt, * 2 + cbHeight)
      CBDVS( baseLineX, baseUpperTxt, * 2 + cbHeight)

      /* - color bar - step one pixel at a time
       *   - draw old color when it changes to new
       *   - draw numeric legend when it goes past a breakpoint
       */
      colorRate = ((double) ncolor) / cbWidth;
      valRate = del / cbWidth;
      colorNum = 0;
      x = baseLineX;
      for ( w=i=1; i<=cbWidth; ++i,++w) {

        newColorNum =                                            /* color bar */
                      0.001 + ( i * colorRate);    /* ensure colorNum reached */
        if ( newColorNum > ncolor) newColorNum = ncolor;
        if ( newColorNum > colorNum) {
          CBFR( colorNum, x, w)
          colorNum = newColorNum;
          x += w;
          w = 0;
        }

        newVal = lo + (i * valRate);                            /* hash marks */
        if ( newVal >= nextBreak) { 
          CBDV( nextBreak, baseLineX+i-1, baseLowerTxt)
          CBDVS( baseLineX+i-1, baseLineY+cbHeight, +0)
          nextBreak += del5;
          if ( nextBreak >= (hi - (del/35)) )                /* skip near end */
            nextBreak = hi + del5;
        }
      } /* for i=1; i<=cbWidth ... */

      X_DRAWSEG(da_dpy, drawable, fgc, seg, iseg, hcfile, PSWHITE)

    }
  }

  /* scale bar - km1 = 10^n km/block */
  /* 5 blocks make a scale bar & cover no more than 1/5 of screen */

  if ( scalebar || textStyle) {
  static double km1, kmbench;
  static int xx, xxold, yy, dxx, dyy;
  static int i;
  static int phaselo, phasehi, phasecur;
  static UTC lclutc;
  static scaletxtWidth;
  double etHi, etLo, etCur;
  fortint instrNum;
  IMGFRM *imgfrmLo, *imgfrmHi;

    /* find length in km that is 1/5 of 1/5 of width of screen */
    kmbench = (da_width * 0.04) / size;
    /* find = or smaller km of form 10^n, n is integer */
    for ( km1=1.0; km1<kmbench; km1 *= 10.0) ;
    while ( km1 > kmbench) km1 /= 10.0;

    xx = 0; dyy = NINT( 0.5 * size * km1);
    yy = da_height - dyy;
    for ( i=0; i<5; i++) {
      dxx = NINT((i+1)*km1*size) - xx;
      if ( scalebar) {
        X_FILLRECT( da_dpy, drawable, fgc, xx, yy, dxx, dyy, hcfile, PSWHITE)
      }
      xx += dxx;
      yy += (i&1) ? (+dyy) : (-dyy);
    }

    /* put box around scale bar */
    seg[0].x1 = 0;
    seg[0].y1 = yy;
    seg[1].x1 = xx - 1;
    seg[1].y1 = seg[0].y1;
    seg[2].x1 = seg[1].x1;
    seg[2].y1 = da_height - 1;
    seg[3].x1 = seg[0].x1;
    seg[3].y1 = seg[2].y1;
    for ( i=0; i<4; i++) {
      seg[i].x2 = seg[(i+1) % 4].x1;
      seg[i].y2 = seg[(i+1) % 4].y1;
    }
    iseg = 4;
    if ( scalebar) {
      X_DRAWSEG(da_dpy, drawable, fgc, seg, iseg, hcfile, PSWHITE)
    }
    iseg = 0;

    /* label scale bar */

    yy -= fontInfo->descent;
    sprintf( tmpstr, "%1lg km", km1 * 5);
    dxx = strlen(tmpstr);
    scaletxtWidth = XTextWidth( fontInfo,tmpstr,dxx);
    xx = NINT( 2.5 * km1 * size) - (scaletxtWidth/2);
    if ( xx < 0) xx = 0;
    X_DRAWSTRING( da_dpy, drawable, fgc, xx, yy, tmpstr, dxx, hcfile, PSWHITE)

/* macros to set selected frames flag for first selected frame and to 
 * clear selected frames flag for either last sel'd frame or no second sel
 * - assumes FRM is stepping through a linked list if IMGFRMs
 */

#define SETSELFRM( FRM) \
  if ( !selectedFrames) if ( FRM == sel1Imgfrm) selectedFrames = 1

#define CLRSELFRM( FRM) \
  if ( selectedFrames) if ( FRM == sel2Imgfrm) selectedFrames = 0


/* macro to determine if IMGFRM is hidden or not
 * ***N.B. this must come first after any left brace ("{") because it 
 *         has some variable declarations
 */
#define HIDEONE(LCLIMGFRM) \
  int hideone; \
  VEC yyy; \
    /* if requested, hide image frames that are facing away from us */ \
    vxm( LCLIMGFRM->_normVec, lclmtx, yyy); \
    hideone = hideframes &&  (yyy[1] > 0.0); \
    /* if requested, hide image frames that are facing away from sun */ \
    hideone |= litframes &&  (LCLIMGFRM->_incid >= 90.0); \
    hideone |= minHits4 > LCLIMGFRM->_nhits4; \
    /* if requested, hide image frames that are > than one orbit away from us*/\
    if ( currentframes ) { \
      hideone |= ( fabs( orbitnum - LCLIMGFRM->_orbitnum) >= 1.0); \
    } \
    /* set selectedFrames indicator */ \
    SETSELFRM(LCLIMGFRM); \
    \
    /* if requested & sel1Imgfrm is not null, hide unselected frames */ \
    /* ***N.B. This will only display ONE frame when */ \
    /*         "Select Frame" button is first pushed */ \
    if ( hideUnselectedFrames && sel1Imgfrm && !selectedFrames) hideone |= 1

    /* additional text */

    metTrunc = 0;

    switch (textStyle) {

    case TEXTSTYLE_RANGE_TRUNC3:  /* truncate last three characters of MET */
      metTrunc = 3;

    case TEXTSTYLE_RANGE:

      if ( !imgfrm) break;

      yy = da_height - fontInfo->descent;
      dyy = NINT( (fontInfo->ascent + fontInfo->descent) * 1.1);
      xx = NINT( 5 * km1 * size);
      if ( xx < scaletxtWidth) xx = scaletxtWidth;
      xx += (dyy/2);

      instrNum=SC_MSI;                 /* loop through instruments, MSI first */
      while (instrNum < SC_NUMINSTR) {

        /* find 1. imgfrm that uses this instrument */

        selectedFrames = 0;

        for ( imgfrmLo = imgfrm; imgfrmLo; imgfrmLo = imgfrmLo->nextif) {
        HIDEONE(imgfrmLo);
          if ( !hideone && imgfrmLo->_instrument == instrNum) break;
          CLRSELFRM(imgfrmLo);  /* clear selected frames flage if appropriate */
        }

        if ( imgfrmLo) {
        char doymet[UTCLEN];
        char *csrc, *cdest;
        fortint lenm1 = (UTCLEN - 1);
        fortint lenout;

          imgfrmHi = imgfrmLo;          /* find frames w/ highest & lowest ET */
          for ( lclimgfrm=imgfrmLo; lclimgfrm; lclimgfrm = lclimgfrm->nextif) {
          HIDEONE(lclimgfrm);
            if ( !hideone && lclimgfrm->_instrument == instrNum) {
              etCur = lclimgfrm->_et;
              if ( etCur > imgfrmHi->_et) imgfrmHi = lclimgfrm;
              else if ( etCur < imgfrmLo->_et) imgfrmLo = lclimgfrm;
            }
            CLRSELFRM(lclimgfrm);/* clear selected frames flag if appropriate */
          }

/* macro to copy from doymet to cdest w/o spaces */

#define CPYNOSPC \
  for ( csrc=doymet; *csrc; ++csrc) { \
    if ( *csrc != ' ') *(cdest++) = *csrc; \
  }

          /*                <--1---><-2-><-3><-4-><><-6-><-7><-8->9 */
          /* build string  "INSTR:  LoUTC to HiUTC (loMET to HiMET)" */

          cdest = tmpstr;

          /* (1) */
          sprintf( tmpstr, "%s:  ", imgfrmLo->_instrName);
          cdest = cdest + strlen( cdest);

          /* (2) convert Lo et to UTC, copy w/o spaces to cdest */
          ospice_et2doy( &imgfrmLo->_et, &lenm1, &lenout, doymet);
          doymet[lenout] = '\0';
          CPYNOSPC

          /* (3) */
          strcpy( cdest, " to ");
          cdest = cdest + strlen( cdest);
          
          /* (4) convert Hi et to UTC, copy w/o spaces to cdest */
          ospice_et2doy( &imgfrmHi->_et, &lenm1, &lenout, doymet);
          doymet[lenout] = '\0';
          CPYNOSPC

          /* (5) */
          strcpy( cdest, " (");
          cdest = cdest + strlen( cdest);

          /* (6) convert Lo sclk to MET, copy to cdest */
          ospice_scdecd( &imgfrmLo->_scid, &imgfrmLo->_sclk
                       , &lenm1, &lenout, doymet);
          doymet[lenout] = '\0';
          CPYNOSPC
          if ( metTrunc) cdest -= metTrunc;

          /* (7) */
          strcpy( cdest, " to ");
          cdest = cdest + strlen( cdest);

          /* (8) convert Hi sclk to MET, copy to cdest */
          ospice_scdecd( &imgfrmHi->_scid, &imgfrmHi->_sclk
                       , &lenm1, &lenout, doymet);
          doymet[lenout] = '\0';
          CPYNOSPC
          if ( metTrunc) cdest -= metTrunc;

          /* (9) */
          strcpy( cdest, ")");
          cdest = cdest + strlen( cdest);

          *cdest = '\0';

          dxx = strlen( tmpstr);
          X_DRAWSTRING( da_dpy, drawable, fgc
                      , xx, yy, tmpstr, dxx, hcfile, PSWHITE)
          yy -= dyy;
        }

        /* next instrument */

        if ( instrNum != SC_MSI) {
          if ( ++instrNum == SC_MSI) ++instrNum;              /* skip SC_MSI */
        } else {
          instrNum = SC_NIS;
        }
      } /* while instrNum <= SC_NIS2 */
 
      break;

    case TEXTSTYLE_ORIGINAL:

      /* orbit info */
      if ( scOrbit._status == ORB_USESPK) {
        sprintf( tmpstr, " ORBITAL INFO:  from SPICE kernels");
      } else {
        sprintf( tmpstr
        , " ORBIT:  a=%.3lg km  e=%.2lf  p=%.2lf h  rho=%.2f g/cc  i=%.0lf deg"
        , scOrbit._so_rp / (1.0 - scOrbit._so_ecc)
        , scOrbit._so_ecc
        , scOrbit._period / 3600.0
        , scOrbit._rho
        , scOrbit._so_inc * degpr);
      }
      yy = da_height - fontInfo->descent;
      xx = NINT( 5 * km1 * size);
      if ( xx < scaletxtWidth) xx = scaletxtWidth;
      dxx = strlen( tmpstr);
      X_DRAWSTRING( da_dpy, drawable, fgc, xx, yy, tmpstr, dxx, hcfile, PSWHITE)
  
      /* sequence info */
  
      /* - find phase range */
      phaselo = 181; /* init range to bad values */
      phasehi = -1;
      selectedFrames = 0;
      for ( lclimgfrm = imgfrm; lclimgfrm; lclimgfrm = lclimgfrm->nextif) {
      HIDEONE(lclimgfrm);
        if ( !hideone) {
          phasecur = NINT(lclimgfrm->_phase);
          if ( phasecur > phasehi) phasehi = phasecur;
          if ( phasecur < phaselo) phaselo = phasecur;
        }
        CLRSELFRM(lclimgfrm);
      }
      orbit_et2utc( (imgfrm) ? imgfrm->_et : 0.0, &lclutc);
  
      sprintf( tmpstr
        , " SEQUENCE:  overlap=%d%%  phase=[%d-%d] deg  duration=%.2f h %s%s"
        , NINT( scOrbit._agen._FrmOverlap * 100)
        , phaselo, phasehi
        , ((imgfrm)? (imgfrm->previf->_et - imgfrm->_et) : 0.0) / 3600.0
        , "start time=", lclutc._utcstr);
  
      yy -= NINT( (fontInfo->ascent + fontInfo->descent) * 1.5);
      dxx = strlen( tmpstr);
      X_DRAWSTRING( da_dpy, drawable, fgc, xx, yy, tmpstr, dxx, hcfile, PSWHITE)
  
      break;

    default:
      break;
    } /* switch textStyle */
  }

#define DRAWLOOP( IFA, IFB) \
  for (iseg = S = 0; S < (spudf->nv - 1); S++) { \
  unsigned long i = spudf->oeidx[S]; \
  unsigned long j = spudf->oeidx[S+1]; \
  unsigned long k; \
\
    IFA \
      for (k= i; k<j; k++) IFB \
        seg[iseg].x1=  size*ICO(S,0) + dx; \
        seg[iseg].y1= -size*ICO(S,2) + dy; \
        seg[iseg].x2 =  size*ICO(spudf->oe[k],0) + dx; \
        seg[iseg].y2 = -size*ICO(spudf->oe[k],2) + dy; \
        if ( (++iseg) == SEGSIZ) { \
          X_DRAWSEG( da_dpy, drawable, SHAPE_gc, seg, iseg, hcfile, PSWHITE) \
          iseg = 0; \
        } \
      } \
    } \
  }

  if ( linestyle != LINESTYLE_NONE) {
    if (hidden) {
      DRAWLOOP( if (vstat[S]) { , if ( segstat[k]) { )
    } else {
      DRAWLOOP( { , { )
    }

    if ( iseg) {
      X_DRAWSEG( da_dpy, drawable, SHAPE_gc, seg, iseg, hcfile, PSWHITE)
      iseg = 0;
    }
  }

  /* draw the saved image frames cyan - last one is yellow */

  orbitnum = (currentImgfrmValid)
             ? currentImgfrm._orbitnum
             : ( (imgfrm)
                 ? imgfrm->previf->_orbitnum
                 : 0.0
               );

  iseg = 0;
  gccurr = C_gc; /* just in case */
  pscurr = PSCYAN;

  for ( lclimgfrm = imgfrm; lclimgfrm; lclimgfrm = lclimgfrm->nextif) {
  int lastframe;
  GC gcnext;
  int psnext;

  HIDEONE(lclimgfrm);

    lclimgfrm->_isHidden = hideone;

    lastframe = (lclimgfrm == imgfrm->previf);

    /* set color of frame edges:  cyan/yellow for MSI; shade of gold for NIS */

    switch( lclimgfrm->_instrument) {
    case SC_MSI:
      if ( lastframe && !hideUnselectedFrames) {
        gcnext = Y_gc;    /* yellow */
        psnext = PSYELLOW;
      } else {
        gcnext = C_gc;    /* cyan */
        psnext = PSCYAN;
      }
      break;
    case SC_NIS:
    case SC_NIS2:
      gcnext = GO_gc[(lclimgfrm->_nisScannum + lclimgfrm->_nisPosition) % 5];
      psnext = PSYELLOW;
      break;
    default:
      gcnext = R_gc;
      psnext = PSRED;
      fprintf( stderr
             , "\n*** Unknown instrument: _instrName='%s'; _instrument=%ld\n"
             , lclimgfrm->_instrName ? lclimgfrm->_instrName : "(nul)"
             , lclimgfrm->_instrument), fflush(stderr);
    }

    /* draw last segments (if any) of different color */
    if ( iseg && ((gcnext != gccurr) || (psnext != pscurr))) {

      X_DRAWSEG( da_dpy, drawable, gccurr, seg, iseg, hcfile, pscurr)
      iseg = 0;
    }
    pscurr = psnext;
    gccurr = gcnext;

    /* draw frame if not hidden */

    if ( !lclimgfrm->_isHidden) {
    static double savex, savey;

      vxm( lclimgfrm->vec3, lclmtx, yyy);
      savex = size * yyy[0] + dx;
      savey = -size * yyy[2] + dy;

      /* save location for selected frames */
      if ( selectedFrames) {
        selSeg[0].x1 = savex;
        selSeg[0].y1 = savey;
      }

      for ( i=1; i<=lclimgfrm->_ncampts; i++) {

        seg[iseg].x1 = savex;
        seg[iseg].y1 = savey;

        j = 3 * (i % lclimgfrm->_ncampts);

        vxm( lclimgfrm->vec3+j, lclmtx, yyy);

        savex = seg[iseg].x2 = size*yyy[0] + dx;
        savey = seg[iseg].y2 = -size*yyy[2] + dy;

        /* save location for selected frames */
        if ( selectedFrames && i < 4) {
          switch ( i) {
          case 1:
            selSeg[1].x1 = savex;
            selSeg[1].y1 = savey;
            break;
          case 2:
            selSeg[0].x2 = savex;
            selSeg[0].y2 = savey;
            break;
          case 3:
            selSeg[1].x2 = savex;
            selSeg[1].y2 = savey;
            break;
          }
        }

        if ( (++iseg) == SEGSIZ) {

          X_DRAWSEG( da_dpy, drawable, gccurr, seg, iseg, hcfile, pscurr )
          iseg = 0;
        }

      } /* for i <= lclimgfrm->_ncampts */

      /* draw frame as selected if it is UNLESS unselected frames are hidden */

      if ( selectedFrames && !hideUnselectedFrames) {
        X_DRAWSEG( da_dpy, drawable, gccurr, selSeg, iselseg, hcfile, pscurr)
      }

      /* draw fov points */

      if ( DOFOV100_TSTGLOBAL(sc) && lclimgfrm->_nFov100Plates) {
      int visCount;
        for ( visCount=i=0; i<lclimgfrm->_nFov100Plates; ++i) {
          if ( pstat[lclimgfrm->_fov100Plates[i]]) {
            vxm( lclimgfrm->_fov100Pts[visCount], lclmtx, yyy);
            xPts[visCount].x = size*yyy[0] + dx;
            xPts[visCount].y = -size*yyy[2] + dy;
            visCount++;
          }
        }
        if ( visCount) {
          X_DRAWPTS( da_dpy, drawable, gccurr, xPts, visCount, hcfile, pscurr)
        }
      }

    } /* if !hideone */

    /* clear selected frames flag for either last sel'd frame or no 2d sel */
    CLRSELFRM(lclimgfrm);

  } /* for lclimgfrm */

  if ( iseg) {
    X_DRAWSEG( da_dpy, drawable, gccurr, seg, iseg, hcfile, pscurr)
    iseg = 0;
  }

  /**********************************************************************/
  /* draw the current image frame magenta */

  if ( currentImgfrmValid && !hideUnselectedFrames) {
  static double savex, savey;
  VEC yyy;

    iseg = 0;
    vxm( currentImgfrm.vec3, lclmtx, yyy);
    savex = size*yyy[0] + dx;
    savey = -size*yyy[2] + dy;

    for ( i=1; i<=currentImgfrm._ncampts; i++) {

      seg[iseg].x1 = savex;
      seg[iseg].y1 = savey;

      j = 3 * (i % currentImgfrm._ncampts);
      vxm( currentImgfrm.vec3+j, lclmtx, yyy);

      savex = seg[iseg].x2 = size*yyy[0] + dx;
      savey = seg[iseg].y2 = -size*yyy[2] + dy;

      /* pass null FILE * to hardcopy so current frame does not get drawn */

      if ( (++iseg) == SEGSIZ) {
        X_DRAWSEG( da_dpy, drawable, M_gc, seg, iseg, (FILE *) 0, PSMAGENTA)
        iseg = 0;
      }
    } /* for ... */
    if ( iseg) {
      X_DRAWSEG( da_dpy, drawable, M_gc, seg, iseg, (FILE *) 0, PSMAGENTA)
      iseg = 0;
    }
  }
  /**********************************************************************/

  if ( linestyle != LINESTYLE_NONE) {

  /* draw RGB coordinate system */

  /* sort from largest to smallest y - for RGB/xyz, axes; VR[1][] is y
     - purpose is to draw RGB/xyz axes so they occlude each other
    properly.
     - use bubble sort - ok for small sorts */
  for (S= 0; S < 3; S++) Sorted[S] = S;
  for (i=2; i; i--)
    for (S= 0; S < i; S++)
      if ( VR[1][Sorted[S]] < VR[1][Sorted[S+1]]) {
        j = Sorted[S];
        Sorted[S] = Sorted[S+1];
        Sorted[S+1] = j;
      }

  seg[0].x1= dx;
  seg[0].y1= dy;

  for (S= 0; S < 3; S++) {
    for (i= 0; i < 3; i+= 2) {
      /* just need to compute new x and z components for plotting unless doing 
       * perspective
       */
      vec[i]= 0.;
      for (j= 0; j < 3; j++)
        vec[i]+= (double)VR[i][j] * (double)(Sorted[S] == j) * asterad[j];
    }

    seg[0].x2=  size*vec[0] + dx;
    seg[0].y2= -size*vec[2] + dy;

    switch (Sorted[S]) {
    case 0:
      X_DRAWSEG( da_dpy, drawable, R_gc, seg, 1, hcfile, PSRED)
      break;
    case 1:
      X_DRAWSEG( da_dpy, drawable, G_gc, seg, 1, hcfile, PSGREEN)
      break;
    case 2:
      X_DRAWSEG( da_dpy, drawable, B_gc, seg, 1, hcfile, PSBLUE)
      break;
    }
  } /* for S=0; s<3 */
  } /* if ( linestyle != LINESTYLE_NONE) ... */

  /* draw comments for hardcopy */

  if ( commentsOn) {
  int dyy = NINT( (fontInfo->ascent + fontInfo->descent) * 1.1);
  int xx;
  int yy = da_height + (dyy*3)/2;
  int dxx;
  int i;

    switch( draw_dev) {
    case Drawdev_ps:
    case Drawdev_xfig:
    case Drawdev_xwin:

      for ( i=0; i<MAXNUMCOMMENT; ++i) {
      char *c = comments[i];
      char cSave;
        xx = dyy / 2;
        dxx = strlen( c);
        while ( dxx > 0) {  /* break all strings at MAXWIDCOMMENT characters */

          if ( dxx > MAXWIDCOMMENT) {
            cSave = c[MAXWIDCOMMENT];
            c[MAXWIDCOMMENT] = '\0';
          }

          X_DRAWSTRING( da_dpy, drawable, fgc, xx, yy, c
                      , (dxx<MAXWIDCOMMENT ? dxx : MAXWIDCOMMENT)
                      , hcfile, PSWHITE)

          if ( dxx > MAXWIDCOMMENT) {
            c[MAXWIDCOMMENT] = cSave;
          }

          dxx -= MAXWIDCOMMENT;
          if ( dxx > 0) {
            c += MAXWIDCOMMENT;
            yy += dyy;
            xx = dyy;    /* indent broken lines */
          }
        }
        yy += dyy;
      }
       break;

    default:
      break;
    } /* switch draw_dev */
  } /* if commentsOn */

  orbitgui_watchCursor( app_shell, False);

  X_FINIS( da_dpy, da_pm, da_win, fgc, da_width, da_height, hcfile)

  return;
}

/**********************************************************/
identity_matrix3d(m)
double	m[4][4];
{
	int i, j;

	for (i= 0; i < 4; i++)
		for (j= 0; j < 4; j++)
			m[i][j]= (double)(i == j);
}

/**************************************************************/
/* add pixels & rgb colors to color list */

void orbitgui_add_prgb( ITEM *cur_item, unsigned long pixel,int r,int g,int b) {
int i, lo, hi;

  if ( numXPixels > 255) {
    fprintf( stderr, "***orbitgui_add_prgb:  out of colors\n");
    fflush( stderr);
    return;
  }

  if ( numXPixels) {

    /* find position where pixel should be inserted */

    /* - ensure pixel is not at either end of list */

    if ( pixel <= xPixels[0])                 { i = lo = hi = 0; }
    else if ( pixel >= xPixels[numXPixels-1]) { i = lo = hi = numXPixels; }
    else { lo = 0;  hi=numXPixels-1; i = (lo+hi) / 2;}

    while ( (hi-lo) > 1) {

      /* - pixel is between lo & hi, do binary search between lo & hi */

      if      ( xPixels[i] < pixel) { lo=i; i=(lo+hi+1)/2; }
      else if ( pixel < xPixels[i]) { hi=i; i=(lo+hi)/2; }
      else { lo = hi = i; }
    }

    /* add no duplicates */

    if ( xPixels[hi] == pixel || xPixels[lo] == pixel) return;

    /* shift arrays */
    for ( i=numXPixels; i>hi; --i) {
      xPixelsRed[i] = xPixelsRed[i-1];
      xPixelsGrn[i] = xPixelsGrn[i-1];
      xPixelsBlu[i] = xPixelsBlu[i-1];
      xPixels[i] = xPixels[i-1];
    }
  } else hi = lo = 0;  /* numXPixels == 0, initial entry */

  xPixelsRed[hi] = r;
  xPixelsGrn[hi] = g;
  xPixelsBlu[hi] = b;
  xPixels[hi] = pixel;
  numXPixels++;

  if ( !hi) {
    lastPixel = xPixels[0];
    lastDN = 0;
  }

  return;
}

/**********************************************************/
unsigned long
set_color(c, cur_item)
char	*c;
ITEM	*cur_item;
{
	Colormap	cmap;
	XColor	c0, c1;

	cmap = DefaultColormap( da_dpy, DefaultScreen(da_dpy));

	XAllocNamedColor(da_dpy, cmap, c, &c1, &c0);

        orbitgui_add_prgb( cur_item, c1.pixel, (int) (c0.red / 256)
                         , (int) (c0.green / 256), (int) (c0.blue / 256));

	return(c1.pixel);
}

/**************************************************************/
/* setup rainbow GC's */

void
orbitgui_set_rainbow( ITEM *cur_item) {
unsigned short *lclrgbs;
unsigned short i256 = 256;    /* for scaling 0-255 to 0-65280 */
int i;
Colormap cmap;
XColor xcol;

  /* get number of colors in rainbow */
  lclrgbs = rainbowRGB;
  for ( nrainbow=0; *lclrgbs < RB_RGBTERM; ++nrainbow) lclrgbs += 3;

  /* allocate GCs & get colormap */
  rainbowGc = (GC *) malloc( sizeof( GC) * nrainbow);
  cmap = DefaultColormap( da_dpy, DefaultScreen(da_dpy));

  /* create GCs & set foreground */

  lclrgbs = rainbowRGB;
  for ( i=0; *lclrgbs < RB_RGBTERM; lclrgbs += 3) {
    xcol.red = i256 * lclrgbs[0] + 255;
    xcol.green = i256 * lclrgbs[1] + 255;
    xcol.blue = i256 * lclrgbs[2] + 255;
    if ( XAllocColor( da_dpy, cmap, &xcol)) {
      orbitgui_add_prgb( cur_item, xcol.pixel
                       , (int) lclrgbs[0], (int) lclrgbs[1], (int) lclrgbs[2]);
      rainbowGc[i] = XCreateGC( da_dpy, da_win, 0, 0);
      XSetForeground( da_dpy, rainbowGc[i], xcol.pixel);
      i++;
    } else nrainbow--;
  }

  /* allocate GCs & get colormap */
  ngray = RB_NUMGRAYS;
  grayGc = (GC *) malloc( sizeof( GC) * ngray);

  /* create GCs & set foreground */

  grayGc[0] = bgc;                       /* black */

  for ( i=1; i<ngray; ) {
    xcol.red =
    xcol.green =
    xcol.blue = i * 65535 / (ngray-1);
    if ( XAllocColor( da_dpy, cmap, &xcol)) {
      orbitgui_add_prgb( cur_item, xcol.pixel, (int) (xcol.red/256)
                       , (int) (xcol.red/256), (int) (xcol.red/256));
      grayGc[i] = XCreateGC( da_dpy, da_win, 0, 0);
      XSetForeground( da_dpy, grayGc[i], xcol.pixel);
      i++;
    } else ngray--;
  }

  return;
}

/**********************************************************/

#define D2(A,B,C) ((ico[A][C]-ico[B][C])*(ico[A][C]-ico[B][C]))
#define V2(A,B) (D2(A,B,0)+D2(A,B,1)+D2(A,B,2))

/**********************************************************/

/* return first item */

ITEM *get_first_item() { return( first_item); }

/**********************************************************/

/* given a pointer to an item, return next item */
/*  - return original pointer if not a valid item */

ITEM *get_next_item( item) ITEM *item; {
ITEM *local_item;

/* if invalid pointer, return original */

  if ( !item) return( item);

/* find matching item if it exists */

  for ( local_item=first_item; local_item && (item != local_item); ) {
    local_item = local_item->next_item;
  }

/* if match succeeded, return next item */
  if ( local_item) return( local_item->next_item);

/* if match failed, return original */
  return( item);
}

/* macro to set type for aimpt and scvec structures */
/* ***N.B.  This only CHANGES the ->type so the first */
/*          call to this routine will drop through to ->type = -1 */
/*          and the callback will return 0 */

#ifdef IFXXX
#undef IFXXX
#endif
#define IFXXX(A,IA,ST) \
  if ( ST->A == w && ST->type != IA ) { \
    ST->type = IA; \
  }

/**********************************************************/


void
orbit_launch_enterxyz( ITEM *cur_item, char *lblpfx, char **lblsfx
                     , char *wildcard, double *vecinout)
{
static char *cptr[20];
static int n;
static char c255[1255];


  for ( cptr[n=0] = c255; lblsfx[n] && (n < 20); n++) {
    sprintf( cptr[n], "%s %s", lblpfx, lblsfx[n]);
    cptr[n+1] = cptr[n] + 1 + strlen( cptr[n]);
  }
  cptr[n] = (char *) 0;

  orbit_enterxyz( (void *) cur_item, app_shell, n, cptr, wildcard, vecinout);

  return;
}

/**********************************************************/
/* set type of and pointing for aimpt */
int
set_aimpt( Aimpt *aimpt, Widget w, char *whoami, ITEM *cur_item)
{
int oldtype = aimpt->type;
char aptype[255], wildcard[255];
char *lclc;

static char *xyzStrings[] = { "X, km", "Y, km", "Z, km", (char *) 0 };
static char *nadirStrings[] = { "Alt, deg", "Azim, deg", (char *) 0 };
char **sentStrings;

#define IFAIM(A,IA) IFXXX(A,IA,aimpt)

  IFAIM(abf,Iabf)
  else IFAIM(aci,Iaci)
  else IFAIM(nadir,Inadir)
  else IFAIM(j2k,Ij2k)
  else IFAIM(eci,Ieci)
  else IFAIM(sci,Isci)

/* don't act on first callback to same button, but leave ->type set
 * to trip next time
 */

  else {
    aimpt->type = -1;
    return(0);
  }

  if ( aimpt->type != Inadir) sentStrings = xyzStrings;
  else sentStrings = nadirStrings;

  switch(aimpt->type) {
  case Iabf:
    lclc = "ABF";
    break;
  case Iaci:
    lclc = "ACI";
    break;
  case Inadir:
    lclc = "Nadir";
    break;
  case Ij2k:
    lclc = "J2000";
    break;
  case Ieci:
    lclc = "ECI";
    break;
  case Isci:
    lclc = "SCI";
    break;
  default:
    fprintf( stderr, "set_aimpt:  %s Huh?\n", whoami);
    return(0);
  }
  sprintf( aptype, "%s%s", whoami, lclc);
  sprintf( wildcard, "%s%s", whoami + strlen(whoami) + 1, lclc);
  orbit_launch_enterxyz( cur_item, aptype, sentStrings, wildcard, aimpt->vec);
  return(0); /* tell caller to not act until new vector entered */
}

/**********************************************************/
/* act on updated pointing info */
void
enterpoint_act( ITEM *cur_item)
{
  solveanddraw( cur_item);
  return;
}

/**********************************************************/
/* act on updated pointing info */
void
enterxyz_act( ITEM *cur_item)
{
  solveanddraw( cur_item);
  return;
}

/**********************************************************/
/* set type of and pointing for scvec */
int
set_scvec( Scvec *scvec, Widget w, char *whoami, ITEM *cur_item)
{
int oldtype = scvec->type;
char sctype[255], wildcard[255];
static char *xyzStrings[] = { "X", "Y", "Z", (char *) 0 };

#define IFSCV(A,IA) IFXXX(A,IA,scvec)

  IFSCV( instr, Iinstr)
  else IFSCV( panel, Ipanel)
  else IFSCV( x, Ix)
  else IFSCV( y, Iy)
  else IFSCV( z, Iz)
  else IFSCV( user, Iuser)

/* don't act on first callback to same button */
  else {
    scvec->type = -1;
    return(0);
  }

  switch(scvec->type) {
  case Iinstr:
  case Ipanel:
  case Ix:
  case Iy:
  case Iz:
    return(1); /* tell caller we made a change */
  case Iuser:
    sprintf( sctype, "%sUser-defined S/C Vector", whoami);
    sprintf( wildcard, "%s%s", whoami + strlen(whoami) + 1, "User");
    orbit_launch_enterxyz( cur_item, sctype, xyzStrings, wildcard, scvec->vec);
    return(0); /* we made a change, but it won't take until vector entered */
  default:
    fprintf( stderr, "set_scvec:  %sHuh?\n", whoami);
    return(0);
  }
}

/**********************************************************/
void
pointing_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
Widget prnt;
int acted;
ITEM *cur_item = (ITEM *) client_data;

/* don't bother with buttons that have been set to False */
  if ( !XmToggleButtonGetState(w)) return;

  prnt = XtParent(w);
  if ( prnt == bore.aimpt.menu) {
    acted = set_aimpt( &bore.aimpt, w, "Boresight Aimpoint:  \0*.boreaim"
                     , cur_item) ;

  } else if ( prnt == bore.scvec.menu) {
    acted = set_scvec( &bore.scvec, w, "S/C Virtual Boresight:  \0*.borevec"
                     , cur_item) ;

  } else if ( prnt == roll.aimpt.menu) {
    acted = set_aimpt( &roll.aimpt, w, "Roll Reference Vector:  \0*.rollref"
                     , cur_item) ;

  } else if ( prnt == roll.scvec.menu) {
    acted = set_scvec( &roll.scvec, w, "S/C Roll Vector:  \0*.rollvec"
                     , cur_item) ;
  }
  if ( acted) enterxyz_act( cur_item);
  return;
}

/**********************************************************/
void
elts_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{

  ORBIT *orbit = (ORBIT *) client_data;
  SC *lclsc = (SC *) orbit->_sc;
  ITEM *cur_item = (ITEM *) lclsc->_cur_item;
  Boolean buttonstate = XmToggleButtonGetState(w);

/* set button true if the callback was generated by a toggle to false */

  if ( !buttonstate) XmToggleButtonSetState( w, True, False);

/* find which button this is, & toggle its opposite to false if necessary */

  if ( orbit == sc->_asterOrbit) {
    orbit->_newcenterid = 0;
    if ( buttonstate)
      XmToggleButtonSetState( orbitstr.spice_aster, False, False);

  } else if ( orbit == sc->_earthOrbit) {
    orbit->_newcenterid = 0;
    if ( buttonstate)
      XmToggleButtonSetState( orbitstr.spice_earth, False, False);

  } else if ( orbit == sc->_scOrbit) {
    orbit->_newcenterid = asterid;
    if ( buttonstate)
      XmToggleButtonSetState( orbitstr.spice_sc, False, False);

  }
  if ( orbit->_newutc[0] == '\0') strcpy( orbit->_newutc, utcstr);
  orbitgui_create_elts_dialog( w, orbit);

  return;
}

/**********************************************************/
void
spice_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{

  ORBIT *orbit = (ORBIT *) client_data;
  SC *lclsc = (SC *) orbit->_sc;
  ITEM *cur_item = (ITEM *) lclsc->_cur_item;
  Boolean buttonstate = XmToggleButtonGetState(w);

/* set button true if the callback was generated by a toggle to false */

  if ( !buttonstate) XmToggleButtonSetState( w, True, False);

/* find which button this is, & toggle its opposite to false if necessary */

  if ( orbit == sc->_asterOrbit) {
    if ( buttonstate)
      XmToggleButtonSetState( orbitstr.elts_aster, False, False);

  } else if ( orbit == sc->_earthOrbit) {
    if ( buttonstate)
      XmToggleButtonSetState( orbitstr.elts_earth, False, False);

  } else if ( orbit == sc->_scOrbit) {
    if ( buttonstate)
      XmToggleButtonSetState( orbitstr.elts_sc, False, False);

  }

  orbit->_status = ORB_USESPK;

  return;
}

/**********************************************************/
void
new_utc_CB(w, client_data, call_data)
Widget	w;
XtPointer	client_data;
XtPointer	call_data;
{
  ITEM *cur_item = (ITEM *) client_data;
  static char fmtstr[40], tmpstr[40];
  XmString tmpxmstr;
  Widget scale_lab_W;
  int *utcval;
  int ndig;

#define IFSCALE(SCALEW,LABW,UTCVAL,NDIG) \
  if ( w == SCALEW) { scale_lab_W = LABW; utcval = &UTCVAL; ndig=NDIG; }


  IFSCALE( year_W, year_lab_W, year, 4)
  else IFSCALE( doy_W, doy_lab_W, doy, 3)
  else IFSCALE( hour_W, hour_lab_W, hour, 2)
  else IFSCALE( minute_W, minute_lab_W, minute, 2)
  else IFSCALE( second_W, second_lab_W, second, 2)
  else return;

  XmScaleGetValue( w, utcval);

  UTCLOAD( utc);

  MAKEUTCXMSTR( utcstr, utc_Xmstr);
  XtVaSetValues( utc_label, XmNlabelString, utc_Xmstr, NULL);
  XmSF(utc_Xmstr);

  sprintf( fmtstr, "%%0%dd", ndig);
  strcpy(fmtstr,"    "); sprintf( fmtstr+4-ndig, "%%0%dd", ndig);
  sprintf( tmpstr, fmtstr, *utcval);
  tmpxmstr =  XmStringCreateLtoR( tmpstr, CHARSET);
  XtVaSetValues( scale_lab_W, XmNlabelString, tmpxmstr, NULL);
  XmUpdateDisplay( scale_lab_W);
  XFlush( da_dpy);
  XmSF(tmpxmstr);

  return;
}

/**********************************************************/
void
solveanddraw( ITEM *cur_item)
{
double sx2y2;
int new_Noraz, new_theta, new_phi;
int sav_Noraz, sav_theta, sav_phi;
double phideg, thetadeg, norazdeg;
int i,j,k;
VEC lclvec;

void c_m2eul( MTX, VEC);

  pointing_solve_ok = pointing_solve( utcstr, &et, sc, &bore, &roll);
  if ( !pointing_solve_ok ) return;

  /* convert matrix to theta/phi/noraz, save old values as sav_theta/phi/noraz
   * before call to XmScaleSetValue() so callbacks to
   * new_phi/theta/Noraz_CB() &/or update_phi() do nothing
   */
  c_m2eul( mcamtoabf, lclvec);

  sav_Noraz = old_Noraz; sav_theta = old_theta; sav_phi = old_phi;

  thetadeg = ( lclvec[0] - 90.0);
  thetadeg *= (double) (spudf->eastlon);

  while ( thetadeg < 0.0) thetadeg += 360.0;
  while ( thetadeg > 360.0) thetadeg -= 360.0;
  old_theta = new_theta = (int) floor( 0.5 + thetadeg / pow_theta);

  phideg = ( - lclvec[1]);

  while ( phideg < -90.0) phideg += 360.0;
  while ( phideg > 90.0) phideg -= 360.0;
  old_phi = new_phi = (int) floor( 0.5 + phideg / pow_phi);

  norazdeg = ( - lclvec[2]);

  /* special handling for Noraz to deal with display_northup */
  while ( norazdeg < 0.0) norazdeg += 360.0;
  while ( norazdeg > 360.0) norazdeg -= 360.0;
  old_Noraz = new_Noraz = (int) floor( 0.5 + norazdeg / pow_Noraz);

  XmScaleSetValue( theta_W, old_theta);
  XmScaleSetValue( phi_W, old_phi);
  XmScaleSetValue( Noraz_W, old_Noraz);

  if ( loadImageFrame( sc, et, &currentImgfrm)) {
    currentImgfrmValid = 1;
    copyInstrument_ScToImgfrm( sc, &currentImgfrm);
  }
  else currentImgfrmValid = 0;

  if ( !display_northup) {

    /* copy matrix to R and draw image */
    for (i=k=0; i<3; i++)
      for (j=0; j<3; j++,k++)
        R[i][j] = mabftocam[k];
    draw_image( cur_item, Drawdev_xwin);

  } else {

    /* northup state is set
     * - use update_phi
     * - restore old_* so update_phi() will call draw_image
     *   - update_phi will ignore old/new_Noraz difference & will not
     *     update old_Noraz
     */

    old_phi = sav_phi; old_theta = sav_theta;
    update_phi( new_phi, new_theta, new_Noraz, cur_item);
  }

  return;
}

/**********************************************************/
/* apply time in utc to display
 * ***N.B. this routine is sometimes called with w==NULL
 */
void
apply_utc_CB( Widget w, XtPointer client_data, XtPointer call_data) {
ITEM *cur_item = (ITEM *) client_data;
UTC lclutc = saved_utc;

  saved_utc = utc;
  orbitgui_set_utc( cur_item);
  solveanddraw( cur_item);

  if ( !pointing_solve_ok) {
    utc = saved_utc = lclutc;
    orbitgui_set_utc( cur_item);
    solveanddraw( cur_item);
  }

  return;
}

/**********************************************************/
void
reset_utc_CB(Widget w, XtPointer client_data, XtPointer call_data) {
ITEM *cur_item = (ITEM *) client_data;

  utc = saved_utc;
  orbitgui_set_utc( cur_item);
  return;
}

/**********************************************************/
