/* Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* color_draw.c -- simple drawing program using predefined colors.  */
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
/* #include <Xm/ScrolledW.h> /**/
#include <Xm/Label.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/DialogS.h>
#include "tie_fighter_hot"
#include "X11/bitmaps/star"

#include <math.h>
#include <stdio.h>

#include "local.h"
#include "debug.h"
#include "orbit3d.h"

typedef struct OrbitWidgetStuff_struct {
  Screen *screen;
  Widget topshell;
  Widget da;
  Widget textfields[3];
  Pixmap pixmap;
  double *vals[6];
  double lclvals[3];
} OrbitWidgetStuff;

static OrbitWidgetStuff ows, ows1;

#define INCLINOFFS 0
#define INCLINOFFS2 (INCLINOFFS+INCLINOFFS)
#define LNODEOFFS 1
#define LNODEOFFS2 (LNODEOFFS+LNODEOFFS)

#define ECCOFFS 0
#define ECCOFFS2 (ECCOFFS+ECCOFFS)
#define ARGPOFFS 1
#define ARGPOFFS2 (ARGPOFFS+ARGPOFFS)
#define M0OFFS 2
#define M0OFFS2 (M0OFFS+M0OFFS)

#define inclinval (ows.vals+INCLINOFFS2)
#define lnodeval (ows.vals+LNODEOFFS2)

#define eccval (ows1.vals+ECCOFFS2)
#define argpval (ows1.vals+ARGPOFFS2)
#define m0val (ows1.vals+M0OFFS2)

#define inclin_W ows.textfields[INCLINOFFS]
#define lnode_W ows.textfields[LNODEOFFS]

#define ecc_W ows1.textfields[ECCOFFS]
#define argp_W ows1.textfields[ARGPOFFS]
#define m0_W ows1.textfields[M0OFFS]

#define STARXYWH 0, 0, star_width, star_height
#define SCXYWH 0, 0, tie_fighter_width, tie_fighter_height
static Pixmap scpixmap, starpixmap;
static XPoint starxpt, scxpt;

#define CIRCLERES 73
static XPoint orbitxpt[CIRCLERES];
static XPoint orbitnormhixpt[CIRCLERES];
static XPoint orbitnormloxpt[CIRCLERES];
static XPoint orbithixpt[CIRCLERES];
static XPoint orbitloxpt[CIRCLERES];

static VEC equatorpts[CIRCLERES];
static VEC orbitnormhipts[CIRCLERES];
static VEC orbitnormlopts[CIRCLERES];
static VEC orbithipts[CIRCLERES];
static VEC orbitlopts[CIRCLERES];

#define CONEANG 30.0
#define NORMANG ((90.0-(CONEANG))*PI/180.0)
#define ORBITANG ((CONEANG)*PI/180.0)

static int notfirst;
#ifdef PI
static double TUPI, HALFPI, DEGPR;
#else
static double PI, TUPI, HALFPI, DEGPR;
#endif

static XPoint orbitnormxpt;
static XPoint sunxpt;
static GC gc, gcW, gcB, gc1xor;
static Dimension width, height;

/* static info for eccentricity, longit. of asc. node, and mean anom. */

static Dimension width1, height1;
static XtWidgetGeometry intendedGeom1, replyGeom1;
static XPoint focusxpt;
static XPoint orbitplanxpt[CIRCLERES];

#define NINT(A) ((int)(0.5+(A)))

#define XYZTOLLXPT( XYZ, LAT, LON, XPT) \
  if ( XYZ[1] == 0.0 && XYZ[0] == 0.0) { \
    LON = 0.0; \
    LAT = atan2( XYZ[2], 0.0); \
  } else { \
    LON = atan2( XYZ[1], XYZ[0]); \
    LAT = asin( XYZ[2] / vlen(XYZ)); \
  } \
  if ( LON < 0.0) LON = LON + TUPI; \
  LLTOXPT( LAT, LON, XPT)

#define XPTTOLL( XPT, LAT, LON) \
  LON = (XPT).x * TUPI / width; \
  LAT = - (  ((XPT).y * PI / (height-1)) - HALFPI  ) /* +y is down */

#define LLTOXPT( LAT, LON, XPT) \
  (XPT).x=NINT( (LON) * width / TUPI ); \
  while ((XPT).x < 0) (XPT).x += width; \
  while ((XPT).x > (width-1)) (XPT).x -= width; \
  (XPT).y= (height-1) - NINT( (HALFPI+(LAT)) * (height-1) / PI )

#define LLTOXYZ( LAT, LON, XYZ) \
  XYZ[0] = cos(LON) * cos(LAT); \
  XYZ[1] = sin(LON) * cos(LAT); \
  XYZ[2] = sin(LAT)

/* utility routine - find top level shell */

Widget
orbitset_findtop( Widget w) {
  while (w && !XtIsWMShell (w)) w = XtParent (w);
  return(w);
}

/* map plan view of ellipse with xptin representing e & omega */

void
makegcs( Display *dpy, Widget main_w) {
XGCValues gcv;
Window root;
Screen *screen;

  /* Create a GC for drawing (callback).  Used a lot -- make global */
  if ( !gcW) {
    screen = XtScreen( main_w);
    root = RootWindowOfScreen( screen);

    gcv.foreground = WhitePixelOfScreen( screen);
    gcW = XCreateGC ( dpy, root, GCForeground, &gcv);
    gc = XCreateGC ( dpy, root, GCForeground, &gcv);

    gcv.foreground = BlackPixelOfScreen( screen);
    gcB = XCreateGC ( dpy, root, GCForeground, &gcv);

    gcv.foreground = BlackPixelOfScreen( screen);
    gcv.background = WhitePixelOfScreen( screen) ^ gcv.foreground;
    gcv.function = GXxor;
    gc1xor = XCreateGC ( dpy, root
                    , GCForeground | GCBackground | GCFunction, &gcv);
  }
}

/* calculate orbiter position from m0, ecc & argp
 */
void
makeorbitspots1b(void) {
double xxx, yyy, ecc, ecc2, tanalpha, a, b, c, delang, xdx, frac, fracm1;
int idx;

  while ( *m0val[1] < 0.0) *m0val[1] += TUPI;
  while ( *m0val[1] >= TUPI) *m0val[1] -= TUPI;

  xxx = cos( *m0val[1]); yyy = sin( *m0val[1]);

  ecc = *eccval[1];
  ecc2 = 1.0 - ecc * ecc;

  /* *m0val[1] is angle wrt focus, now solve for ang of m0 wrt ell. center
   * - first for non-circular and m0 != 90 or 270
   */
  if ( xxx != 0.0 && ecc > 0.0) {
    tanalpha = yyy / xxx;
    a = 1.0 + (tanalpha*tanalpha) / ecc2;
    b = 2.0 * ecc;
    c = - ecc2;
    if ( xxx > 0.0) xxx = ( - b + sqrt( b*b - 4.0 * a * c)) / (2.0 * a);
    else xxx = ( - b - sqrt( b*b - 4.0 * a * c)) / (2.0 * a);
    delang = atan2( xxx*tanalpha, ecc+xxx);

  /* - next for non-circular & m0 = 90 or 270
   */
  } else {
    if ( xxx == 0.0 && ecc > 0.0) {
      if ( yyy > 0.0) { delang = atan2( ecc2, ecc); }
      else { delang = atan2( -ecc2, ecc); }

    /* - finally for circular orbit, delang = *m0val[1]
     */
    } else { delang = *m0val[1]; }
  }

  /* bring delang into [0-2PI) range */
  while( delang < 0.0) delang += TUPI;

  /* find index into orbitplanxpt[] just below delang */
  xdx = delang * (CIRCLERES-1) / TUPI;
  idx = (int) xdx;
  if ( idx == (CIRCLERES-1)) idx = xdx = 0;

  /* interpolate orbitplanxpt[] to get orbiter position */
  frac = xdx - idx;
  fracm1 = 1.0 - frac;
  scxpt.x = (orbitplanxpt[idx].x * fracm1) + (orbitplanxpt[idx+1].x * frac);
  scxpt.y = (orbitplanxpt[idx].y * fracm1) + (orbitplanxpt[idx+1].y * frac);

  /* adjust for hot spot & XOR new orbiter position into pixmap */
  scxpt.x -= tie_fighter_x_hot;
  scxpt.y -= tie_fighter_y_hot;
}

/* calculate orbit points from ecc & argp
 */
void
makeorbitrack1a( XPoint *xptout) {
double ecc2, cosap, sinap, tanth, costh, xxx, yyy, theta;
double x, y;
int w2 = (width1-1) / 2;
int h2 = (height1-1) / 2;
int i;

  cosap = cos( *argpval[1]);
  sinap = sin( *argpval[1]);

  /* save 1-ecc^2 in ecc2 */
  ecc2 = 1.0 - (*eccval[1] * *eccval[1]);

  /* get x,y coordinates for every point in orbit */
  for ( i=0; i<CIRCLERES; i++) {
    theta = TUPI * ((double)i) / (CIRCLERES-1);
    costh = cos( theta);
    if ( -0.0001 < costh && costh < 0.0001) {
      tanth = sin(theta) * 10000.0;
      costh = 0.0001;
    } else tanth = tan( theta);
    x = 1.0 / sqrt( 1.0 + (tanth * tanth / ecc2));
    if ( costh < 0.0) x = -x;
    y = tanth * x;
    xxx = x * cosap - y * sinap;
    yyy = x * sinap + y * cosap;
    xptout[i].x = NINT( w2 * (1.0 + xxx));
    xptout[i].y = NINT( h2 * (1.0 - yyy));
  }
  return;
}

/* calculate focus point from ecc & argp
 * - reset ecc if ecc>0.999
 */
void
makeorbitspots1a(XPoint *xptin) {
double xxx, yyy;
double h2=((height1-1)/2.0);
double w2=((width1-1)/2.0);

  if ( *eccval[1] > 0.999) *eccval[1] = 0.999;

  xxx = *eccval[1] * cos( *argpval[1]);
  yyy = *eccval[1] * sin( *argpval[1]);

  xptin->x = NINT(w2 * (1.0 + xxx));
  xptin->y = NINT(h2 * (1.0 - yyy));

  return;
}

/* calculate ecc, argp, & orbit points from focus point
 * - recalculate focus point if ecc>0.999
 */
void
makeorbitrack1(XPoint *xptin, XPoint *xptout) {
double xxx, yyy, x, y, ecc2, tanang, cosang, ang, cosap, sinap;
double theta, tanth, costh;
double h2=((height1-1)/2.0);
double w2=((width1-1)/2.0);

  /* scale x, y; get omega from x, y */
  xxx = ((*xptin).x - w2) / w2;
  yyy = (h2 - (*xptin).y) / h2;

  /* calculate eccentricity */
  ecc2 = (xxx*xxx) + (yyy*yyy);
  if ( ecc2 > 0.998001) {
    *eccval[1] = 0.999 / sqrt( ecc2);
    xxx *= *eccval[1];
    yyy *= *eccval[1];
    xptin->x = NINT(w2 * (1.0 + xxx));
    xptin->y = NINT(h2 * (1.0 - yyy));
    *eccval[1] = 0.999;
    ecc2 = *eccval[1] * *eccval[1];

  } else *eccval[1] = sqrt(ecc2);

  if ( *eccval[1] == 0.0) *argpval[1] = 0.0;
  else *argpval[1] = atan2( yyy, xxx);

  makeorbitrack1a( xptout);
  return;
}


/* exit orbit windows
 */
void
orbitset_exit_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
OrbitWidgetStuff *lclows = (OrbitWidgetStuff *) client_data;
Pixmap *pixptr;
XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;

  if ( lclows->pixmap) XmDestroyPixmap( lclows->screen, lclows->pixmap);
  if ( w != lclows->topshell) XtDestroyWidget( lclows->topshell);
  lclows->pixmap = (Pixmap) 0;

  return;
}

/* load double precision value into text field
 */
void
orbitset_setval1( Widget w, double x) {
char lclstr[40];

  sprintf( lclstr, "%.7g", x);
  XmTextFieldSetString( w, lclstr);
  return;
}

/* callback routine for when any one of the ecc/argp/m0 values are changed
 */
void
orbitset_readval1(Widget widget, XtPointer client_data, XtPointer call_data)
{
String value = XmTextFieldGetString( widget);
int ioffs = (int) client_data;
double lcldbl;
double **lcldblptr;
int i;
Display *dpy = XtDisplay( ows1.da);

XSync( dpy, True), XFlush( dpy);

  if ( !(i = sscanf( value, "%lf", &lcldbl)) ) return;

  switch (ioffs) {
  case ECCOFFS:
    if ( lcldbl < 0.0 || lcldbl > 0.999) return;
    if ( lcldbl == 0.0) {
      *argpval[1] = 0.0;
      orbitset_setval1( argp_W, 0.0);
    }
    break;
  case ARGPOFFS:
  case M0OFFS:
    while( lcldbl >= 360.0) lcldbl -= 360.0;
    while( lcldbl < 0.0) lcldbl += 360.0;
    lcldbl /= DEGPR;
    break;
  default:
    return;
  }

  lcldblptr = ows1.vals + (ioffs+ioffs);
  *lcldblptr[1] = lcldbl;

  makeorbitspots1a( &focusxpt);
  starxpt.x = focusxpt.x - star_x_hot;
  starxpt.y = focusxpt.y - star_y_hot;
  makeorbitrack1a( orbitplanxpt);
  makeorbitspots1b();

  XFillRectangle ( dpy, ows1.pixmap, gcW, 0, 0, width1, height1);
  XDrawLines( dpy, ows1.pixmap, gcB, orbitplanxpt, CIRCLERES, 0);
  XCopyArea( dpy, starpixmap, ows1.pixmap, gc1xor, STARXYWH
               , starxpt.x, starxpt.y);
  XCopyArea( dpy, scpixmap, ows1.pixmap, gc1xor, SCXYWH, scxpt.x, scxpt.y);
  XCopyArea( dpy, ows1.pixmap, XtWindow(ows1.da), gcB
           , 0, 0, width1, height1, 0, 0);
  
  return;
}

/* callback routine for when any of the color tiles are pressed.
 * This general function may also be used to set the global gc's
 * color directly.  Just provide a widget and a color name.
 */
void
set_color(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    String color = (String) client_data;
    Display *dpy = XtDisplay (widget);
    Colormap cmap = DefaultColormapOfScreen (XtScreen (widget));
    XColor col, unused;

    if (!XAllocNamedColor (dpy, cmap, color, &col, &unused)) {
        char buf[32];
        sprintf (buf, "Can't alloc %s", color);
        XtWarning (buf);
        return;
    }
    XSetForeground (dpy, gc, col.pixel);
}

/* Action procedure to respond to any of the events from the
 * translation table declared in orbitset1_CB().  This function is called
 * in response to Button1 Down, Up and Motion events.  Basically,
 * we're just undrawing the old & drawing the new orbit track
 */

void
draw1(widget, event, args, num_args)
Widget widget;
XEvent *event;
String *args;
int *num_args;
{
XButtonEvent *bevent = (XButtonEvent *) event;
int i;
Display *dpy = XtDisplay( widget);

    if (*num_args != 1)
        XtError ("Wrong number of args!");

    /* Button 1: move focus */

    if ( !strcmp( *args, "focus")) {

      /* undraw previous orbit */
      XFillRectangle ( dpy, ows1.pixmap, gcW
                     , starxpt.x, starxpt.y, star_width, star_height);
      XFillRectangle ( dpy, ows1.pixmap, gcW
                     , scxpt.x, scxpt.y, tie_fighter_width, tie_fighter_height);
  
      XDrawLines( dpy, ows1.pixmap, gcW, orbitplanxpt, CIRCLERES, 0);
  
      /* create new orbit track, draw new orbit */
  
      focusxpt.x = bevent->x;
      focusxpt.y = bevent->y;
 
      makeorbitrack1( &focusxpt, orbitplanxpt);
      XDrawLines( dpy, ows1.pixmap, gcB, orbitplanxpt, CIRCLERES, 0);
  
      starxpt.x = focusxpt.x - star_x_hot;
      starxpt.y = focusxpt.y - star_y_hot;
 
/*      /* when changing ecc or argp via this routine, set m0 to 0 */
/*      *m0val[1] = 0.0;  
/*      orbitset_setval1( m0_W, 0.0);
/*      scxpt.x = orbitplanxpt[0].x - tie_fighter_x_hot;
/*      scxpt.y = orbitplanxpt[0].y - tie_fighter_y_hot;
/**/
      makeorbitspots1b();
  
      XCopyArea( dpy, starpixmap, ows1.pixmap, gc1xor, STARXYWH
               , starxpt.x, starxpt.y);
      XCopyArea( dpy, scpixmap, ows1.pixmap, gc1xor, SCXYWH, scxpt.x, scxpt.y);

      orbitset_setval1( ecc_W, *eccval[1]);
      orbitset_setval1( argp_W, *argpval[1] * DEGPR);

    /* Button 3:  move orbiter */

    } else {

      /* erase old orbiter */
      XCopyArea( dpy, scpixmap, ows1.pixmap, gc1xor, SCXYWH, scxpt.x, scxpt.y);

      /* get mean anomaly relative to argp */
      if ( bevent->x == focusxpt.x && bevent->y == focusxpt.y) {
        *m0val[1] = 0.0;
      } else {
        *m0val[1] = atan2( - (double)(bevent->y - focusxpt.y)
                         , (double)(bevent->x - focusxpt.x) );
      }
      *m0val[1] -= *argpval[1];

      makeorbitspots1b();
      orbitset_setval1( m0_W, *m0val[1] * DEGPR);

      XCopyArea( dpy, scpixmap, ows1.pixmap, gc1xor, SCXYWH, scxpt.x, scxpt.y);
      
    }

    /* copy pixmap to window */
    XCopyArea (bevent->display, ows1.pixmap, bevent->window, gcB,
        0, 0, width1, height1, 0, 0);
    return;
}

/* draw a mapped circle on a pixmap */
void
xdraw( Display *dpy, Pixmap pixmap, XPoint *xdrawxpt)
{
int i, i2;

  /* don't draw any lines wider that cover more than 1/2 width of screen */

  for ( i=0
      ; i<(CIRCLERES-1) && abs(xdrawxpt[i].x - xdrawxpt[i+1].x) <= (width/2)
      ; i++) ;

  if ( i==(CIRCLERES-1) && abs(xdrawxpt[i].x - xdrawxpt[i-1].x) <= (width/2)) 
    XDrawLines( dpy, pixmap, gc, xdrawxpt, CIRCLERES, 0);

  else {

    if ( i)
      XDrawLines( dpy, pixmap, gc, xdrawxpt, i+1, 0);

    i2 = i + 1;

    for ( i=i2
      ; i<(CIRCLERES-1) && abs(xdrawxpt[i].x - xdrawxpt[i+1].x) <= (width/2)
      ; i++) ;

    if ( i==(CIRCLERES-1) && abs(xdrawxpt[i].x - xdrawxpt[i-1].x) <= (width/2))
       XDrawLines( dpy, pixmap, gc, xdrawxpt+i2, CIRCLERES-i2, 0);

    else {

      if ( i > i2)
        XDrawLines( dpy, pixmap, gc, xdrawxpt+i2, i+1-i2, 0);

      if ( i < (CIRCLERES-1) )
        XDrawLines( dpy, pixmap, gc
                  , xdrawxpt+(i+1), CIRCLERES-(i+1), 0);
     }
  }
  return;
}

/* redraw is called whenever 
 * - all or portions of the drawing area is exposed.  
 *   - This includes newly exposed portions of the widget resulting
 *     from the user's interaction with the scrollbars.
 */
void
redraw( Widget da, XtPointer client_data, XtPointer call_data)
{
Display *dpy = XtDisplay (da);
Dimension ww,hh;
long redraw_orbits = (long) client_data;

  if ( redraw_orbits) {
    /* erase */
    XFillRectangle (dpy, ows.pixmap, gcW, 0, 0, width, height);
  
    /* draw orbit normal limits */
    set_color( da, (String) "Blue");
    xdraw( dpy, ows.pixmap, orbitnormhixpt);
    xdraw( dpy, ows.pixmap, orbitnormloxpt);
  
    /* draw orbit track limits */
    set_color( da, (String) "Red");
    xdraw( dpy, ows.pixmap, orbithixpt);
    xdraw( dpy, ows.pixmap, orbitloxpt);
  
    /* draw orbit */
    set_color( da, (String) "Black");
    xdraw( dpy, ows.pixmap, orbitxpt);
  }

/* copy to window */
  XCopyArea ( dpy, ows.pixmap, XtWindow( da), gc,
        0, 0, width, height, 0, 0);

  return;
}

/* map track of circlepts rotated to lat,lon represented by *xptin.y,x */

void
makeorbitrack(XPoint *xptin, VEC *circlepts, XPoint *xptout) {
double lon;
double lat;
VEC orbitnorm;
VEC v1;
MTX m1, m2, morbit;
int i;

/* convert xptin to lat, lon, then to orbit normal */

  XPTTOLL( *xptin, lat, lon);
  LLTOXYZ( lat, lon, orbitnorm);

/* get vector that rotates vectors into orbit coordinates
 * - find matrix, m1, that rotates orbitnorm into ZX plane as v1
 * - find matrix, m2, that rotates v1 to Z axis
 * - m1 x m2 = transpose of desired matrix
 */

  Rot2AbyB( 'x', 'z', orbitnorm, m1);
  vxm( orbitnorm, m1, v1);
  Rot2AbyB( 'z', 'y', v1, m2);
  mxm( m1, m2, morbit);
  MT( morbit);

/* rotate equator points into orbit, convert them to xptout */

  for ( i=0; i<CIRCLERES; i++) {
    vxm( circlepts[i], morbit, v1);
    XYZTOLLXPT( v1, lat, lon, xptout[i]);
  }
  return;
}

/* we're just undrawing the old & drawing the new orbit track
 */
void
orbitset_update( int dosetval)
{
Window window = XtWindow( ows.da);
Display *dpy = XtDisplay( ows.da);
int i;

  /* fill in text fields */

  if ( dosetval) {
    orbitset_setval1( inclin_W, *inclinval[1] * DEGPR);
    orbitset_setval1( lnode_W, *lnodeval[1] * DEGPR);
  }

  /* undraw previous orbit */

  set_color( ows.da, (String) "White");
  xdraw( dpy, ows.pixmap, orbitxpt);

  /* draw limit lines in case they were damaged by undraw */
  set_color( ows.da, (String) "Blue");
  xdraw( dpy, ows.pixmap, orbitnormhixpt);
  xdraw( dpy, ows.pixmap, orbitnormloxpt);
  set_color( ows.da, (String) "Red");
  xdraw( dpy, ows.pixmap, orbithixpt);
  xdraw( dpy, ows.pixmap, orbitloxpt);

  /* create new orbit track, draw new orbit */

  LLTOXPT( HALFPI - *inclinval[1], *lnodeval[1] - HALFPI, orbitnormxpt);
/*
  orbitnormxpt.x = NINT( width * (*lnodeval[1] - HALFPI) / TUPI);
  if( orbitnormxpt.x == width) orbitnormxpt.x = 0;
  if( orbitnormxpt.x < 0) orbitnormxpt.x += width;
  orbitnormxpt.y = NINT( (height-1) * (HALFPI - *inclinval[1]) / PI);
*/
  makeorbitrack( &orbitnormxpt, equatorpts, orbitxpt);

  set_color( ows.da, (String) "Black");
  xdraw( dpy, ows.pixmap, orbitxpt);

  /* copy to window */

  XCopyArea (dpy, ows.pixmap, window, gc, 0, 0, width, height, 0, 0);

}

/* callback routine for when any one of the inclin/lnode values are changed
 */
void
orbitset_readval(Widget widget, XtPointer client_data, XtPointer call_data)
{
String value = XmTextFieldGetString( widget);
int ioffs = (int) client_data;
double lcldbl;
double **lcldblptr;
int i;
Display *dpy = XtDisplay( ows1.da);

  if ( !(i = sscanf( value, "%lf", &lcldbl)) ) return;

  switch (ioffs) {
  case INCLINOFFS:
    if ( lcldbl > 180.0 || lcldbl < 0.0) {
      orbitset_setval1( widget, *inclinval[1] * DEGPR);
      return;
    }
    lcldbl /= DEGPR;
    break;
  case LNODEOFFS:
    if ( lcldbl >= 0.0 && lcldbl < 360.0 ) {
    } else {
      if ( lcldbl < 0.0) { while( lcldbl >= 360.0) lcldbl -= 360.0; }
      else { while( lcldbl < 0.0) lcldbl += 360.0; }
      orbitset_setval1( widget, lcldbl);
    }
    lcldbl /= DEGPR;
    break;
  default:
    return;
  }

  lcldblptr = ows.vals + (ioffs+ioffs);
  *lcldblptr[1] = lcldbl;

  orbitset_update( (int) 0);

  return;
}

/* Action procedure to respond to any of the events from the
 * translation table declared in orbitset_CB().  This function is called
 * in response to Button1 Down, Up and Motion events.  Basically,
 * we're just undrawing the old & drawing the new orbit track
 */
void
draw(widget, event, args, num_args)
Widget widget;
XEvent *event;
String *args;
int *num_args;
{
XButtonEvent *bevent = (XButtonEvent *) event;
int i;

    if (*num_args != 1)
        XtError ("Wrong number of args!");

    /* undraw previous orbit */

    set_color( widget, (String) "White");
    xdraw( bevent->display, ows.pixmap, orbitxpt);

    /* draw limit lines in case they were damaged by undraw */
    set_color( widget, (String) "Blue");
    xdraw( bevent->display, ows.pixmap, orbitnormhixpt);
    xdraw( bevent->display, ows.pixmap, orbitnormloxpt);
    set_color( widget, (String) "Red");
    xdraw( bevent->display, ows.pixmap, orbithixpt);
    xdraw( bevent->display, ows.pixmap, orbitloxpt);

    /* create new orbit track, draw new orbit */

    orbitnormxpt.x = bevent->x;
    orbitnormxpt.y = bevent->y;

    if ( orbitnormxpt.x < 0) orbitnormxpt.x = 0;
    else if ( orbitnormxpt.x >= width) orbitnormxpt.x = width-1;
    if ( orbitnormxpt.y < 0) orbitnormxpt.y = 0;
    else if ( orbitnormxpt.y >= height) orbitnormxpt.y = height-1;

    makeorbitrack( &orbitnormxpt, equatorpts, orbitxpt);

    set_color( widget, (String) "Black");
    xdraw( bevent->display, ows.pixmap, orbitxpt);

    /* copy to window */

    XCopyArea (bevent->display, ows.pixmap, bevent->window, gc,
        0, 0, width, height, 0, 0);

  XPTTOLL( orbitnormxpt, *inclinval[1], *lnodeval[1]);
  *inclinval[1] = HALFPI - *inclinval[1];
  *lnodeval[1] += HALFPI;
  orbitset_setval1( lnode_W, *lnodeval[1] * DEGPR);
  orbitset_setval1( inclin_W, *inclinval[1] * DEGPR);

  return;
}

void 
newsun(widget, event, args, num_args)
Widget widget;
XEvent *event;
String *args;
int *num_args;
{
XtPointer clientd, calld;
XButtonEvent *bevent = (XButtonEvent *) event;

  sunxpt.x = bevent->x;
  sunxpt.y = bevent->y;

  if ( sunxpt.x < 0) sunxpt.x = 0;
  else if ( sunxpt.x >= width) sunxpt.x = width-1;
  if ( sunxpt.y < 0) sunxpt.y = 0;
  else if ( sunxpt.y >= height) sunxpt.y = height-1;

  orbitnormxpt = sunxpt;
  makeorbitrack( &orbitnormxpt, equatorpts, orbitxpt);
  makeorbitrack( &sunxpt, orbitnormhipts, orbitnormhixpt);
  makeorbitrack( &sunxpt, orbitnormlopts, orbitnormloxpt);
  makeorbitrack( &sunxpt, orbithipts, orbithixpt);
  makeorbitrack( &sunxpt, orbitlopts, orbitloxpt);

  clientd = (XtPointer) 1;
  redraw( widget, clientd, calld);

  return;
}

/* initialisation - do once */

void
orbitset_init( Widget w) {
int i;
double lon;
/* XtActionsRec actions[4]; /**/

  if ( notfirst) return;

  notfirst = 1;
#ifndef PI
  PI = acos( -1.0);
#endif
  HALFPI = PI / 2.0;
  TUPI = PI * 2.0;
  DEGPR = 180.0 / PI;
  for ( i=0; i<CIRCLERES; i++) {
    lon = TUPI * (i+0.5) / (CIRCLERES-1);
    LLTOXYZ( 0.0, lon, equatorpts[i]);
    LLTOXYZ( NORMANG, lon, orbitnormhipts[i]);
    orbitnormlopts[i][0] = orbitnormhipts[i][0],
    orbitnormlopts[i][1] = orbitnormhipts[i][1],
    orbitnormlopts[i][2] = -orbitnormhipts[i][2];
    LLTOXYZ( ORBITANG, lon, orbithipts[i]);
    orbitlopts[i][0] = orbithipts[i][0],
    orbitlopts[i][1] = orbithipts[i][1],
    orbitlopts[i][2] = -orbithipts[i][2];
  }

  /* Add the "draw" action/function used by the translation table
   * parsed by the translations resource below.
   */
/*  actions[0].string = "draw";
/*  actions[0].proc = draw;
/*  actions[1].string = "newsun";
/*  actions[1].proc = newsun;
/*  actions[2].string = "draw1";
/*  actions[2].proc = draw1;
/*  XtAppAddActions (XtWidgetToApplicationContext(w), actions, 3); /**/

  return;
}

/* redraw1 is called whenever 
 * - all or portions of the drawing area is exposed.  
 *   - This includes newly exposed portions of the widget resulting
 *     from the user's interaction with the scrollbars.
 */
void
redraw1(w, client_data, call_data)
Widget    w;
XtPointer client_data;
XtPointer call_data;
{
Widget da = (Widget) client_data;
Display *dpy = XtDisplay (da);

  /* copy whole pixmap to window */
  XCopyArea ( dpy, ows1.pixmap, XtWindow( da), gcB,
        0, 0, width1, height1, 0, 0);

  return;
}

/* handle button press from inclin, lnode window
 */
void
orbitset_button_CB( Widget pb, XtPointer client_data, XtPointer call_data)
{
int option = (int) client_data;

  switch( option) {
  case XmDIALOG_OK_BUTTON:
    orbitset_readval( inclin_W, (XtPointer) INCLINOFFS, call_data);
    orbitset_readval( lnode_W, (XtPointer) LNODEOFFS, call_data);
    *inclinval[0] = *inclinval[1];
    *lnodeval[0] = *lnodeval[1];
  case XmDIALOG_CANCEL_BUTTON:
    orbitset_exit_CB( pb, (XtPointer) &ows, call_data);
    break;
  case XmDIALOG_DEFAULT_BUTTON:
    *inclinval[1] = *inclinval[0];
    *lnodeval[1] = *lnodeval[0];
    orbitset_update((int) 1);
    break;
  }
  return;
}

void
orbitset_CB( Widget pb0, XtPointer client_data, XtPointer call_data)
{
Widget main_w, sw, drawing_a, pb, lbltxt, bottomw;
Widget toplevel = orbitset_findtop( pb0);
/* XtAppContext app; /**/
XGCValues gcv;
int i;
double lat, lon;
String translations = /* for the DrawingArea widget */
    "<Btn1Down>:   draw(down)\n\
     <Btn1Motion>: draw(motion)\n\
     <Btn3Down>:   newsun(down)\n\
     <Btn3Motion>: newsun(motion)";
Display *dpy;
double **dblptrs = (double **) client_data;

  if ( ows.pixmap) return;

  orbitset_init( pb0);

  ows.topshell = XtVaCreateManagedWidget( 
      "Inclination & Longitude of ascending node"
      , xmDialogShellWidgetClass, toplevel
      , XmNallowShellResize, True
      , NULL);
  XtAddCallback (ows.topshell, XmNpopdownCallback, orbitset_exit_CB, &ows);

  /* Create a MainWindow to contain the drawing area */
  main_w = XtVaCreateManagedWidget( "main_w", xmFormWidgetClass, ows.topshell
      , XmNautoUnmanage, False
      , NULL);

  dpy = XtDisplay( main_w);
  makegcs( dpy, main_w);

  ows.screen = XtScreen( main_w);

  pb = XtVaCreateManagedWidget ("OK",
       xmPushButtonGadgetClass, main_w,
       XmNleftAttachment,    XmATTACH_FORM,
       XmNbottomAttachment,     XmATTACH_FORM,
       NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_button_CB
               , XmDIALOG_OK_BUTTON);

  pb = XtVaCreateManagedWidget ("Reset",
       xmPushButtonGadgetClass, main_w,
       XmNleftAttachment,    XmATTACH_WIDGET,
       XmNleftWidget,        pb,
       XmNbottomAttachment,     XmATTACH_FORM,
       NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_button_CB
               , XmDIALOG_DEFAULT_BUTTON);

  bottomw =
  pb = XtVaCreateManagedWidget ("Cancel",
       xmPushButtonGadgetClass, main_w,
       XmNleftAttachment,    XmATTACH_WIDGET,
       XmNleftWidget,        pb,
       XmNbottomAttachment,     XmATTACH_FORM,
       NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_button_CB
               , XmDIALOG_CANCEL_BUTTON);

  lbltxt = XtVaCreateManagedWidget( "Longit of Asc. Node deg"
           , xmLabelWidgetClass,      main_w
           , XmNleftAttachment,       XmATTACH_FORM
           , XmNbottomAttachment,     XmATTACH_WIDGET
           , XmNbottomWidget,         bottomw
           , NULL);

  bottomw =
  lnode_W = XtVaCreateManagedWidget ( "lnodelbl"
         , xmTextFieldWidgetClass, main_w
         , XmNrightAttachment,    XmATTACH_FORM
         , XmNleftAttachment,     XmATTACH_WIDGET
         , XmNleftWidget,         lbltxt
         , XmNbottomAttachment,     XmATTACH_WIDGET
         , XmNbottomWidget,         bottomw
         , NULL);
  XtAddCallback (lnode_W, XmNactivateCallback, orbitset_readval, LNODEOFFS);
  lnodeval[1] = ows.lclvals+LNODEOFFS;
  lnodeval[0] = dblptrs[LNODEOFFS];
  *lnodeval[1] = *lnodeval[0];
  orbitset_setval1( lnode_W, *lnodeval[1] * DEGPR);

  lbltxt = XtVaCreateManagedWidget ("Inclination, deg"
           , xmLabelWidgetClass,      main_w
           , XmNleftAttachment,       XmATTACH_FORM
           , XmNbottomAttachment,     XmATTACH_WIDGET
           , XmNbottomWidget,         bottomw
           , NULL);

  bottomw =
  inclin_W = XtVaCreateManagedWidget ( "inclinlbl"
         , xmTextFieldWidgetClass, main_w
         , XmNrightAttachment,    XmATTACH_FORM
         , XmNleftAttachment,     XmATTACH_WIDGET
         , XmNleftWidget,         lbltxt
         , XmNbottomAttachment,     XmATTACH_WIDGET
         , XmNbottomWidget,         bottomw
         , NULL);
  XtAddCallback (inclin_W, XmNactivateCallback, orbitset_readval, INCLINOFFS);
  inclinval[1] = ows.lclvals+INCLINOFFS;
  inclinval[0] = dblptrs[INCLINOFFS];
  *inclinval[1] = *inclinval[0];
  orbitset_setval1( inclin_W, *inclinval[1] * DEGPR);


  /* Create a DrawingArea widget.  Make it approx twice as wide as tall.
   * - make height odd (so 0 lat pixel exists)
   */

  width=600; height=301;
  drawing_a = XtVaCreateManagedWidget ("drawing_a",
      xmDrawingAreaWidgetClass, main_w,
      XmNtranslations, XtParseTranslationTable (translations),
      XmNwidth,        width,
      XmNheight,       height,
      XmNresizePolicy, XmNONE,  /* keep this a fixed size */

      XmNbottomAttachment,          XmATTACH_WIDGET,
      XmNbottomWidget,              bottomw,
      XmNleftAttachment,         XmATTACH_FORM,
      XmNtopAttachment,          XmATTACH_FORM,
      XmNrightAttachment,        XmATTACH_FORM,

      NULL);

  ows.da = drawing_a;

  /* When scrolled, the drawing area will get expose events */
  XtAddCallback (drawing_a, XmNexposeCallback, redraw, (XtPointer) 0);

  /* create a pixmap the same size as the drawing area. */
  ows.pixmap = XCreatePixmap( dpy, RootWindowOfScreen( ows.screen)
                            , width, height
                            , DefaultDepthOfScreen(ows.screen));

  /* clear pixmap with white */
  XFillRectangle ( dpy, ows.pixmap, gcW, 0, 0, width, height);

  /* set sun initially on equator */
  sunxpt.x = 0;
  sunxpt.y = (height-1) / 2;
/*  orbitnormxpt = sunxpt; */
  LLTOXPT( HALFPI - *inclinval[1], *lnodeval[1]-HALFPI, orbitnormxpt);
  makeorbitrack( &orbitnormxpt, equatorpts, orbitxpt);
  makeorbitrack( &sunxpt, orbitnormhipts, orbitnormhixpt);
  makeorbitrack( &sunxpt, orbitnormlopts, orbitnormloxpt);
  makeorbitrack( &sunxpt, orbithipts, orbithixpt);
  makeorbitrack( &sunxpt, orbitlopts, orbitloxpt);
  redraw( drawing_a, (XtPointer) 1, (XtPointer) NULL);

  XtManageChild(main_w);
  XtPopup(XtParent(main_w), XtGrabNone);

}

/* update text fields and display for ecc, argp & m0
 */
void
orbitset_update1( void)
{
Window window = XtWindow( ows1.da);
Display *dpy = XtDisplay( ows1.da);
int i;

  orbitset_setval1( ecc_W, *eccval[1]);
  orbitset_setval1( argp_W, *argpval[1] * DEGPR);
  orbitset_setval1( m0_W, *m0val[1] * DEGPR);

  makeorbitspots1a( &focusxpt);
  starxpt.x = focusxpt.x - star_x_hot;
  starxpt.y = focusxpt.y - star_y_hot;
  makeorbitrack1a( orbitplanxpt);
  makeorbitspots1b();
  XFillRectangle ( dpy, ows1.pixmap, gcW, 0, 0, width1, height1);
  XDrawLines( dpy, ows1.pixmap, gcB, orbitplanxpt, CIRCLERES, 0);
  XCopyArea( dpy, starpixmap, ows1.pixmap, gc1xor, STARXYWH
               , starxpt.x, starxpt.y);
  XCopyArea( dpy, scpixmap, ows1.pixmap, gc1xor, SCXYWH, scxpt.x, scxpt.y);
  XCopyArea( dpy, ows1.pixmap, XtWindow(ows1.da), gcB
           , 0, 0, width1, height1, 0, 0);
  
  return;

}

/* callback for buttons in ecc, argp, m0 window
 */
void
orbitset_button1_CB( Widget pb, XtPointer client_data, XtPointer call_data)
{
int option = (int) client_data;
XmAnyCallbackStruct cbs;

cbs.event = (XEvent *) 0;

  switch( option) {
  case XmDIALOG_OK_BUTTON:
    orbitset_readval1( ecc_W, (XtPointer) ECCOFFS, call_data);
    orbitset_readval1( argp_W, (XtPointer) ARGPOFFS, call_data);
    orbitset_readval1( m0_W, (XtPointer) M0OFFS, call_data);
    *eccval[0] = *eccval[1];
    *argpval[0] = *argpval[1];
    *m0val[0] = *m0val[1];
  case XmDIALOG_CANCEL_BUTTON:
    orbitset_exit_CB( pb, (XtPointer) &ows1, (XtPointer) &cbs);
    break;
  case XmDIALOG_DEFAULT_BUTTON:
    *eccval[1] = *eccval[0];
    *argpval[1] = *argpval[0];
    *m0val[1] = *m0val[0];
    orbitset_update1();
    break;
  default:
    break;
  }
  return;
}

void
orbitset1_CB( Widget pb0, XtPointer client_data, XtPointer call_data)
{
Widget main_w, sw, rc, drawing_a1, pb, lbltxt, bottomw;
Widget toplevel = orbitset_findtop( pb0);
/* XtAppContext app; /**/
XGCValues gcv;
int i;
double lat, lon;
String translations = /* for the DrawingArea widget */
    "<Btn1Down>:   draw1(focus)\n\
     <Btn1Motion>: draw1(focus)\n\
     <Btn3Down>:   draw1(meananom)\n\
     <Btn3Motion>: draw1(meananom)\n";
Display *dpy;
double **dblptrs = (double **) client_data;

  if ( ows1.pixmap) return;

  orbitset_init( pb0);

  ows1.topshell = XtVaCreateManagedWidget( 
    "Eccentricity & Arg. of periapse (Button 1); Mean anomaly (Button 3)"
    , xmDialogShellWidgetClass, toplevel
    , XmNallowShellResize, True
    , NULL);
  XtAddCallback (ows1.topshell, XmNpopdownCallback, orbitset_exit_CB, &ows1);

  /* Create a MainWindow to contain the drawing area */
  main_w = XtVaCreateManagedWidget( "main_w", xmFormWidgetClass, ows1.topshell
           , XmNautoUnmanage, False
           , NULL);

  dpy = XtDisplay( main_w);
  makegcs( dpy, main_w);

  ows1.screen = XtScreen(main_w);

  /* use tie_fighter pixmap with hotspot for spacecraft 
   * & use star for body center
   */
  if ( !scpixmap) scpixmap = XCreatePixmapFromBitmapData(
    dpy, DefaultRootWindow(dpy)
    , tie_fighter_bits, tie_fighter_width, tie_fighter_height
    , WhitePixelOfScreen(ows1.screen), 0
    , DefaultDepthOfScreen(ows1.screen) );

  if ( !starpixmap) starpixmap = XCreatePixmapFromBitmapData( dpy
    , DefaultRootWindow(dpy)
    , star_bits, star_width, star_height
    , WhitePixelOfScreen( ows1.screen), 0, DefaultDepthOfScreen(ows1.screen) );

  pb = XtVaCreateManagedWidget( "OK"
      , xmPushButtonGadgetClass, main_w
      , XmNleftAttachment,    XmATTACH_FORM
      , XmNbottomAttachment,     XmATTACH_FORM
      , NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_button1_CB
                 , XmDIALOG_OK_BUTTON);

  pb = XtVaCreateManagedWidget( "Reset"
       , xmPushButtonGadgetClass, main_w
       , XmNleftAttachment,       XmATTACH_WIDGET
       , XmNleftWidget,           pb
       , XmNbottomAttachment,     XmATTACH_FORM
       , NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_button1_CB
               , XmDIALOG_DEFAULT_BUTTON);

  bottomw =
  pb = XtVaCreateManagedWidget( "Cancel"
       , xmPushButtonGadgetClass, main_w
       , XmNleftAttachment,       XmATTACH_WIDGET
       , XmNleftWidget,           pb
       , XmNbottomAttachment,     XmATTACH_FORM
       , NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_button1_CB
               , XmDIALOG_CANCEL_BUTTON);

  lbltxt = XtVaCreateManagedWidget ("Mean Anomaly at Epoch, deg"
           , xmLabelWidgetClass,      main_w
           , XmNleftAttachment,       XmATTACH_FORM
           , XmNbottomAttachment,     XmATTACH_WIDGET
           , XmNbottomWidget,         bottomw
           , NULL);

  bottomw =
  m0_W = XtVaCreateManagedWidget ( "m0lbl"
         , xmTextFieldWidgetClass, main_w
         , XmNrightAttachment,    XmATTACH_FORM
         , XmNleftAttachment,     XmATTACH_WIDGET
         , XmNleftWidget,         lbltxt
         , XmNbottomAttachment,     XmATTACH_WIDGET
         , XmNbottomWidget,         bottomw
         , NULL);
  XtAddCallback (m0_W, XmNactivateCallback, orbitset_readval1, M0OFFS);
  m0val[1] = ows1.lclvals+2;
  m0val[0] = dblptrs[2];
  *m0val[1] = *m0val[0];
  orbitset_setval1( m0_W, *m0val[1] * DEGPR);

  lbltxt = XtVaCreateManagedWidget( "Argument of Periapse, deg"
           , xmLabelWidgetClass,      main_w
           , XmNleftAttachment,       XmATTACH_FORM
           , XmNbottomAttachment,     XmATTACH_WIDGET
           , XmNbottomWidget,         bottomw
           , NULL);

  bottomw =
  argp_W = XtVaCreateManagedWidget ( "argplbl"
           , xmTextFieldWidgetClass, main_w
           , XmNrightAttachment,    XmATTACH_FORM
           , XmNleftAttachment,     XmATTACH_WIDGET
           , XmNleftWidget,         lbltxt
           , XmNbottomAttachment,     XmATTACH_WIDGET
           , XmNbottomWidget,         bottomw
           , NULL);
  XtAddCallback ( argp_W, XmNactivateCallback, orbitset_readval1, ARGPOFFS);
  argpval[1] = ows1.lclvals+1;
  argpval[0] = dblptrs[1];
  *argpval[1] = *argpval[0];
  orbitset_setval1( argp_W, *argpval[1] * DEGPR);

  lbltxt = XtVaCreateManagedWidget( "Eccentricity, 0-1"
           , xmLabelWidgetClass,      main_w
           , XmNleftAttachment,       XmATTACH_FORM
           , XmNbottomAttachment,     XmATTACH_WIDGET
           , XmNbottomWidget,         bottomw
           , NULL);

  bottomw =
  ecc_W = XtVaCreateManagedWidget ( "ecclbl"
          , xmTextFieldWidgetClass, main_w
          , XmNrightAttachment,    XmATTACH_FORM
          , XmNleftAttachment,     XmATTACH_WIDGET
          , XmNleftWidget,         lbltxt
          , XmNbottomAttachment,     XmATTACH_WIDGET
          , XmNbottomWidget,         bottomw
          , NULL);
  eccval[1] = ows1.lclvals+0;
  eccval[0] = dblptrs[0];
  *eccval[1] = *eccval[0];
  orbitset_setval1( ecc_W, *eccval[1]);
  XtAddCallback ( ecc_W, XmNactivateCallback, orbitset_readval1, ECCOFFS);

  /* Create a square DrawingArea widget
   * - make the # of pixels odd so the center exists as a pixel
   */

  width1 = height1 = 601;
  drawing_a1 = XtVaCreateManagedWidget( "drawing_a1"
        , xmDrawingAreaWidgetClass, main_w
        , XmNtranslations, XtParseTranslationTable (translations)
        , XmNwidth,        width1
        , XmNheight,       height1
        , XmNresizePolicy, XmNONE  /* keep this a fixed size */

        , XmNbottomAttachment,          XmATTACH_WIDGET
        , XmNbottomWidget,              bottomw
        , XmNtopAttachment,       XmATTACH_FORM
        , XmNleftAttachment,         XmATTACH_FORM
        , XmNrightAttachment,        XmATTACH_FORM
        , NULL);

  ows1.da = drawing_a1;

  /* When scrolled, the drawing area will get expose events */
  XtAddCallback (drawing_a1, XmNexposeCallback, redraw1, drawing_a1);

  /* create a pixmap the same size as the drawing area. */
  ows1.pixmap = XCreatePixmap ( dpy
                , RootWindowOfScreen (ows1.screen), width1, height1
                , DefaultDepthOfScreen (ows1.screen));

  /* clear ows1.pixmap with white */
  XFillRectangle ( dpy, ows1.pixmap, gcW, 0, 0, width1, height1);

  makeorbitspots1a( &focusxpt);
  starxpt.x = focusxpt.x - star_x_hot;
  starxpt.y = focusxpt.y - star_y_hot;
  makeorbitrack1a( orbitplanxpt);
  makeorbitspots1b();

  XDrawLines( dpy, ows1.pixmap, gcB, orbitplanxpt, CIRCLERES, 0);

  XCopyArea( dpy, starpixmap, ows1.pixmap, gc1xor, STARXYWH
           , starxpt.x, starxpt.y);
  XCopyArea( dpy, scpixmap, ows1.pixmap, gc1xor, SCXYWH
           , scxpt.x, scxpt.y);

  /* copy to window */
  XCopyArea( dpy, ows1.pixmap, XtWindow(drawing_a1), gcB
           , 0, 0, width1, height1, 0, 0);

  XtManageChild(main_w);
  XtPopup(XtParent(main_w), XtGrabNone);
}

int
main(argc, argv)
int argc;
char *argv[];
{
Widget toplevel, pb, rc;
XtAppContext app;
XtActionsRec actions[4];

double orbitparms[5] = {  30.0      /*inclin*/
                       , 135.0      /*lnode*/
                       ,   0.7      /*ecc*/
                       ,  60.0      /*argp*/
                       , 270.0      /*m0*/
                       } ;
double *dblptrs[5];
char *lbls[5] = { "inclin"
               , "lnode"
               , "ecc"
               , "argp"
               , "m0"
               } ;
int i;

#define DEGTORAD(I) orbitparms[I] *= (atan(1.0) / 45.0)
#define RADTODEG(I) orbitparms[I] *= (45.0 / atan(1.0))

  for ( i=0; i<5; i++) fprintf( stderr
                              , "%8s:  %15.7lg\n", lbls[i], orbitparms[i]);
  DEGTORAD(0); /*inclin*/
  DEGTORAD(1); /*lnode*/
  DEGTORAD(3); /*argp*/ /*skip ecc*/
  DEGTORAD(4); /*m0*/

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize( &app, "Demos", NULL, 0
                                , &argc, argv, NULL, NULL);

    rc = XtVaCreateWidget( "rowcol", xmRowColumnWidgetClass, toplevel,
        NULL);

#define SETPTR(I,J) dblptrs[I] = orbitparms+J

  SETPTR(0,0);
  SETPTR(1,1);
  SETPTR(2,2);
  SETPTR(3,3);
  SETPTR(4,4);

  pb = XtVaCreateManagedWidget( "Push me 1"
                              , xmPushButtonWidgetClass, rc, NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset_CB, dblptrs);

  pb = XtVaCreateManagedWidget( "Push me 2"
                              , xmPushButtonWidgetClass, rc, NULL);
  XtAddCallback( pb, XmNactivateCallback, orbitset1_CB, dblptrs+2);

  XtManageChild( rc);

  /* Add the "draw" action/function used by the translation table
   * parsed by the translations resource below.
   */
  actions[0].string = "draw";
  actions[0].proc = draw;
  actions[1].string = "newsun";
  actions[1].proc = newsun;
  actions[2].string = "draw1";
  actions[2].proc = draw1;
  XtAppAddActions( app, actions, 3);

  XtRealizeWidget( toplevel);
  XtAppMainLoop( app);

  RADTODEG(0); /*inclin*/
  RADTODEG(1); /*lnode*/
  RADTODEG(3); /*argp*/ /*skip ecc*/
  RADTODEG(4); /*m0*/
  for ( i=0; i<5; i++) fprintf( stderr
                              , "%8s:  %15.7lg\n", lbls[i], orbitparms[i]);

  return(0);
}

#ifdef CTOFORT_
int
__main() { 
int argc = 1;
char *argvv = { "orbitsetter" };
char *argv[] = { argvv, 0 };
  main(); 
}
#endif
