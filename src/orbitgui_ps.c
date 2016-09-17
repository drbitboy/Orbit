#include <string.h>
#include "orbitgui_hc.h"
#include "rainbow.h"

#include "debug.h"
#include "local.h"

#define PAGEW (8.5*72)  /* points across */
#define PAGEH (11.0*72) /* points top to bottom */
#define PAGEM 18 /* 1/4" margin */
#define PRINTW ((double) (PAGEW - (2*PAGEM)))
#define PRINTH ((double) (PAGEH - (2*PAGEM)))

static int numRainbowColors;

FILE *
PSOpenPage( char *filename, int width, int height) {
FILE *psfile = (FILE *) 0;
double scale1, scale2, scale;
double offx, offy;
unsigned short *usp, i;

  if ( !filename || width < 1 || height < 1) return( psfile);
  psfile = fopen( filename, "w");
  if ( !psfile) return( psfile);

fprintf( psfile, 
"%%!PS-Adobe-2.0\n\
/$OrbitDict 200 dict def \n\
$OrbitDict begin\n\
/l {lineto} bind def\n\
/m {moveto} bind def\n\
/s {stroke} bind def\n\
/n {newpath} bind def\n\
/fcol {dup dup currentrgbcolor 4 -2 roll mul 4 -2 roll mul\n\
4 -2 roll mul setrgbcolor} bind def\n\
/cp {closepath} bind def\n\
/gs {gsave} bind def\n\
/gr {grestore} bind def\n\
/col-1 {} def\n");

  for ( i=0; i<8; ++i) 
    fprintf( psfile, "/col%d {%d %d %d setrgbcolor} bind def\n"
                   , i, (i&4)>>2, (i&2)>>1, i&1);

  for ( i=0, usp=rainbowRGB; *usp < RB_RGBTERM; ++i) {
  int i3;
    fprintf( psfile, "/col%d {", RB_RAINBOWOFFSET+i);
    for ( i3=0; i3<3; ++i3) fprintf( psfile, "%lf ", (double) *(usp++) / 255.0);
    fprintf( psfile, "setrgbcolor} bind def \n");
  }
  numRainbowColors = i;

  for ( i=0; i<RB_NUMGRAYS; ++i ) {
  double d3;
    d3 = i / (RB_NUMGRAYS-1.0);
    fprintf( psfile, "/col%d {%lf %lf %lf setrgbcolor} bind def \n"
                   , RB_GRAYOFFSET+i, d3, d3, d3);
  }

fprintf( psfile,
" end\n\
/$OrbitBeg {$OrbitDict begin /$OrbitState save def} def\n\
/$OrbitEnd {$OrbitState restore end} def\n\
\n\
$OrbitBeg\n\
0 setlinecap 0 setlinejoin\n"
);

  scale1 = PRINTW / (double) width;
  scale2 = PRINTH / (double) height;

  scale = (scale1<scale2) ? scale1 : scale2;

  offx = PAGEM + 0.5 * ( PRINTW - (scale * width));
  offy = (-PAGEM) + 0.5 * ( PRINTH + (scale * height));

  fprintf( psfile
         , "%.1lf %.1lf translate %.3lf -%.3lf scale\n0.500 setlinewidth\n"
         , offx, offy, scale, scale);


  return( psfile);
}

void
PSClosePage( FILE *psfile) {
/* DPR1( "PSClosePage //"); /**/
  if ( !psfile) return;
  fprintf( psfile, "showpage\n$OrbitEnd\n");
  fclose( psfile);
  return;
}

/***********************************/
/* set input color to proper range */

#define PSFIX( VAL, BASE, MODULO) \
  ( ((VAL) < ((BASE)+(MODULO))) ? (VAL) : ((BASE)+(((VAL)-(BASE)) % (MODULO))) )

int PSFixColor( int pscolor) {
static int lastIn;   /* initializes to 0 */
static int lastOut;  /*  " */

  if ( lastIn == pscolor) return lastOut;  /* use most recent buffer, if poss */

  lastIn = pscolor;

  if ( RB_RAINBOWOFFSET > RB_GRAYOFFSET) {
    if ( pscolor >= RB_RAINBOWOFFSET) {                     /* rainbow colors */
      lastOut = PSFIX( pscolor, RB_RAINBOWOFFSET, numRainbowColors);
    } else if ( pscolor >= RB_GRAYOFFSET) {                          /* grays */
      lastOut = PSFIX( pscolor, RB_GRAYOFFSET, RB_NUMGRAYS);
    } else {                                                     /* primaries */
      lastOut = (pscolor&PSWHITE)^PSWHITE;        /* XOR for white background */
    }
  } else {
    if ( pscolor >= RB_GRAYOFFSET) {                                 /* grays */
      lastOut = PSFIX( pscolor, RB_GRAYOFFSET, RB_NUMGRAYS);
    } else if ( pscolor >= RB_RAINBOWOFFSET) {              /* rainbow colors */
      lastOut = PSFIX( pscolor, RB_RAINBOWOFFSET, numRainbowColors);
    } else {                                                     /* primaries */
      lastOut = (pscolor&PSWHITE)^PSWHITE;        /* XOR for white background */
    }
  }
  return lastOut;
}

#define X1 seg[iseg].x1
#define X2 seg[iseg].x2
#define Y1 seg[iseg].y1
#define Y2 seg[iseg].y2

void PSDrawSegments( FILE *psfile, int pscolor, XSegment *seg, int nseg) {
int iseg;

  if ( !psfile) return;

  for ( iseg=0; iseg<nseg; iseg++) {
    fprintf( psfile, "n %d %d m %d %d l gs col%d s gr\n"
           , X1, Y1, X2, Y2, PSFixColor( pscolor));
  }

  return;
}

void PSFillPoly( FILE *psfile, int pscolor, XPoint *xpt, int numpt) {
int i;
  if ( !psfile) return;
  if ( numpt < 3) return;
  pscolor = PSFixColor( pscolor);

  fprintf( psfile, "n %d %d m ", xpt[0].x, xpt[0].y);
  for ( i=1; i<numpt; ++i)
    fprintf( psfile, "%d %d l ", xpt[i].x, xpt[i].y);
  fprintf( psfile, "cp gs col%d \n 1.00 fcol fill gr gs col%d s gr\n"
         , pscolor, pscolor);

  return;
}

void PSFillRectangle( FILE *psfile, int pscolor, int x, int y, int dx, int dy)
{
XPoint xpt[4];

  xpt[0].x = x;       xpt[0].y = y;
  xpt[1].x = x + dx;  xpt[1].y = y;
  xpt[2].x = x + dx;  xpt[2].y = y + dy;
  xpt[3].x = x;       xpt[3].y = y + dy;

  PSFillPoly( psfile, pscolor, xpt, (int) 4);

/*  fprintf( psfile
         , "n %d %d m %d %d l %d %d l %d %d l cp gs col%d %s col%d s gr\n"
         , x, y, x+dx, y, x+dx, y+dy, x, y+dy
         , pscolor, "\n 1.00 fcol fill gr gs", pscolor); */

  return;
}

void PSDrawString( FILE *psfile, int pscolor, int x, int y, char *outstr) {
int psc = PSFixColor( pscolor);
char *badchar;

  if ( !psfile) return;

  /* look for characters that need to be escaped */

  badchar = strchr( outstr, '\\');
  if ( !badchar) badchar = strchr( outstr, '(');
  if ( !badchar) badchar = strchr( outstr, ')');

  if ( !badchar) {           /* print string as is if no bad characters found */
    fprintf( psfile,
"/Times-Roman findfont 12.00 scalefont setfont\n\
%d %d m\n\
gs 1 -1 scale (%s) col%d show gr\n"
         , x, y, outstr, psc);

  } else {                                          /* escape bad characters */
  char *c;
    fprintf( psfile,
      "/Times-Roman findfont 12.00 scalefont setfont\n%d %d m\ngs 1 -1 scale ("
      , x, y);
    for ( c=outstr; *c; ++c) {
      switch ( *c) {
      case '\\':
      case '(':
      case ')':
        fprintf( psfile, "\\%c", *c);
        break;
      default:
        fprintf( psfile, "%c", *c);
        break;
      }
    }
    fprintf( psfile, ") col%d show gr\n", psc);
  }
  return;
}
