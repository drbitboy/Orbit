#include <string.h>
#include "orbitgui_hc.h"

#include "debug.h"
#include "local.h"

#define PAGEW (8.5*72) /* points across */
#define PAGEH (11*72) /* points top to bottom */
#define PAGEM 18 /* 1/4" margin */
#define PRINTW (PAGEW - (2*PAGEM))
#define PRINTH (PAGEH - (2*PAGEM))

FILE *
XFIGOpenPage( char *filename, int width, int height) {
FILE *hcfile = (FILE *) 0;
double scale1, scale2, scale;
double offx, offy;

  if ( !filename || width < 1 || height < 1) return( hcfile);
  hcfile = fopen( filename, "w");
  if ( !hcfile) return( hcfile);

  fprintf( hcfile, "#FIG 2.1\n80 2\n");

  return( hcfile);
}

void
XFIGClosePage( FILE *hcfile) {
  /* DPR1( "XFIGClosePage //"); /**/
  if ( !hcfile) return;
  fclose( hcfile);
  return;
}

#define X1 seg[iseg].x1
#define X2 seg[iseg].x2
#define Y1 seg[iseg].y1
#define Y2 seg[iseg].y2
#define INVERTCOL \
  ((pscolor<=PSWHITE && pscolor>=PSBLACK) ? (pscolor^PSWHITE) : pscolor)

void XFIGDrawSegments( FILE *hcfile, int pscolor, XSegment *seg, int nseg) {
int iseg;

  if ( !hcfile) return;

  for ( iseg=0; iseg<nseg; iseg++) {
    fprintf( hcfile, "2 1 0 1 %d 0 0 0 0.000 -1 0 0\n\t  %d %d %d %d %d %d\n"
           , INVERTCOL, X1, Y1, X2, Y2, 9999, 9999);
  }

  return;
}

void
XFIGFillRectangle( FILE *hcfile, int pscolor, int x, int y, int dx, int dy)
{

  if ( !hcfile) return;

  fprintf( hcfile
         , "2 1 0 1 %d 0 0 21 0.000 0 0 0\n\t %d %d %d %d %d %d %d %d %d %d\n"
         , INVERTCOL, x+dx, y+dy, x+dx, y, x, y, x, y+dy, 9999, 9999);

  return;
}

void
XFIGDrawString( FILE *hcfile, int pscolor, int x, int y, char *outstr) {

  if ( !hcfile) return;

  /* 12 point Helvetica Font */
#define FONTSIZE 12
#define HELVETICA 16
  fprintf( hcfile
         , "4 0 %d %d 0 %d 1 0.00000 4 %d %ld %d %d %s%c\n"
         , HELVETICA, FONTSIZE
         , INVERTCOL, FONTSIZE+1, strlen(outstr) * ((FONTSIZE*2/3)-1)
         , x, y, outstr, (char) 1);

  return;
}
