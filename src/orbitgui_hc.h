/* orbitgui_hc.h */

#ifndef _ORBITGUI_HC_H_
#define _ORBITGUI_HC_H_

#include <stdio.h>
#include <X11/Xlib.h>

FILE * PSOpenPage( char *filename, int width, int height);
void PSClosePage( FILE *hcfile);
void PSDrawSegments( FILE *hcfile, int hccolor, XSegment *seg, int nseg);
void PSFillRectangle( FILE *hcfile, int hccolor, int x, int y, int dx, int dy);
void PSFillPoly( FILE *hcfile, int hccolor, XPoint *xpt, int numpt);
void PSDrawString( FILE *hcfile, int hccolor, int x, int y, char *outstr);

FILE * XFIGOpenPage( char *filename, int width, int height);
void XFIGClosePage( FILE *hcfile);
void XFIGDrawSegments( FILE *hcfile, int hccolor, XSegment *seg, int nseg);
void XFIGFillRectangle(FILE *hcfile, int hccolor, int x, int y, int dx, int dy);
void XFIGFillPoly( FILE *hcfile, int hccolor, XPoint *xpt, int i);
void XFIGDrawString( FILE *hcfile, int hccolor, int x, int y, char *outstr);

/* simple colors - primary and secondary only */

#define PSBLACK 0
#define PSBLUE 1
#define PSGREEN 2
#define PSCYAN 3
#define PSRED 4
#define PSMAGENTA 5
#define PSYELLOW 6
#define PSWHITE 7
#define PSLASTCOLOR PSWHITE   /* hook for additional colors */

#endif
/* end orbitgui_hc.h */
