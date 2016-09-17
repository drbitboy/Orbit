#include <stdio.h>
#include <string.h>
#include <math.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>


#define deg		*3.14159265358979323846/180.
#define min(a,b)	((a) < (b) ? (a) : (b))
#define max(a,b)	((a) > (b) ? (a) : (b))

void display_3d(Display *);

char **orbit_argv;
int orbit_argc;

int
main( int argc, char **argv) {
Display *dpy;
XtAppContext app_context;
Widget sub_window;

  orbit_argv = argv;
  orbit_argc = argc;

  sub_window = XtAppInitialize(&app_context, "Display3d", NULL, 0
				, &argc, argv, NULL, NULL, 0);
  dpy = XtDisplay(sub_window);

  display_3d( dpy);
  XtAppMainLoop( app_context);
}

/**********************************************************/
