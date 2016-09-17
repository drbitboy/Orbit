#ifndef _ORBIT_STATS_H_                                      /* orbit_stats.h */
#define _ORBIT_STATS_H_

#include <stdio.h>

#define FRMNULLVAL -1.0

/*************/
/* -<option> */

/******************************/
/* argument to int conversion */

typedef struct {
  char *_name;
  int _type;
  char *_help;
} OPTION;

/********************/
/* -stat <statType> */

enum { STAT_INCID=0
     , STAT_MU0
     , STAT_EMISS
     , STAT_MU
     , STAT_MU0MU       /* (Mu0 * Mu) */
     , STAT_PHASE
     , STAT_COSPHASE
     , STAT_RESOLUTION  /* best resolution:  1 / (cos(inc)*km/pixel) ~ pxl/km */
     , STAT_COVERAGE    /* # of times covered */
     , STAT_MRMORPH     /* from Mark Robinson:  */
     , STAT_MRALBEDO    /* from Mark Robinson:  */
     /*
      * If BACKCHK=MORPH, the lower value of the equation:
      * resolution/cos(emission angle)  will be kept.
      * If BACKCHK=ALBEDO, the lower value of the equation:
      * resolution*[1/cos(emission angle)+1/cos(incidence angle)]
      * will be kept.
      */
     , STAT_OBSPARM     /* observational parameter */
     , STAT_COUNT       /* last STAT_ type - not used other than as count */
     };

static OPTION stats[] = {
   "incid", STAT_INCID, "incidence angle"
 , "mu0", STAT_MU0, "cos(incid)"
 , "emiss", STAT_EMISS, "emission angle"
 , "mu", STAT_MU, "cos(emission)"
 , "mu0mu", STAT_MU0MU, "mu0 x mu"
 , "phase", STAT_PHASE, "phase angle"
 , "cosphase", STAT_COSPHASE, "1+cos(phase)"
 , "resolution", STAT_RESOLUTION, "mu / (Pxl x Range), pxl/km"
 , "coverage", STAT_COVERAGE, "# FOV containing this image"
 , "mrmorph", STAT_MRMORPH, "(Pxl x Range) * (1/mu), m/pxl"
 , "mralbedo", STAT_MRALBEDO, "(Pxl x Range) * (1/mu+1/mu0), m/pxl"
 , "obs", STAT_OBSPARM, "obs param from frames file"
 , (char *) 0, STAT_COUNT, ""
};

/* which is best (i.e. which to save, max or min) for each statistic
 * when that statistic controls
 */
static OPTION statsMinMax[] = {
   "min", STAT_INCID, "min incid"
 , "max", STAT_MU0, "max mu0"
 , "min", STAT_EMISS, "min emiss"
 , "max", STAT_MU, "max mu"
 , "max", STAT_MU0MU, "max (mu x mu0)"
 , "min", STAT_PHASE, "min phase"
 , "max", STAT_COSPHASE, "max cosphase"
 , "max", STAT_RESOLUTION, "max resolution"    /* pxl/km */
 , "max", STAT_COVERAGE, "max coverage"
 , "min", STAT_MRMORPH, "min mrmorph"       /* km/pxl */
 , "min", STAT_MRALBEDO, "min mralbedo"      /* km/pxl */
 , "avg", STAT_OBSPARM, "avg parm"
 , (char *) 0, STAT_COUNT, ""
};

/****************************************************************************/
/* frm2_getArgInt():  get an integer selection from a command line argument */

static int
frm2_getArgInt( OPTION *opts, const char *arg) {
  while ( opts->_name) {/* loop through opts until ->_name matches or is null */
    if ( !strcmp( arg, opts->_name)) return opts->_type;     /* return match */
    ++opts;                                                       /* next opt */
  }
  return -1;           /* return error if no match found i.e. ->_name is null */
}

/****************************************************************************/
/* frm2_outOptHelp():  output help info for options                         */

static void
frm2_outOptHelp( OPTION *opts, FILE *stdWhat) {
  while ( opts->_name) {/* loop through opts until ->_name is null */
    fprintf( stdWhat, " -%s:  %s\n", opts->_name, opts->_help);
    ++opts;                                                       /* next opt */
  }
  return;
}

#define TYPECHK( TYPE, OPTS, STR) TYPECHKRTN( TYPE, OPTS, STR, -1)

#define TYPECHKRTN( TYPE, OPTS, STR, RTN) \
  if ( -1 == (TYPE=frm2_getArgInt( OPTS, STR))) { \
    fprintf( stderr, "BAD VALUE FOR OPTION (=%s); exiting\n", STR); \
    fflush( stderr); \
    return (RTN); \
  }

int orbit_readStatFile( FILE *fInp, long *nFacePtr
                      , double **statValsPtr);
int orbit_readStatFileName( char *filename, long *nFacePtr
                          , double **statValsPtr);

void orbit_writeStatFile( FILE *f, long nFace, int ctlStat, double *statVals);
void orbit_writeStatFileName( char *filename, long nFace
                            , int ctlStat, double *statVals);

#endif /* _ORBIT_STATS_H_ */                             /* end orbit_stats.h */
