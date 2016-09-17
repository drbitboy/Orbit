#include <string.h>
#include <stdio.h>
#include <math.h>
#include "iepcmn_par.h"
#include "orbitfort.h"

#define iepcmnsetall iepcmnsetall_
#define ioffn ioffn_
#define testnan testnan_

#define RPD (M_PI / 180.0)

static double iepdbl[IEPDBLCOUNT];
static fortint iepint[IEPINTCOUNT];

static char iepsfn[512], ieplcf[512], iepc12[1024];

fortint lensfn, lenlcf, lentot;

/*************************/
long
testnan( double *testhis) {
  return (long) finite( *testhis);
}

/*****************************************/
void
orbitlcrvUpdateStr( char *sfn, char *lcf) {
  if ( sfn != iepsfn) {
    strcpy( iepsfn, sfn);
    lensfn = strlen( iepsfn);
  }
  if ( lcf != ieplcf) {
    strcpy( ieplcf, lcf);
    lenlcf = strlen( ieplcf);
  }

  strcpy( iepc12, iepsfn);
  strcpy( iepc12+lensfn, ieplcf);
  lentot = lensfn + lenlcf;
  return;
}

#define USAGE \
  fprintf( stderr, "USAGE:\n  %s <iepfile>\n", argv[0]); fflush( stderr)

#define FAILURE_FPRINTF(A) fprintf A; USAGE; return 0
  
#define FAILURE(STR) \
  fprintf( stderr, "%s\n", STR); \
  USAGE; \
  if ( !fiep) fclose( fiep); \
  return 0

/****************************/
int
main( int argc, char **argv) {
fortint pass;
double dninp;
FILE *fiep;
#define INSIZE 1024
char inpLine[INSIZE];
double saveEt;

double ioffn(fortint *, double *);
void iepcmnsetall( double *, fortint *, fortint *, fortint *, char *);

  if ( argc != 2) { FAILURE(""); }

  if ( !(fiep = fopen( argv[1], "r")) ) {
    FAILURE_FPRINTF( (stderr, "Can't open file %s\n", argv[1]));
  }

  iepdbl[ IEP_l] =
  iepdbl[ IEP_s] =
  iepdbl[ IEP_lin] =
  iepdbl[ IEP_smp] =
  iepdbl[ IEP_cla] =
  iepdbl[ IEP_clo] =
  iepdbl[ IEP_ola] =
  iepdbl[ IEP_olo] =
  iepdbl[ IEP_rng] =
  iepdbl[ IEP_nor] =
  iepdbl[ IEP_rpp] =
  iepdbl[ IEP_inc] =
  iepdbl[ IEP_emi] =
  iepdbl[ IEP_pha] =
  iepdbl[ IEP_et] =
  iepdbl[ IEP_met] = 
  0.0;

  iepint[ IEP_pic] =
  iepint[ IEP_cam] = 
  0;

  orbitlcrvUpdateStr( "orbitlcrv_fn.inp", "orbitlcrv.dat");


  /* loop through each line in IEP file
   * - each line is one plate's contribution to a single datum of the lightcurve
   * - each datum has multiple plates with the same time
   * - when the time changes, it is time to start a new datum
   * - one IEP file represents a single lightcurve
   */

  pass = 99;         /* init pass to something other than 2 (see logic below) */
  saveEt = -2e301;  /* init time to impossible value so we know to do header, */
                /* lightcurve, input file & datum initialization (99 then 98) */
                  

  while ( fgets( inpLine, INSIZE, fiep) ) {                  /* read one line */
    /* parse it */
    if ( 11 == sscanf( inpLine
                     , "%lf %lf% %lf %lf% %lf %lf %lf% %lf %lf% %lf %lf"
                     , iepdbl+IEP_rpp
                     , iepdbl+IEP_rng
                     , iepdbl+IEP_inc
                     , iepdbl+IEP_emi
                     , iepdbl+IEP_pha
                     , iepdbl+IEP_cla
                     , iepdbl+IEP_clo
                     , iepdbl+IEP_ola
                     , iepdbl+IEP_olo
                     , iepdbl+IEP_met
                     , iepdbl+IEP_et)) {

      iepdbl[IEP_inc] *= RPD;
      iepdbl[IEP_emi] *= RPD;
      iepdbl[IEP_pha] *= RPD;

      iepint[IEP_pic] = iepdbl[IEP_clo]*100;  /* use s/c lon (cDeg) for picno */

      if (iepdbl[IEP_et] != saveEt) {      /* next datum (time) in lightcurve */
        if ( pass == 2) {
          pass = 3;
          ioffn( &pass, &dninp);             /* finish off current lightcurve */
          pass = 97;                 /* init 2nd or later datum, skip headers */
        } else {
          if (saveEt < -1e300) {
            pass = 99;     /* create input file for all future data (98 & 97) */
            iepcmnsetall( iepdbl, iepint, &lensfn, &lentot, iepc12);
            ioffn( &pass, &dninp);

            pass = 98;   /* init 1st datum, print headers per input file (99) */
          } else {
            pass = 97;                           /* this should never execute */
          }
        }
        saveEt = iepdbl[IEP_et];
        iepcmnsetall( iepdbl, iepint, &lensfn, &lentot, iepc12);

fprintf( stderr, "."); fflush( stderr);

        ioffn( &pass, &dninp);                   /* init 98 (1st datum) or 97 */

      } else {

        /* if time doesn't change, copy i,e,p,area,range for pass 2 below */

        iepcmnsetall( iepdbl, iepint, &lensfn, &lentot, iepc12);

      } /* if (iepdbl[IEP_et] != saveEt) ... else ... */

      pass = 2;    /* datum initialization is done, start summing plate i/f's */
      if ( (iepdbl[IEP_inc] < (M_PI/2)) && (iepdbl[IEP_emi] < (M_PI/2)) ) {
        ioffn( &pass, &dninp);
      }
    } /* if 10 == sscanf */
  } /* while fgets */

fprintf( stderr, "\n"); fflush( stderr);

  if ( pass == 2) {
    pass = 3;
    ioffn( &pass, &dninp);                      /* finish off last lightcurve */
  }

  return 0;
}
