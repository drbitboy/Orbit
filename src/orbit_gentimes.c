/* orbit_gentimes.c */
/* Modifications
 *
 * btcarcich 11.may'2000
 * - 0 allowed as columns for instrument & mirror position (mp)
 *   - will use current instrument &/or current mp
 * - don't exit loop on failure of pointing_solve
 * - progress meter (+/- every 100 pointing_solve successes/failures)
 */

#include <stdio.h>
#include <malloc.h>

#ifndef MAX
#define MAX(A,B) ( ((A)>(B)) ? (A) : (B) )
#endif

#include "orbit3d.h"
#include "pointing.h"
#include "debug.h"
#include "orbit_gentimes.h"
#include "orbit_spice_names.h"

#define gtFN gentime->_filnam
#define gtTFmt gentime->_timeFormat
#define gtTCols gentime->_timeColumns
#define gtICol gentime->_instrColumn
#define gtMPCol gentime->_mirrorPosnColumn
#define gtHdrLin gentime->_headerLines

#define MAXLENLINE 256
#define MAXNUMTOK 16

/* orbit_gentimes_act - generate frames from info in file
 * - return 0 if any new frames are generated, otherwise return 1
 */
#define RTNERR { if ( gtfile) fclose( gtfile); return 1; }
#define RTNOK return 0

int
orbit_gentimes_act( GENTIME *gentime)
{
FILE *gtfile = (FILE *) 0;
char inpline[MAXLENLINE];
char coltxt[MAXNUMTOK][MAXLENLINE];
char fmt[MAXLENLINE];
long i, j, maxcol, instr, imp, tfmt;
long itcol0, itcol1, iicol, impcol;
unsigned long failCount, succCount;
IMGFRM *imgfrm1, *imgfrmnew;
POINTING *lclBore, *lclRoll;

  /* test for bad inputs */

  /*if ( gtTCols[0] < 1 || gtTCols[1] < gtTCols[0] || gtICol < 1 || gtMPCol <0*/
  /*
   * allow 0 in instrument & mp column 11.may'2000
   */
  if ( gtTCols[0] < 1 || gtTCols[1] < gtTCols[0] || gtICol < 0 || gtMPCol < 0
    || gtHdrLin < 0) RTNERR

  maxcol = MAX( gtTCols[0], gtTCols[1]);
  maxcol = MAX( maxcol, gtICol);
  maxcol = MAX( maxcol, gtMPCol);
  if ( maxcol > MAXNUMTOK) RTNERR
  if ( maxcol < ((gtMPCol>0) ? 3 : ((gtICol>0) ? 2 : 1)) ) RTNERR

  for ( tfmt=GTFMT_KMET
      ; (1L<<tfmt) != (gtTFmt&GTFMT_ALLBITS) && tfmt <= GTFMT_LAST; ++tfmt) ;
  if ( tfmt > GTFMT_LAST) RTNERR

  /* open file, test for error */
  gtfile = fopen( gtFN, "r");
  if ( !gtfile) RTNERR

  /* read any header lines */
  for ( i=gtHdrLin; i>0 && fgets( inpline, MAXLENLINE, gtfile); --i) ;
  if ( i) RTNERR

  /* make the column references zero-based */
  itcol0 = gtTCols[0]-1;
  itcol1 = gtTCols[1]-1;
  iicol = gtICol-1;
  impcol = gtMPCol-1;

  /* init image frame structures */
  imgfrm1 = (IMGFRM *) 0;

  /* get bore and roll pointing structures for orbitgui.c */                    
  orbitgui_return_boreroll( gentime->_sc, &lclBore, &lclRoll);
                                                                                
  failCount = succCount = 0;                       /* for +/- progress meters */

# define FAILCONTINUE \
  if ( !(++failCount & FAILBITS)) { fprintf( stderr, "-"); fflush( stderr); }
# define SUCCEED \
  if ( !(++succCount & SUCCBITS)) { fprintf( stderr, "+"); fflush( stderr); }
# define FAILBITS (0xfL)   /* 0000 1111 => every 16 */
# define SUCCBITS (0x7fL)  /* 0111 1111 => every 128 */

  while ( fgets( inpline, MAXLENLINE, gtfile)) {
  int ntok;
  double et, sclk;
    i = 0;
    /* read up to 16 whitespace-delimited tokens */
    ntok = sscanf( inpline, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s"
                 , coltxt[0] , coltxt[1] , coltxt[2] , coltxt[3] 
                 , coltxt[4] , coltxt[5] , coltxt[6] , coltxt[7] 
                 , coltxt[8] , coltxt[9] , coltxt[10] , coltxt[11] 
                 , coltxt[12] , coltxt[13] , coltxt[14] , coltxt[15] );

    /* if enough tokens were read, translate them
     * - continue with each line as long as ntok >= maxcol
     * - set ntok to -1 on error
     */

    /* - time - combine multiple whitespace-separated columns & convert to ET */

    if ( ntok > itcol1) {
      for ( i=1; (itcol0 + i) <= itcol1; ++i) {
        strcat( coltxt[itcol0], " ");
        strcat( coltxt[itcol0], coltxt[itcol0 + i]);
      }

      /* - convert field to double ET - s past J2000 */
      switch ( tfmt) {
      case GTFMT_KMET:
        if ( sscanf( coltxt[itcol0], "%lf", &sclk) != 1) {
          ntok = -1;
          break;
        }
        sclk *= 1000;
        sct2e( &gentime->_sc->_scid, &sclk, &et);
        break;
      case GTFMT_INTMET:
        if ( sscanf( coltxt[itcol0], "%lf", &sclk) != 1) {
          ntok = -1;
          break;
        }
        sct2e( &gentime->_sc->_scid, &sclk, &et);
        break;
      case GTFMT_SCLK:
        orbit_sclkch2et( gentime->_sc->_scid, coltxt[itcol0], &et);
        break;
      case GTFMT_DPET:
        if ( sscanf( coltxt[itcol0], "%lf", &et) != 1) ntok = -1;
        break;
      case GTFMT_UTC:
        orbit_utc2et( coltxt[itcol0], &et);
        break;
      default:  /* just in case */
        if ( failCount>FAILBITS || succCount>SUCCBITS) fprintf( stderr, "\n");
        fprintf( stderr, "orbit_gentimes:  Error, contact Programmer, code %s\n"
               , "WSNBATGH-0");
        fflush( stderr);
        RTNERR;
        break;
      } /* switch tfmt */
    } else ntok = -1;    /* time - if ntok > itcol1 */

    /* - instrument - convert to lower case and set instr */

    if ( iicol < 0)             /* use current values if no instrument column */
      instr = gentime->_sc->_instrument;
    else {
      if ( ntok > iicol) {
        for ( i=strlen( coltxt[iicol])-1; i>=0; --i) 
          coltxt[iicol][i] = tolower( coltxt[iicol][i]);
        if ( !strcmp( coltxt[iicol], "nis")) instr = SC_NIS;
        else if ( !strcmp( coltxt[iicol], "niswide")) instr = SC_NIS2;
        else if ( !strcmp( coltxt[iicol], "nis2")) instr = SC_NIS2;
        else if ( !strcmp( coltxt[iicol], "msi")) instr = SC_MSI;
        else ntok = -1;
      } else ntok = -1;    /* instrument - if ntok >= maxcol */
    }


    /* - mirror position - only if instrument is NIS (wide or narrow) */

    if ( iicol < 0 || impcol < 0 ) {/* use current mp if no instr or mp column*/
      imp = gentime->_sc->_nisStepAct;
    } else {
      switch ( instr) {
      case SC_NIS:
      case SC_NIS2:
        if ( ntok > impcol) {
          if ( sscanf( coltxt[impcol], "%ld", &imp) != 1) ntok = -1;
          if ( imp < 0 || imp > 350) ntok = -1;
        } else ntok = -1;
        break;
      default:
        imp = gentime->_sc->_nisStepAct;      /* save mp so it doesn't change */
        break;
      } /* switch instr */
    }

    /* finally, generate the frame */
    if ( ntok > 0) {
    char *instrName;

      /* load instrument & mp info IF THEIR COLUMNS WERE SPECIFIED */
      if ( iicol >= 0) {
         if ( impcol >= 0) gentime->_sc->_nisStepAct = imp;
         instrName = orbit_set_instrument( gentime->_sc, instr);
      }

      /* solve for pointing & generate the frame */

      if ( !pointing_solve( (char *) 0, &et, gentime->_sc, lclBore, lclRoll)) {
        FAILCONTINUE
      }
      imgfrmnew = loadImageFrame( gentime->_sc, et, (IMGFRM *) 0);

      if ( imgfrmnew){
        /* copy instrument info to IMGFRM struct, add to or build linked list */
        copyInstrument_ScToImgfrm( gentime->_sc, imgfrmnew);
        imgfrmnew->_nisDuration = 0;
        imgfrmnew->_nisSpecnum = 0;
        imgfrmnew->_nisScannum = 0;
        imgfrmnew->_nisDarkFollows = 0;
        if ( imgfrm1) {
          imgfrm1->previf->nextif = imgfrmnew;
          imgfrmnew->previf = imgfrm1->previf;
        } else {
          imgfrm1 = imgfrmnew;
        }
        imgfrm1->previf = imgfrmnew;
      } else { /* if imgfrmnew */
        FAILCONTINUE
      } /* if imgfrmnew else */
    } else { /* if ntok > 0 */
      FAILCONTINUE
    }
    SUCCEED
  } /* while fgets */
  fclose( gtfile);
  if ( failCount >= FAILBITS || succCount >= SUCCBITS) fprintf( stderr, "\n");

  /* add frames to orbit structure */
  if ( imgfrm1) add_gen_frames( gentime->_sc->_scOrbit, imgfrm1);
  RTNOK;
}
