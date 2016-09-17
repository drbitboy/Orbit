/* spudplates.c - read in APL plate model
- three types, scanstat0 gives number of fields on first line:

1)  Original (was official) model format 
    - scanstat0 == 1
    - NVERT & NPLATE precede vertices & plates, resp.
    - ***N.B. NPLATE line is optional
    - ***N.B. indices precede data on vertex data & plate data lines

 ************************************************
NVERT ...
1 X1 Y1 Z1
2 X2 Y2 Z2
3 X3 Y3 Z3
...
NVERT XNVERT YNVERT ZNVERT ...
NPLATE
1 V11 V12 V13 ...
2 V21 V22 V23 ...
3 V31 V32 V33 ...
...
NPLATE VNPLATE1 VNPLATE2 VNPLATE3 ...
 ************************************************


2)  Official format (Yanping Guo)
    - scanstat0 == 2

 ************************************************
NVERT NPLATE
X0 Y0 Z0
...
X(NVERT-1) Y(NVERT-1) Z(NVERT-1)
V00 V01 V02
...
V(NPLATE-1)0 V(NPLATE-1)1 V(NPLATE-1)2
 ************************************************


3)  same as 2) without initial "NVERT NPLATE" line
    - scanstat0 == 3
    - to interpret this format, count lines:
      - assume NPLATE=2*(NVERT-2):
        LINECOUNT=(NPLATE+NVERT) = (3*NVERT) - 4
        NVERT = (LINECOUNT + 4) / 3
        0 == ((LINECOUNT + 4) modulo 3)
        2 == (LINECOUNT modulo 3)

 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "orbit3d.h"
#include "spudshap.h"

#define ENVNAME "PLATES"

# define LSWAP(A,B) tmplng=lptr[A]; lptr[A]=lptr[B]; lptr[B]=tmplng

#ifdef VMS
#define FNAME1 "plates.dat"
#define FNAME2 FNAME1
#else
#define FNAME1 ENVNAME
#define FNAME2 "plates"
#endif

static char fname1[] = { FNAME1 };
static char fname2[] = { FNAME2 };


typedef struct {
  double _sinlat;
  unsigned long _oldIdx;
} SORTVERTSTRUCT;

/*************************************************/
int
qsort_vertStructByLat( const void *v1, const void *v2) {
SORTVERTSTRUCT *ss1 = (SORTVERTSTRUCT *) v1;
SORTVERTSTRUCT *ss2 = (SORTVERTSTRUCT *) v2;
  return (ss1->_sinlat<ss2->_sinlat) ? -1 : (ss1->_sinlat>ss2->_sinlat) ? 1 : 0;
}

/*********************************************************************/
/* renumber plate model's vertices by increasing latitude so orbit's plate 
 * definition scheme is more likely to work
 * nv    - number of vertices (number of plates = np = 2*nv - 4
 * Rxyz  - [nv][3] vx vx vz for each vertex
 * verts - [np][4] plate indices + plate number
 */
void spud_RenumPlateVertices( unsigned long nv, double *Rxyz
                            , unsigned long *verts) {
unsigned long np = (2 * nv) - 4;               /* number of triangular plates */
unsigned long *old2new = (unsigned long *)malloc(sizeof(unsigned long) * nv);
unsigned long *o2n;
unsigned long *vp;
double *RxyzUnsorted = (double *) malloc(sizeof(double) * 3 * nv);
double *r, *ru;
SORTVERTSTRUCT *new2old = (SORTVERTSTRUCT *)malloc(sizeof(SORTVERTSTRUCT) * nv);
SORTVERTSTRUCT *n2o;
unsigned long iv, ip;

  /* save sin(latitude) & original vertex number in new2old,
   * copy Rxyz to RxyzUnsorted
   */
  for ( n2o=new2old, r=Rxyz, ru=RxyzUnsorted, iv=0; iv<nv; ++iv, ++n2o) {
    n2o->_sinlat = r[2] / VLEN(r);
    n2o->_oldIdx = iv;
    *(ru++) = *(r++);
    *(ru++) = *(r++);
    *(ru++) = *(r++);
  }

  /* sort new2old by latitude */

  qsort( new2old, nv, sizeof(SORTVERTSTRUCT), qsort_vertStructByLat);

  /* copy vertices from old RxyzUnsorted in new order back to Rxyz */

  for ( n2o=new2old, r=Rxyz, iv=0; iv<nv; ++iv, ++n2o) {
    ru = RxyzUnsorted + (n2o->_oldIdx * 3);
    *(r++) = *(ru++);
    *(r++) = *(ru++);
    *(r++) = *(ru++);
    old2new[n2o->_oldIdx] = iv;     /* fill old2new array from sorted new2old */
  }

  for ( vp=verts, ip=0; ip<np; ++ip) {   /* translate vertex numbers in verts */
  unsigned long tmplng;
    *vp = old2new[*vp]; vp++;
    *vp = old2new[*vp]; vp++;
    *vp = old2new[*vp]; vp++;

#   define VSWAP(A,B) tmplng=vp[A]; vp[A]=vp[B]; vp[B]=tmplng

    /* sort each plate's indices               123 132 213 231 312 321 */

    if ( vp[-3] > vp[-2]) { VSWAP(-3,-2); } /* 123 132 123 231 132 231 */
    if ( vp[-3] > vp[-1]) { VSWAP(-3,-1); } /* 123 132 123 132 132 132 */
    if ( vp[-2] > vp[-1]) { VSWAP(-2,-1); } /* 123 123 123 123 123 123 */

    vp++;                                     /* step by 1-based plate number */
  }

  free( old2new);         /* cleanup */
  free( RxyzUnsorted);
  free( new2old);
  return;
}

#define FREE(A, CAST) if (A) { free(A); A = (CAST *) 0; }

/****************************************************************************/
/* free spud facet model structure members, optionally free spudf structure */

void
spud_freeSpudf( SPUDF *spudf, int spudfToo) {

  if ( !spudf) return;

  FREE( spudf->Rxyz, double);
  FREE( spudf->faceapices, unsigned long);
  FREE( spudf->faceoeidx, long);
  spudf->oe--;                       /* oe pointer was moved */
  FREE( spudf->oe, unsigned long);
  FREE( spudf->oeidx, unsigned long);
  FREE( spudf->platecolor, double);
  FREE( spudf->platenum, unsigned long);
  if ( !spudf->_didNotMallocLatInfo) {
    FREE( spudf->lats, double);
    FREE( spudf->latidx, unsigned long);
    FREE( spudf->nlon, unsigned long);
  }
  if ( spudfToo) free( spudf);
  return;
}

/*************************************************************************/
/* compare two sets of plate indices for qsort - sort in ascending order */

int spudplates_qsort_verts( const void *i1, const void *i2) {
unsigned long *l1 = (unsigned long *) i1;
unsigned long *l2 = (unsigned long *) i2;

# define RTNLG(A) \
  if ( l1[A] < l2[A]) return -1; \
  if (l1[A] > l2[A]) return 1

  RTNLG(0);
  RTNLG(1);
  RTNLG(2);
  fprintf( stderr, "WARNING: duplicate plate vertex indices: %ld %ld %ld\n"
                 , *l1, l1[1], l1[2]);
  fprintf( stderr, "         for plates %ld & %ld\n", l1[3], l2[3]);
  fprintf( stderr
  , "  Check plate model &/or contact programmer, CODE%s\n"
  , " WSNBATGH-spudplates_qsort_verts.0");
  return(0);
}

/**************************************************************/
/* read text plate shape model, return NULL pointer for error */

SPUDF *getplatByname( SPUDR *spudr, char *plateFN, char **outFN) {
FILE *f = (FILE *) 0;
int scanstat, scanstat0, scanstat1;
int ilat, ilon;
long nlines, ilines, iv, iseg, iplate;
SPUDF *spudf;
double *fptr, *colorptr, *platecolor, flt[3];
long lng[3];
unsigned long *lptr, *verts, extraoe=999;

char *fname;
char str[255];

#define MAXMALLOCS 25 /* should be more than enough */
void *mallocs[MAXMALLOCS];
int nmallocs = 0;
long orphans = 0;
long *segpervert;
double tmpmax2;
VEC tmpvec;

  if ( outFN) *outFN = (char *) 0;

# define RTN(A) \
  { \
    while ( nmallocs) free( mallocs[--nmallocs]); \
    if ( f) fclose(f); \
    fprintf( stderr, "%s\n", A); \
    fflush( stderr); \
    return (SPUDF *)0; \
  }

# define MALLOC( PTR, CAST, COUNT) \
  if ( !(PTR = (CAST *) malloc( (COUNT) * sizeof( CAST) ))) \
    RTN( "MALLOC error") \
  else \
    mallocs[nmallocs++] = (void *) PTR

  /* try to plateFN, then try environment variable PLATES, plates, 
   * or file "plates" or "PLATES"
   */
  fname = plateFN;
  if ( !fname) fname = getenv( ENVNAME);
  if ( !fname) fname = fname1;
  f = fopen( fname, "r");

  if ( !f && plateFN) { 
    fprintf( stderr, "Problem opening plate model file:  ");
    RTN( plateFN)
  }

  if ( strcmp( fname1, fname2) && !f) fname = fname2;
  f = fopen( fname, "r");
  if ( !f) RTN( "Plate model not found (not necessarily a problem)") /**/

  if ( outFN) *outFN = fname;

  MALLOC( spudf, SPUDF, 1);

  /*************************/
  /* read 1st line of file - should be NV or 3 columns:  VX VY VZ */

  if ( !fgets( str, 255, f)) RTN( "Problem reading plate model file ")

  scanstat0 = sscanf( str, "%lf %lf %lf", flt, flt+1, flt+2);

  /*************************/
  switch ( scanstat0 ) {
  case 2:           /* assume type 2) "NV NP" then "vx vy vz" then "v1 v2 v3" */

    /* re-read nv & nface as longs */
    scanstat0 = sscanf( str, "%ld %ld", &spudf->nv, &spudf->nface);
    if ( 2 != scanstat0)
      RTN( "Bad format in plate model file, 1st line is not 'NV', 'NV NP' or 'VX VY VZ'\n")

    /* check nv-to-nface consistency */
    if ( spudf->nface != ((2 * spudf->nv)-4) ) {
      sprintf( str, "PLATES NPLT(=%ld) != NV(=%ld)*2 - 4"
                  , spudf->nface, spudf->nv);
      RTN( str)
    }

    /***********
     * the rest of the file should be the same as the scanstat0==3 case
     * - read the first "vx vy vz" line
     * - and drop through to scanstat0==3 case:
     */
    if ( !fgets( str, 255, f)) RTN( "Problem reading plate model file ")
    if ( 3 != sscanf( str, "%lf %lf %lf", flt, flt+1, flt+2) )
      RTN( "Bad format in plate model file, 2nd line is not 'VX VY VZ'\n")
    /************/

  /*************************/
  case 3:                  /*  assume type 3) i.e. "vx vy vx" then "v1 v2 v3" */

    /* count lines, assume nlines = NV*3 - 4 */
    for ( nlines=1; fgets( str, 255, f); nlines++) ;

    if ( (nlines % 3) != 2) {
      sprintf( str
             , "PLATES # of vertices' + plates' (=%ld) != N*3 - 4"
             , nlines);
      RTN( str)
    }

    if ( scanstat0 == 3 ) {               /* calculate nv & nface from nlines */
      spudf->nv = (nlines+4) / 3;
      spudf->nface = (2 * spudf->nv) - 4;

    } else {     /* scanstat0 == 2, nv & nface are known, nlines sanity check */

      if ( nlines != ((spudf->nv*3)-4)) {
        sprintf( str
               , "PLATES vertices' + plates' line count(=%ld) != NV(=%ld)*3 - 4"
               , nlines, spudf->nv);
        RTN( str)
      }
    }
   /* tell plate-reading code below to start reading plate vertices' indices
    * immediately after last vertex' X Y Z
    */
    scanstat1 = 3;
    break;

  /*************************/
  case 1: /* assume type 1) i.e. "NV", "iv vx vy vz", "NPLATE", "ip v1 v2 v3" */

    sscanf( str, "%ld", &spudf->nv); /* re-read NV as long */

    /* read NV lines of vertices' x y z */
    for ( ilines=0; ilines<spudf->nv && fgets( str, 255, f); ++ilines) ;
    if ( ilines != spudf->nv) 
      RTN( "Problem reading plate model file:  too few vertices")

    /* read next line & scan for integers */

    if ( !fgets( str, 255, f)) 
      RTN( "Problem scanning NPLATE or V1 V2 V3 from plate model file")

   /* tell plate-reading code below to whether to read NPLATE or 
    * plate vertices' indices immediately after last vertex' 'iv vx vy vz'
    */
    scanstat1 = sscanf( str, "%ld %ld %ld", lng, lng+1, lng+2);

    /* Test if it was 'NPLATE' or 'V1 V2 V3' */

    switch ( scanstat1) {
    case 3:                             /* if 3 col, assume they are V1 V2 V3 */
      spudf->nface = (2 * spudf->nv) - 4;                 /* calculate NPLATE */
      break;
    case 1:                                  /* if 1 col, assume it is NPLATE */
      spudf->nface = lng[0];
      break;
    default:
      RTN( "Problem scanning NPLATE or V1 V2 V3 from plate model file");
    }
    break;

  /*************************/
  default:
    RTN( "Bad format in plate model file, 1st line is not 'NV', 'NV NP' or 'VX VY VZ'\n")
  } /* switch scanstat0 */
  /*************************/

  /*************************
   * allocate spudf structure
   */
  spudf->nseg = (3 * spudf->nv) - 6;

  MALLOC( spudf->Rxyz, double, spudf->nv*3);
  MALLOC( spudf->oeidx, unsigned long, spudf->nv);
  MALLOC( spudf->faceapices, unsigned long, spudf->nface);
  MALLOC( spudf->faceoeidx, long, spudf->nface);
  /* allow for an extra oe at the beginning; see wraparound discussion below */
  MALLOC( spudf->oe, unsigned long, spudf->nseg+1+extraoe);
  spudf->oe++;

  /* allocate off-segments per vertex array:  
   *   keeps count of segments starting at a vertex for which the 
   *   SEGMENT-starting vertex is not a PLATE-starting vertex
   */
  MALLOC( segpervert, long, spudf->nv);
  MALLOC( spudf->platenum, unsigned long, spudf->nface);

  /****************************
   * rewind file, read in vertices' coordinates, zero out segpervert array
   */
  rewind( f);
  fptr = spudf->Rxyz;

  /*********************/
  switch ( scanstat0 ) {
  case 2:
      fgets( str, 255, f);          /* scanstat0==2:  skip first line (NV NP) */
                                      /* ***N.B. DROP THROUGH TO scanstat0==3 */
  /*********************/
  case 3:                                                  /* read 'vx vy vz' */
    for ( iv=0; iv<spudf->nv; ++iv) {
      fgets( str, 255, f);
      scanstat = sscanf( str, "%lf %lf %lf", fptr, fptr+1, fptr+2);
      if ( scanstat != 3) RTN( "Problem reading plate vertices' xyz")
      fptr += 3;
      segpervert[iv] = 0;
    }
    break;
  /*********************/
  case 1:                                  /* read NV line, then 'iv vx vy vz */
    fgets( str, 255, f);
    for ( iv=0; iv<spudf->nv; ++iv) {
      fgets( str, 255, f);
      scanstat = sscanf( str, "%ld %lf %lf %lf", lng, fptr, fptr+1, fptr+2);
      if ( scanstat != 4) RTN( "Problem reading plate vertices' iv x y z")
      fptr += 3;
      segpervert[iv] = 0;
    }
  } /* switch scanstat0 */
  /*********************/

  /* *******************
   * allocate array for plate vertices + plate number + extra vert/4 for later
   * - also allocate plate colors
   */
  MALLOC( verts, unsigned long, (spudf->nface*4) + 1);
  MALLOC( spudf->platecolor, double, spudf->nface);
  MALLOC( platecolor, double, spudf->nface);

  /* read in plate vertices' indices */

  if ( scanstat1 == 3) {
  int found0, foundnv;
  found0 = foundnv = 0;
    for ( iplate=0, lptr=verts, colorptr=platecolor;
          iplate<spudf->nface; ++iplate, ++colorptr) {
    unsigned long tmplng;
      fgets( str, 255, f);
      scanstat = sscanf( str, "%ld %ld %ld %lf", lptr,lptr+1,lptr+2, colorptr);
      if ( scanstat < 3) RTN( "Problem reading plate vertices v1 v2 v3 [color]")

      /* sort each plate's indices               123 132 213 231 312 321 */
      if ( lptr[0] > lptr[1]) { LSWAP(0,1); } /* 123 132 123 231 132 231 */
      if ( lptr[0] > lptr[2]) { LSWAP(0,2); } /* 123 132 123 132 132 132 */
      if ( lptr[1] > lptr[2]) { LSWAP(1,2); } /* 123 123 123 123 123 123 */

      /* test for vertices numbered 0 or nv */
      if ( !found0 && lptr[0] == 0 ) {
        found0 = 1;
      }
      if ( !foundnv && lptr[2] == spudf->nv) {
      unsigned long i, *llptr;
        foundnv = 1;
        /* correct segpervert - correct verts later */
        /* for ( i=1; i<spudf->nv; ++i) segpervert[i-1] = segpervert[i]; */
        fprintf( stderr
               , "Found 1-based indexing of plate vertices; continuing ...\n");
      }
      if ( foundnv && found0) RTN( "Vertex numbers exceed range:  0 & NV read");

      lptr[3] = iplate + 1;

      /* put in -1.0 for plate color if none read from file */

      if ( scanstat == 3) {
        *colorptr = -1.0;
        /* fptr = spudf->Rxyz + (3 * lptr[1]); /**/
        /* *colorptr = 1.0 + (fptr[2]/VLEN(fptr)); /* sin(lat) /**/
        /* *colorptr = VLEN(fptr); /* radius /**/
      }

      /* segpervert[lptr[1]-foundnv]++;/*update off-segments/vertex counter*/

      /* prepend comment to next line to disable plate (PPLATE) & segment 
       * (PSEG later) printing to stdout
       */
      /* #define PPLATE printf("%10ld%10ld%10ld\n",lptr[0],lptr[1],lptr[2]);/**/
#     ifndef PPLATE
#       define PPLATE
#       define NOPRINT
#     endif

      PPLATE
      lptr += 4;
    } /* for iplate */

    /* correct verts for 1-based indexing */
    if ( foundnv) for ( iplate=0, lptr=verts; iplate<spudf->nface; ++iplate) {
      lptr[0]--; lptr[1]--; lptr[2]--;
      lptr += 4;
    }

  } else {       /* scanstat1 = 1, read in NPLATE, then plates' "iv v1 v2 v3" */
    fgets( str, 255, f);
    for ( iplate=0, lptr=verts, colorptr=platecolor; 
          iplate<spudf->nface; ++iplate, ++colorptr) {
    unsigned long tmplng;
      fgets( str, 255, f);
      scanstat = sscanf( str, "%ld %ld %ld %ld %lf"
                       , lptr+3, lptr, lptr+1, lptr+2, colorptr);
      if ( scanstat < 4) 
        RTN( "Problem reading plate indices ip v1 v2 v3 [color]")

      /* sort each plate's indices               123 132 213 231 312 321 */
      if ( lptr[0] > lptr[1]) { LSWAP(0,1); } /* 123 132 123 231 132 231 */
      if ( lptr[0] > lptr[2]) { LSWAP(0,2); } /* 123 132 123 132 132 132 */
      if ( lptr[1] > lptr[2]) { LSWAP(1,2); } /* 123 123 123 123 123 123 */

      lptr[0]--; lptr[1]--; lptr[2]--;   /* change vertices to zero-base */

      /* put -1.0 for plate color if none read from file */

      if ( scanstat == 4) {
        *colorptr = -1.0;
        /* fptr = spudf->Rxyz + (3 * lptr[1]); /**/
        /* *colorptr = 1.0 + (fptr[2]/VLEN(fptr)); /* sin(lat) /**/
        /* *colorptr = VLEN(fptr); /* radius /**/
      }

      /* segpervert[lptr[1]]++;    /* update off-segment per vertex counter */
      PPLATE
      lptr += 4;
    }
  } /* if scanstat1 == 3 ... else */

  fclose(f); f = (FILE *)0;

  /* put vertex # 0 in extra vert/4 for later */
  *lptr = 0;

  /* re-number vertices by latitude - also resorts vertex #'s for each plate */

  spud_RenumPlateVertices( spudf->nv, spudf->Rxyz, verts);

  /* load segpervert array - number of off-segments per vertex */
  /*                 ***N.B. |    */
  /*                         V    */
  for ( iplate=0, lptr=verts+1; iplate<spudf->nface; ++iplate, lptr += 4) {
    segpervert[*lptr]++;
  }

  /* sort the plates - see spudplates_qsort_verts above */
  qsort( verts, spudf->nface, 4*sizeof(unsigned long), spudplates_qsort_verts);

  /* lay out the segments' oe (other end) indices in order of adjacent plates
   * - all segments start at the lowest-numbered vertex, and go between 
   *   that vertex and that of its other ends, which are stored in the oe array
   *   as indices into the Rxyz array.
   * - for each vertex which has 1 or more other ends, the other end index 
   *   (oeidx) array points to the first other end in the oe array.
   * - a plate (i.e. face or triangle) is defined by a starting vertex & 
   *   the first of two adjacent other ends of that vertex.
   *   - the plate-starting verts are stored in faceapices as indices into Rxyz
   *   - the first of two adj. verts is stored in faceoeidx as an index into oe,
   *     which implies that the order of verts in oe is consistent with the 
   *     order of plate verts around the starting vert.
   * SPECIAL CASES
   * - vertex 0 wraparound:  the plates (& other ends) that have vert 0 as their
   *   starting vert go completely around vert 0, so the first other end needs 
   *   to follow the last other end.  this is accomplished by duplicating 
   *   vert 0's last other end (oe[oeidx[1]-1]) at oe[-1], oe[-1] is 
   *   pointed to by one of faceoeidx elements for start vert 0
   *   (see MALLOC above for how the space is arranged)
   * - last vertex has no other end:  oeidx[nv-1] points off end of oe array 
   *   as upper limit for other ends of nv-2.
   */

  /* use iseg as index into oe */
  for ( lptr=verts, iplate=iseg=iv=0; iv<(spudf->nv-1); ++iv) { 
  unsigned long *curptr, *endptr, *srchptr;
  unsigned long lnf, lif, lnseg, liseg;

    /* count plates that have iv as starting vertex */
    for ( lnf=0; lptr[4*lnf] == iv; ++lnf) ;
    endptr = lptr + (4 * lnf);

    /* index into oe for this iv */
    spudf->oeidx[iv] = iseg;

    if ( lnf) {
    int idx;
    int didloop;

      /* MACRO to step srchptr from current plate to next plate in array
       * - break if either
       *   - moved to adjacent plate crossing common segment between 2 plates 
       *  OR
       *   - no adjacent plate exists
       * - also, set B to be index to vertex in srchptr that matches curptr[A]
       */
#     define WALK1(A,B) \
      srchptr = ((srchptr + 4) == endptr) ? lptr : (srchptr+4); /* wrap */ \
      if ( srchptr == curptr) break; \
      if ( srchptr[1] == curptr[A]) { B=1; break; } \
      if ( srchptr[2] == curptr[A]) { B=2; break; }

      /* search for a segment in the current set of plates that is adjacent 
       * to only one of the current set of plates
       * e.g. lptr points someplace in verts which is the start of 
       * a sequence of plates in verts that all start at vertex A; the 
       * order of the plates is arbitrary in verts, but 
       * they are arranged like this in the model:
       *
       *      a  b
       *      | /
       *      |/
       *      A---c
       *      |\
       *      | \
       *      e  d
       *
       * say the first plate in the lptr sequence is lptr[0/1/2] = A/c/d; 
       * set srchptr to lptr, use WALK1 to find plate adjacent to Acd 
       * along Ac, WALK1 returns srchptr pointing to Abc & sets its 
       * second argument index to c in Abc (srchptr[1 or 2]).
       * Set curptr to srchptr, set the index to b in Abc (3 - Ac => Ab)
       * and repeat WALK1 to set srchptr to Aab, setting the index to 
       * Ab in Aab.  The next WALK1 will break out of loop.
       * 
       * Initialize idx so at least one step is taken if lnf>1:
       *  set idx=1; walk one step from lptr.  
       *  - if no step was taken, set idx to 2
       */
      idx = 1;
      srchptr = curptr = lptr;
      while(1) { WALK1(idx,idx); }
      if ( srchptr == curptr) idx = 2;

      for ( lif=0, srchptr = curptr = lptr; lif<lnf; ++lif) { 
        while (1) { WALK1(idx,idx) }         /* search for adjacent plate */
        if ( srchptr==curptr) break;           /* found no adjacent plate */
        curptr = srchptr;                        /* reset for next search */
        idx = 3 - idx;          /* reset to other segment for next search */
      }

      /* test if we looped through all plates back to lptr */
      didloop = 0;
      if ( lif==lnf) didloop |= 1;                 /* stepped through all */
      if ( lif && srchptr==lptr) didloop |= 2; /* stepped around to start */
      if ( !segpervert[iv]) didloop |= 4;              /* no off-segments */

      if ( didloop==7 && iv) {       /* looping plates w/start vertex > 0 */
        if ( extraoe) {
          fprintf( stderr, "Looping plates with start vert=%ld; continuing...\n"
                         , *lptr);
          extraoe--;                                         /* expand oe */
          spudf->nseg++;                                        /* & nseg */
        } else { 
          sprintf( str, "Looping plates with start vert=%ld; out of extra oe\n"
                      , *lptr);
          RTN( str)
        }

      } else if ( !didloop && !iv) {   /* plates w/vertex 0 didn't loop */

        RTN("Bad plate vertices:  NON-Looping plates w/start vert = 0")
      } else if ( didloop && didloop!=7) { /* didloop is neither 0 nor 7 */
        fprintf( stderr
               , "Bad plate vertices:  low vertex = %ld, did & didn't loop\n"
               , *lptr);
        sprintf( str
               , " count:%s // returned to start:%s; // no off-segments:%s\n%s"
               , ((didloop&1)?"YES":"NO")
               , ((didloop&2)?"YES":"NO")
               , ((didloop&4)?"YES":"NO")
               , " (should be all YES or all NO)");
        RTN( str)
      }

      spudf->faceapices[iplate] = iv;                  /* add starting seg */
      spudf->faceoeidx[iplate] = iseg;
      spudf->oe[iseg++] = curptr[idx];
      spudf->platecolor[iplate] = platecolor[curptr[3]-1];  /* plate color */
      spudf->platenum[iplate++] = curptr[3];               /* plate number */

#     define DOMAX \
      VMINUS2( spudf->Rxyz+(3*iv), spudf->Rxyz+(3*spudf->oe[iseg-1]), tmpvec); \
      tmpmax2 = VDOT( tmpvec, tmpvec); \
      if ( spudf->maxseg2 < tmpmax2) spudf->maxseg2 = tmpmax2;

      /* following controlled by PPLATE above */
#     ifndef NOPRINT
#       define PSEG DOMAX printf( "%10ld%10ld\n", iv, spudf->oe[iseg-1]);
#     else
#       define PSEG DOMAX
#     endif

      PSEG

      lnseg = lnf + (iv ? 1 : 0);      /* add segs inline, loop | non-loop */
                                                        /* unless vertex 0 */

      idx = 3 - idx;                               /* switch to common seg */
      srchptr = curptr;
      for ( liseg=1; liseg<(lnseg-1); ++liseg) {
        curptr = srchptr;                         /* reset for next search */
        spudf->faceapices[iplate] = iv;                 /* add common segs */
        spudf->faceoeidx[iplate] = iseg;
        spudf->oe[iseg++] = curptr[idx];
        PSEG
        /* common seg, don't decr off-segments for this vertex */
        while(1) { WALK1(idx,idx) }
        idx = 3 - idx;           /* reset to other segment for next search */
        spudf->platecolor[iplate] = platecolor[srchptr[3]-1];/*plate color */
        spudf->platenum[iplate++] = srchptr[3];             /* plate number */
      }
      if ( liseg != (lnseg-1)) {
        sprintf( str, "Bad plate vertices - too few segs at vertex %ld", iv);
        RTN( str);
      }

      spudf->oe[iseg++] = srchptr[idx];             /* add ending segment */
      PSEG

      if ( !iv) {           /* special case for looping plates in vertex 0 */
        spudf->faceapices[iplate] = iv;                 /* add extra plate */
        spudf->faceoeidx[iplate] = -1;                        /* BEFORE oe */
        spudf->oe[-1] = srchptr[idx];                         /*  "        */
        curptr = srchptr;          
        while(1) { WALK1(idx,idx) }                   /* walk 1 more step */
        spudf->platecolor[iplate] = platecolor[srchptr[3]-1];/*plate color */
        spudf->platenum[iplate++] = srchptr[3];            /* plate number */
      }

      if ( !didloop) segpervert[iv] -= 2; /* decr off-segments for vertex */

    } /* if lnf */

    if ( segpervert[iv]) { 

      /* there are more segments that start with the current iv i.e. "orphans"; 
       * since the orphans exist with the current iv:
       * 1 iv will be in the second position of a plate vertices triplet
       *   (it's not first or we wouldn't be here, and it's not third
       *    or else the 2nd vertex would be the start of the segment)
       * 2 it cannot occur at or after the plate of current lptr 
       *   (the second vertex of the current & future lptr must be > iv)
       * Strategy:  loop through plates from verts[0:2] to lptr[-4:-2], test if
       *   verts[4*N+1] is iv, 
       *   - if it is, proceed with further tests of its oe verts[4*N+2]
       *   - to avoid duplication, search for oe's in increasing order 
       *     by initializing foundoe to nv (which should never be found) 
       *     and initializing lastoe to 0 (which should also never be found) 
       *     and resetting foundoe to smallest oe greater than the last 
       *     selected oe (lastoe).
       *   - skip any oe's which only occur once because all iv/oe's occur
       *     in pairs, and single occurences are matches to the non-common 
       *     oe's that surround the plates above 
       *   - once an oe passes the above tests, add corresponding verts[4*N+2] 
       *     to ->oe and adjust extraoe & nseg.
       */
    unsigned long lastoe = 0;
    unsigned long foundoe;
    int count;
      while (1) {
        count = 0;                              /* init count to 0 */

        /*                                  see 1    &    2 above  */
        /*                                      V         V        */
        for ( foundoe = spudf->nv, curptr=verts+1; curptr < lptr; curptr+=4) {
          if ( *curptr == iv) {
            if ( curptr[1] < foundoe && curptr[1] > lastoe) {
              foundoe = curptr[1];
              count = 1;
            } else if ( curptr[1] == foundoe) {
              count++; /* should increment 1 to 2 */
            }
          }
        }
        if ( foundoe<spudf->nv) {                   /* orphan segment found */
          if ( count > 2) {                                  /* check count */
            sprintf( str
              , "Bad vertices, %d orphan segments (> 2) between %ld & %ld; %s\n"
              , count, iv, foundoe, "exiting...");
            RTN( str)
          } else if ( count == 2) {                    /* ignore count == 1 */
            orphans++;                            /* increment orphan count */
            spudf->oe[iseg++] = foundoe;                         /* save oe */
            PSEG
            segpervert[iv] -= 2;
          }
          lastoe = foundoe;             /* increase largest found orphan oe */
        } /* foundoe < nv */
        else break; /* break out of while(1) because */
                    /* foundoe == nv => no orphan segment found > lastoe */
      } /* while 1 */
    } /* if segpervert[iv] */

    if ( segpervert[iv]) {
      sprintf( str, "Bad vertices, leftover orphans for vertex %ld", iv);
      RTN( str);
    }

    lptr = endptr;                         /* move to next group of plates */
  } /* for iv < spudf->nv-1 */
  spudf->oeidx[iv] = iseg;                       /* end of other end index */

  if ( orphans) fprintf( stderr, "Orphans:  %ld; continuing ... \n", orphans);

  if ( iseg != spudf->nseg) {
    sprintf( str, "Bad plate vertices - %ld segments found, %ld expected"
                   , iseg, spudf->nseg);
    RTN( str)
  }
  free( platecolor);
  free( verts);
  free( segpervert);

  /* dummy values into lat,lon-related members of spudf */
  {
  static double lats[3] = { -90.0, 0.0, 90.0 };
  static unsigned long latidx[4] = { 0, 1, 2, 3 };
  static unsigned long nlon[3] = { 1, 4, 1 };
    spudf->_didNotMallocLatInfo = 1;
    spudf->eastlon = -1;
    spudf->nlat = 3;
    spudf->lats = lats;
    spudf->latidx = latidx;
    spudf->nlon = nlon;
  }

  if ( spudr) {
    /* put dummy values into spudr - 90 degree shape model */
    spudr->nlatR = 3; spudr->nlonR = 5;
    spudr->eastlon = -1;
    spudr->londel = spudr->latdel = 90.0;

#   define MZ spudr->Rmodel[0][0] = spudr->Rmodel[0][1] = spudr->Rmodel[0][2] \
                                  = spudr->Rmodel[0][3] = spudr->Rmodel[0][4] 
#   define PX spudr->Rmodel[1][0] = spudr->Rmodel[1][4]
#   define MY spudr->Rmodel[1][1] /* WEST longitude */
#   define MX spudr->Rmodel[1][2] 
#   define PY spudr->Rmodel[1][3] 
#   define PZ spudr->Rmodel[1][0] = spudr->Rmodel[1][1] = spudr->Rmodel[1][2] \
                                  = spudr->Rmodel[1][3] = spudr->Rmodel[1][4] 

    MZ=MX=PY=PX=MY=PZ=0.0;
    for ( fptr=spudf->Rxyz, iv=0; iv<spudf->nv; iv++) {
      if ( *fptr > (PX)) PX = *fptr;
      else if ( *fptr < -(MX)) MX = - *fptr;
      fptr++;
      if ( *fptr > (PY)) PY = *fptr;
      else if ( *fptr < - (MY)) MY = - *fptr;
      fptr++;
      if ( *fptr > (PZ)) PZ = *fptr;
      else if ( *fptr < - (MZ)) MZ = - *fptr;
      fptr++;
    }
  } /* if spudr */

  /* calculate unit normals - set pointer null so _calcs allocates space */
  spudf->midpts = spudf->r2 = spudf->uvnorms = (double *) 0;
  spudf->_spudf2 = (void *) 0;
  spudf_calcs( spudf);

  fflush( stderr);
  return spudf;
}
