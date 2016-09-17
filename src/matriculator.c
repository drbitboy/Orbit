#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#include "orbit_spice_names.h"
#include "orbit3d.h"

#define MTRCLATOR_FIXVERSION "MATRICULATOR V"
#define MTRCLATOR_VERSION "0"

#define NDESC 20L   /* max length of descriptor */
#define NCOMM 47L   /* max length of comment */

/* printf format strings; relationship between position of 
 * VECSTRUCT number "(#%03d)" & NCOMM above must be maintained
 */

#define VECFMT "%15.7lg %15.7lg %15.7lg  (#%03ld)  %s\n"
#define MTXFMT "%15.7lg %15.7lg %15.7lg  ...\n%15.7lg %15.7lg %15.7lg  ...\n%15.7lg %15.7lg %15.7lg  (#%03ld)  %s\n"
#define SCALARFMT "%47.7lg  (#%03ld)  %s\n"
#define COMMENTFMT "%-47s  (#%03ld)\n"

typedef             /* Vector Structure (may also be scalar or comment */
struct Vs{
 long type;
 VEC v;
 MTX m;
 long n, oldn, inpNum;
 char name[NDESC+1];
 char comment[NCOMM+1];
 struct Vs *next;
} VECSTRUCT;

typedef VECSTRUCT *VSP;    /* pointer to vector structure */

/* TYPEs of VECSTRUCTs */

enum { TYPE_VECpos=0, TYPE_MTXpos, TYPE_SCALARpos, TYPE_COMMENTpos
     , TYPE_COUNTpos};

#define TYPE_VEC (1L<<TYPE_VECpos)
#define TYPE_MTX (1L<<TYPE_MTXpos)
#define TYPE_SCALAR (1L<<TYPE_SCALARpos)
#define TYPE_COMMENT (1L<<TYPE_COMMENTpos)
#define TYPE_ANY (TYPE_VEC | TYPE_MTX | TYPE_SCALAR | TYPE_COMMENT)

#define NUMINS 1000    /* max number of concurrently open stacked input files */
FILE *inFiles[NUMINS];                             /* streams for those files */
long curInpNum;            /* index into inFiles of current input file stream */
#define curin inFiles[curInpNum]                 /* current input file stream */

static VSP V;                           /* start of linked list of VECSTRUCTs */static VSP *nextVspToPrintP;               /* point to last ->next, typically */
static long nVsp;    /* unique identifier for each VECSTRUCT, increments by 1 */
static VSP firstVecOfMtx;

/* print out a VECSTRUCT on one line */
void
fprintVsp( FILE *f, VSP vsp) {
  if ( !vsp) return;
  switch (vsp->type) {
  case TYPE_VEC:
    fprintf( f, VECFMT, vsp->v[0], vsp->v[1], vsp->v[2], vsp->n, vsp->name);
    break;
  case TYPE_MTX:
    fprintf( f, MTXFMT, vsp->m[0], vsp->m[1], vsp->m[2]
                      , vsp->m[3], vsp->m[4], vsp->m[5]
                      , vsp->m[6], vsp->m[7], vsp->m[8]
                      , vsp->n, vsp->name);
    break;
  case TYPE_SCALAR:
    fprintf( f, SCALARFMT , vsp->v[0], vsp->n, vsp->name);
    break;
  case TYPE_COMMENT:
    fprintf( f, COMMENTFMT, vsp->comment, vsp->n);
    break;
  default:
    fprintf( stderr
           , "matriculator Program error; contact programmer, code WSNBATGH-1\n");
    break;
  }
  return;
}

#define printVsp(VVV) fprintVsp( stdout, VVV)

VSP
findVspOld( oldNum, inpNum) {
VSP *vspp;
  for ( vspp = &V; *vspp; vspp = &(*vspp)->next) {
    if ( (*vspp)->oldn == oldNum && (*vspp)->inpNum == inpNum) break;
  }
  return *vspp;
}

VSP
findVsp( long i) {
VSP *vsp = &V;
  for ( vsp=&V; *vsp; vsp = &((*vsp)->next)) { if ( (*vsp)->n == i) break; }
  return *vsp;
}

VSP
findVspPrev( VSP vsp) {
VSP *vspPrev;
  for ( vspPrev=&V; *vspPrev; vspPrev = &((*vspPrev)->next)) { 
    if ( (*vspPrev)->next == vsp) break;
  }
  return *vspPrev;
}

void 
deleteVsp( long i) {
VSP vsp = findVsp( i);
VSP vspPrev;
  if ( vsp) {
    if ( vspPrev=findVspPrev( vsp)) vspPrev->next = vsp->next;
    else V = V->next;
    free( vsp);
    nextVspToPrintP = &V;
  }
  return;
}

#define newVec( V, VDESC) newVsp( TYPE_VEC, V, VDESC)
#define newMtx( M, VDESC) newVsp( TYPE_MTX, M, VDESC)
#define newScalar( V, VDESC) newVsp( TYPE_SCALAR, V, VDESC)
#define newComment( VDESC) newVsp( TYPE_COMMENT, (double *) 0, VDESC)

VSP
newVsp( long vspType, VEC v, char *vDesc) {
VSP *newVSP = &V;
VSP vs;
char tmpChar[10];
int isComment = vspType == TYPE_COMMENT;

  while ( *newVSP) newVSP = &((*newVSP)->next);
  *newVSP = (VSP) malloc( sizeof( VECSTRUCT));
  vs = *newVSP;

  switch ( vspType) {
  case TYPE_VEC:
    CPYVEC( v, vs->v);
    break;
  case TYPE_MTX:
    CPYMTX( v, vs->m);
    break;
  case TYPE_SCALAR:
    *vs->v = vs->v[1] = vs->v[2] = *v;
    break;
  case TYPE_COMMENT:
    strncpy( vs->comment, vDesc, NCOMM);

    /* move vDesc pointer to possible start of (#...) to update numbers */

    if ( strlen( vDesc) > (NCOMM+2)) vDesc += (NCOMM+2); 
    else vDesc += strlen(vDesc);
    break;
  default:
    fprintf( stderr
           , "matriculator Program error; contact programmer, code WSNBATGH-0\n"
           );
    free( vs);
    return (VSP) 0;
    break;
  }

  vs->type = vspType;
  vs->oldn = vs->n = nVsp++;
  strncpy( vs->name, vDesc, NDESC);
  vs->name[NDESC] = '\0';                        /* ensure string termination */
  vs->next = (VSP) 0;
  vs->inpNum = curInpNum;

  /* combine 3 successive vectors into a matrix
   * - to trigger the building of a matrix the first two vectors must have
   *   the string "..." as their vDesc
   * - that a matrix build is in progress indicated by a non-null
   *   static (VSP *) firstVecOfMtx
   */
  if ( vspType != TYPE_VEC) firstVecOfMtx = (VSP) 0;
  else {
    if ( !firstVecOfMtx && !strcmp( vDesc, "...") ) {/* vs is first of matrix */
      firstVecOfMtx = vs;
    } else if ( firstVecOfMtx) {      /* if a matrix build in in progress ... */

      if ( firstVecOfMtx->inpNum != curInpNum) { /* matrices can't span files */
        firstVecOfMtx = (VSP) 0;                      /* reset matrix build */

      } else if ( firstVecOfMtx->next == vs &&      /* vs is second of matrix */
           !strcmp( vDesc, "...")) {
        /* do nothing, wait for third vec */

      } else {                               /* vs is third & final of matrix */
        firstVecOfMtx->type = TYPE_MTX;                  /* convert to matrix */
        CPYVEC( firstVecOfMtx->v, firstVecOfMtx->m);  /* copy vecs to mtx ... */
        CPYVEC( firstVecOfMtx->next->v, firstVecOfMtx->m+3);
        CPYVEC( vs->v, firstVecOfMtx->m+6);
        strcpy( firstVecOfMtx->name, vs->name); /* copy name of 3. vec to mtx */
        free( firstVecOfMtx->next);                      /* free 2. & 3. vecs */
        free( vs);
        firstVecOfMtx->next = (VSP) 0;                         /* update link */
        nVsp = firstVecOfMtx->n + 1;                      /* reset identifier */
        vs = firstVecOfMtx;                   /* reset current vec struct ptr */
        firstVecOfMtx = (VSP) 0;                        /* reset matrix build */
        if ( !curInpNum) {                         /* reset next VSP to print */
        VSP prevVsp = findVspPrev( vs);
          if ( prevVsp) nextVspToPrintP = &prevVsp->next;
          else nextVspToPrintP = &V;
        }
      }
    }
  }


  /* update numbers:  #[0-9]* */
  if ( *vDesc == '(' && (vDesc+5) == strchr( vDesc, ')') ) 
  if ( vDesc[1] == '#') {

  /* get old number */

  if ( 3 == sscanf( vDesc, "%[(#]%ld%[)]", tmpChar, &vs->oldn, tmpChar)) {
  long oldNum;
  VSP vsRef;
  char *sptr = isComment ? vs->comment : (vDesc + 6);
  char dStr[NCOMM*4];     /* temp string to hold updated description/comments */
  char *dptr = dStr;

    /* copy rest of label */

    while ( *sptr == ' ') ++sptr;   /* skip leading spaces */

    while ( *sptr) {
      *(dptr++) = *(sptr++);
      *dptr = '\0';
      if (sptr[-1] == '#' && 1 == sscanf( sptr, "%ld", &oldNum)) {
        if ( (vsRef = findVspOld( oldNum, curInpNum))) {
          sprintf( dptr, "%ld", vsRef->n);
        } else {
          strcpy( dptr, "?");
        }
        dptr += strlen( dptr);
        while ( isdigit(*sptr) ) sptr++;
      }
    }
    strncpy( isComment ? vs->comment : vs->name
           , dStr
           , isComment ? NCOMM : NDESC);
  }
  }
  return( vs);
}

#define GETVSPI( I, TYPE_THING) \
  vsp = findVsp( I); \
  if ( !vsp) { printf( "Bad vector #:  %ld\n", I); break; } \
  if ( !(vsp->type & (TYPE_THING))) { printf( "Not a vector\n"); break; }

#define GET2VSP( P1, P2, TYPE_THING) \
  if ( nary) { \
    GETVSPI( ii, (TYPE_THING)) \
    v1 = vsp->v; \
    GETVSPI( i, (TYPE_THING)) \
  } else { \
    GETVSPBYNUM( P1, (TYPE_THING)) \
    v1 = vsp->v; ii=i; \
    GETVSPBYNUM( P2, (TYPE_THING)) \
  }

#define GETVSPBYNUM( PROMPT, TYPE_THING) \
  printf( PROMPT); \
  *inLine = '\0'; \
  if ( !fgets( inLine, INLEN, curin)) break; \
  if ( 1 !=  sscanf( inLine, "%ld", &i)) { printf( "Bad vector #\n"); break; } \
  GETVSPI( i, (TYPE_THING))

#define r2d(R) ((R)*(45.0/atan(1.0)))
#define d2r(D) ((D)*(atan(1.0)/45.0))

void
vecInpClose() {
VSP *vspp;

  if ( curInpNum) {
    fclose( curin);
    fprintf( stderr, "Done importing vectors at level %ld\n", curInpNum);
  }

  /* reset inpNums since this file is being closed */

  for ( vspp = &V; *vspp; vspp = &(*vspp)->next) {
    if ( (*vspp)->inpNum == curInpNum) (*vspp)->inpNum = -1;
  }
  curInpNum--;
  return;
}

int
main() {
#define INLEN 1000
char inLine[INLEN];
char tmpStr[INLEN];
char *cptr;
long i, ii, iii, iv;
long j, jj;
VSP vsp;
VEC lclVec;
MTX lclMtx, fromMtx, toMtx;
double *v1, *v2;
double scal, vlenprod, cosang, rotd, rotr;
int isScalar;
FILE *newFile;
int nary;
char *fNBC;

  curInpNum = 0;
  curin = stdin;

  nVsp = 1;
  firstVecOfMtx = V = (VSP) 0;
  nextVspToPrintP = &V;

#define VPROMPT printf( "Enter option (h => help):  ")

  VPROMPT;
  while ( curInpNum > -1) {
    *inLine = '\0';
    fgets( inLine, INLEN, curin);
    if ( strlen( inLine)) {
      if ( (cptr=strchr( inLine, '\n'))) *cptr = '\0';
 
      /* find first non-blank character */
      for ( fNBC=inLine; *fNBC == ' '; ++fNBC) ;
 
      /* binary & unary ops */
      nary = 1;
      *tmpStr = '\0';
  
      if ( *fNBC=='n' &&                    /* new vector:  n X Y Z <comment> */
           2 < sscanf( inLine, "%*[n ]%lf%*[, ]%lf%*[, ]%lf%*[ ]%[!-z ]"
                             , lclVec, lclVec+1, lclVec+2, tmpStr)) {
        *inLine = 'n';
        isScalar = 0;
      } else if                                 /* quick vector:  x y z descr */
        ( 4 == sscanf( inLine, "%lf%*[, ]%lf%*[, ]%lf%*[ ]%[!-z ]"
                             , lclVec, lclVec+1, lclVec+2, tmpStr)) {
        *inLine = 'n';
        isScalar = 0;
      } else if                    /* import or export file: < file or > file */
        ( 1 == sscanf( inLine, "%*[<>]%s", tmpStr)) {
      } else if ( *fNBC=='d' &&                        /* delete vector:  d N */
          1 == sscanf( inLine, "%*[d ]%ld", &i)) {
        if ( 2 != sscanf( inLine, "%*[d ]%ld%*[- ]%ld", &i, &ii)) ii=i;
        *inLine = 'd';
      } else if ( (*fNBC=='h' || *fNBC=='?') &&      /* help OP:  h OP | ? OP */
          1 == sscanf( inLine, "%*[h? ]%s", tmpStr)) {
        *inLine = 'h';
      } else if ( *fNBC=='m' &&                  /* create matrix:  m N N N N */
          4 <= sscanf( inLine, "%*[m ]%ld%ld%ld%ld%[!-z ]"
                             , &i, &ii, &iii, &iv, tmpStr)) {
        *inLine = 'm';
      } else if ( *fNBC=='t' &&                     /* transpose matrix:  t N */
          1 == sscanf( inLine, "%*[t ]%ld", &i)) {
        *inLine = 't';
      } else if ( *fNBC=='s' &&             /* to spherical coordinates:  s N */
          1 == sscanf( inLine, "%*[s ]%ld", &i)) {
        *inLine = 's';
      } else if ( *fNBC=='S' &&          /* from spherical coordinates:   S N */
          1 == sscanf( inLine, "%*[S ]%ld", &i)) {
        *inLine = 'S';
      } else if ( *fNBC=='l' &&             /* to lat,long coordinates:   l N */
          1 == sscanf( inLine, "%*[l ]%ld", &i)) {
        *inLine = 'l';
      } else if ( *fNBC=='L' &&           /* from lat,long coordinates:   L N */
          1 == sscanf( inLine, "%*[L ]%ld", &i)) {
        *inLine = 'L';
      } else if ( *fNBC=='u' &&                            /* normalize:  u N */
          1 == sscanf( inLine, "%*[u ]%ld", &i)) {
        *inLine = 'u';
      } else if                                                /* add:  N + N */
        ( 2 == sscanf( inLine, "%ld%*[+ ]%ld", &ii, &i)) {
        *inLine = '+';
      } else if                                           /* subtract:  N - N */
        ( 2 == sscanf( inLine, "%ld%*[- ]%ld", &ii, &i)) {
        *inLine = '-';
      } else if                   /* scale:            "N * F"  or "N * #N"  OR 
                                   * matrix multiply:  "M * #N" or "M * #M"
                                   */
        ( 2 == sscanf( inLine, "%ld%*[#* ]%lf", &i, &scal)) {
        /* alternate possibility:  N/M * #N/M */
        if ( 0 == sscanf( inLine, "%*ld%*[* ]%*[#]%ld", &ii)) ii = -1;
        *inLine = '*';
      } else if                                   /* divide:  N / F or N / #N */
        ( 2 == sscanf( inLine, "%ld%*[#/ ]%lf", &i, &scal)) {
        /* alternate possibility:  N / #N */
        if ( 0 == sscanf( inLine, "%*ld%*[/ ]%*[#]%ld", &ii)) ii = -1;
        *inLine = '/';
      } else if                                      /* cross product:  N x N */
        ( 2 == sscanf( inLine, "%ld%*[x ]%ld", &ii, &i)) {
        *inLine = 'x';
      } else if          /* dot product:  N . N - trailing spaces not allowed */
        ( 2 == sscanf( inLine, "%ld%*[. ]%ld%[ ]", &ii, &i, tmpStr)) {
        *inLine = '.';
      } else if                                    /* rotate:  N r N rotation */
        ( 3 == sscanf( inLine, "%ld%*[r ]%ld%lf", &ii, &i, &rotd)) {
        *inLine = 'r';
      } else if  /* quick scalar:  F descr - must go after N op N tests above */
        ( 2 == sscanf( inLine, "%lf%*[ ]%[!-z ]", lclVec, tmpStr)) {
        *inLine = 'n';
        isScalar = 1;
      } else if                                             /* comment:  !... */
        ( 1 == sscanf( inLine, "%*[!]%[!-z ]", tmpStr)) {
        *inLine = '!';
      } else nary = 0;
  
      switch ( *inLine) {
  
      case 'q':    /* quit */
        return(0);
        break;
  
      case '!':    /* quit */
        newComment( inLine);
        break;
  
      case '>':    /* import or export file */
      case '<':
        if ( !nary) {
          printf( "Enter filename to %s:  "
                , (*inLine=='>') ? "export" : "import");
          *tmpStr = '\0';
          fgets( tmpStr, INLEN, curin);
        }
        if ( (cptr=strchr( tmpStr, '\n'))) *cptr = '\0';
  
        newFile = fopen( tmpStr, (*inLine == '>') ? "w" : "r");
        if ( !newFile) {
          fprintf( stderr, "Error opening file %s for %s\n"
                         , tmpStr, (*inLine == '>') ? "export" : "import");
          break;
        }
  
        if ( *inLine == '>') {
        VSP *vspp;
          fprintf( stderr, "Exporting current state to %s ...", tmpStr);
          fprintf( newFile, "%s%s\n", MTRCLATOR_FIXVERSION, MTRCLATOR_VERSION);
          for ( vspp=&V; *vspp; vspp=&(*vspp)->next) {
            fprintVsp( newFile, *vspp);
          }
          fclose( newFile);
          fprintf( stderr, " done\n");
        } else {
          ++curInpNum;
          curin = newFile;
          fprintf( stderr, "Importing vectors from %s at level %ld\n"
                         , tmpStr, curInpNum);
        }
        break;
  
      case 'd':    /* delete vector(s) */
        if ( !nary) {
          GETVSPBYNUM( "Vector('s) #(s) to delete (# or #-#; hit enter to do nothing):  ", TYPE_VEC)
          if ( 2 != sscanf( inLine, "%ld%*[- ]%ld", &i, &ii)) ii = i; \
        }
        while ( i <= ii) deleteVsp( i++);
        break;
  
      case 'm':    /* new matrix */
        /* convert two set of vectors into a rotation matrix by associating 
         * common vectors with an intermediate reference frame
         *
         * INPUT:  two pairs af vectors, each pair has one FROM vector and 
         * one TO vector.  First pair are coincident (and are the intermediate 
         * X axis), second pair are coplanar with first pair (and are in 
         * the intermediate XZ plane)
         *
         * first pair are intermediate X axis, convert to unit vectors:
         * - from => fromMtx+0
         * - to   => toMtx+0
         * second pair are in intermediate XZ plane
         * - from => v1
         * - to   => vsp->v
         */
        if ( nary) {
          GETVSPI( i, TYPE_VEC)    vhat( vsp->v, fromMtx);
          GETVSPI( ii, TYPE_VEC)   vhat( vsp->v, toMtx);
          GETVSPI( iii, TYPE_VEC)  v1 =  vsp->v;
          GETVSPI( iv, TYPE_VEC)
        } else { 
          GET2VSP( "Common vector # in FROM frame:  "
                 , "Common vector # in TO frame:  "
                 , TYPE_VEC)
          vhat( v1, fromMtx); vhat( vsp->v, toMtx);

          GET2VSP( "Common-plane vector # in FROM frame:  "
                 , "Common-plane vector # in TO frame:  "
                 , TYPE_VEC)
          printf( "Enter description (%ld characters max):  ", NDESC);
          *tmpStr = '\0';
          fgets( tmpStr, INLEN, curin);
        }
        if ( cptr = strchr( tmpStr, '\n')) *cptr = '\0';

        /* plane normal is ZcrossX => intermediate Y */
        ucrss( v1, fromMtx, fromMtx+3); ucrss( vsp->v, toMtx, toMtx+3); 

        /* XcrossY => intermediate Z */
        ucrss( fromMtx, fromMtx+3, fromMtx+6); ucrss( toMtx, toMtx+3, toMtx+6);

        /* at this point, both matrices convert from vector in their own 
         * ref frame to intermediate ref frm; transpose toMtx so it 
         * converts from the intermediate frame to the "to" frame, then 
         * combine fromMtx & toMtx
         */
        MT( toMtx);
        mxm( fromMtx, toMtx, lclMtx);
        newMtx( lclMtx, tmpStr);
        break;
  
      case 'n':    /* new vector */
        if ( !nary) {
          printf( "Enter vector elements (x,y,z):  ");
          *inLine = '\0';
          fgets( inLine, INLEN, curin);
          if ( 3 != sscanf( inLine, "%lf %lf %lf", lclVec, lclVec+1, lclVec+2)){
            printf( "Bad elements\n");
            break;
          }
          printf( "Enter description (%ld characters max):  ", NDESC);
          *tmpStr = '\0';
          fgets( tmpStr, INLEN, curin);
        }
        if ( cptr = strchr( tmpStr, '\n')) *cptr = '\0';
        if ( isScalar) newScalar( lclVec, tmpStr);
        else newVec( lclVec, tmpStr);
        break;

      case 'u':
        if ( nary) { 
          if ( !(vsp = findVsp(i))) break;
          if ( !(vsp->type & TYPE_VEC)) { printf( "Not a vector\n"); break; }
        } else { GETVSPBYNUM( "Vector # to unitize:  ", TYPE_VEC) }
        if ( vsp) {
          vhat( vsp->v, lclVec);
          sprintf( tmpStr, "Unit vec of #%ld", i);
          newVec( lclVec, tmpStr);
        }
        break;
  
      case '+':
        GET2VSP( "First vector # to add:  ", "Second vector # to add:  "
               , TYPE_VEC)
        VADD2( v1, vsp->v, lclVec);
        sprintf( tmpStr, "#%ld + #%ld", ii, i);
        newVec( lclVec, tmpStr);
        break;
  
      case '-':
        GET2VSP( "Vector # from which to subtract:  "
               , "Vector # to subtract:  "
               , TYPE_VEC)
        VMINUS2( v1, vsp->v, lclVec);
        sprintf( tmpStr, "#%ld - #%ld", ii, i);
        newVec( lclVec, tmpStr);
        break;
  
      case '*':
      case '/':
        if ( nary) { 
          if ( (vsp = findVsp(i))) 
            if ( !(vsp->type & (TYPE_VEC|TYPE_MTX))) vsp = (VSP) 0;
          if ( !vsp) { printf( "#%ld is not a vector or matrix\n", i); break; }
        } else {
          sprintf( tmpStr, "Vector # to %s or matrix # to multiply:  "
                         , (*inLine == '*') ? "scale" : "divide");
          GETVSPBYNUM( tmpStr, TYPE_VEC|TYPE_MTX)
          if ( vsp->type == TYPE_VEC) {
            printf( "Enter factor or '#<scalar number>':  ");
            fgets( inLine, INLEN, curin);
            ii = -1;
            if ( 1 != sscanf( inLine, "%lf", &scal)) {
              if ( 1 != sscanf( inLine, "%*[#]%ld", &ii)) { 
                printf( "Bad scale factor\n");
                break;
              }
            }
          } else { /* vsp is matrix */
            printf( "Enter '#<vector number>' or '#<matrix number>':  ");
            fgets( inLine, INLEN, curin);
            ii = -1;
            if ( 1 != sscanf( inLine, "%*[#]%ld", &ii)) { 
              printf( "Bad vector/matrix number\n");
              break;
            }
          }
        }

        /* to this point, vsp is first operand, and:
         * - if vsp is vector
         *   - if ii > 0, ii is scalar number, else scal is scale factor
         * - if vsp is matrix
         *   - ii is vector number or matrix number
         */
        if ( vsp->type == TYPE_VEC) {
          if ( ii > 0 ) {             /* get factor from its scalar VECSTRUCT */
          VSP vsp2;
            if ( (vsp2=findVsp(ii))) 
              if ( !(vsp2->type&TYPE_SCALAR)) vsp2=(VSP) 0;
            if ( !vsp2) { printf( "#%ld is not a scalar\n", ii); break; }
            scal = *vsp2->v;
          }
          if ( *inLine == '/') {
            if ( scal == 0.0) { printf( "Can't divide by 0\n"); break; }
            VSCAL2( 1.0/scal, vsp->v, lclVec);
          } else {
            VSCAL2( scal, vsp->v, lclVec);
          }
          sprintf( tmpStr, "#%ld %c %lg", i, *inLine, scal);
          newVec( lclVec, tmpStr);

        } else {  /* vsp is matrix */
        VSP vsp2;
        /* MTX lclMtx; */
          if ( (vsp2=findVsp(ii)))
            if ( !(vsp2->type&(TYPE_VEC|TYPE_MTX))) vsp2=(VSP) 0;
          if ( !vsp2) { printf( "#%ld is not a vector or matrix\n", ii); break;}
          if ( vsp2->type == TYPE_VEC) {
            vxm( vsp2->v, vsp->m, lclVec);
            sprintf( tmpStr, "#%ld * #%ld (mtx * vec)", ii, i);
            newVec( lclVec, tmpStr);
          } else {
            mxm( vsp->m, vsp2->m, lclMtx);
            sprintf( tmpStr, "#%ld * #%ld (mtx * mtx)", ii, i);
            newMtx( lclMtx, tmpStr);
          }
        }
        break;
  
      case 'x':
        GET2VSP( "First vector # of cross product:  "
               , "Second vector # of cross product:  "
               , TYPE_VEC)
        vcrss( v1, vsp->v, lclVec);
        sprintf( tmpStr, "#%ld x #%ld", ii, i);
        newVec( lclVec, tmpStr);
        break;
  
      case '.':
        GET2VSP( "First vector # of dot product:  "
               , "Second vector # of dot product:  "
               , TYPE_VEC)
        scal = VDOT( v1, vsp->v);
        sprintf( tmpStr, "= #%ld . #%ld", ii, i);
        newScalar( &scal, tmpStr);
        vlenprod = sqrt( VDOT(v1,v1) * VDOT(vsp->v,vsp->v));
        if ( vlenprod > 0.0) {
          cosang = scal / vlenprod;
          if ( cosang > 1.0) cosang = 1.0;
          else if ( cosang < -1.0) cosang = -1.0;
          lclVec[0] = r2d(acos( cosang));
          sprintf( tmpStr, "= ang #%ld-#%ld deg", ii, i);
          newScalar( lclVec, tmpStr);
        }
        break;
  
      case 't':
        if ( nary) { 
          if ( !(vsp = findVsp(i))) break;
          if ( !(vsp->type & TYPE_MTX)) { printf( "Not a matrix\n"); break; }
        } else { GETVSPBYNUM( "Matrix # to transpose:  ", TYPE_MTX) }
        MT2( vsp->m, lclMtx);
        sprintf( tmpStr, "#%ld transpose", i);
        newMtx( lclMtx, tmpStr);
        break;
  
      case 's':
        if ( nary) { 
          if ( !(vsp = findVsp(i))) break;
          if ( !(vsp->type & TYPE_VEC)) { printf( "Not a vector\n"); break; }
        } else { GETVSPBYNUM( "Vector # to convert to R,RA,DEC:  ", TYPE_VEC) }
        lclVec[0] = VLEN( vsp->v);
        if ( lclVec[0] == 0.0) {
          lclVec[1] = lclVec[2] = 0.0;
        } else {
          lclVec[2] = r2d( asin( vsp->v[2] / lclVec[0]) );  /* DEC */
          lclVec[1] = r2d( atan2( vsp->v[1], vsp->v[0]) );  /* RA */
        }
        if ( lclVec[1] < 0.0) lclVec[1] += 360.0;
        sprintf( tmpStr, "#%ld R, RA, DEC", i);
        newVec( lclVec, tmpStr);
        break;
  
      case 'S':
        if ( nary) { 
          if ( !(vsp = findVsp(i))) break;
          if ( !(vsp->type & TYPE_VEC)) { printf( "Not a vector\n"); break; }
        } else { GETVSPBYNUM( "Vector # to convert from R,RA,DEC:  ", TYPE_VEC) }
        lclVec[0] = vsp->v[0] * cos(d2r(vsp->v[1])) * cos(d2r(vsp->v[2]));
        lclVec[1] = vsp->v[0] * sin(d2r(vsp->v[1])) * cos(d2r(vsp->v[2]));
        lclVec[2] = vsp->v[0] * sin(d2r(vsp->v[2]));
        sprintf( tmpStr, "xyz of #%ld(R,RA,DEC)", i);
        newVec( lclVec, tmpStr);
        break;
  
      case 'l':
        if ( nary) { 
          if ( !(vsp = findVsp(i))) break;
          if ( !(vsp->type & TYPE_VEC)) { printf( "Not a vector\n"); break; }
        } else { GETVSPBYNUM( "Vector # to convert to lat,long(w),r:  "
                          , TYPE_VEC) }
        lclVec[2] = VLEN( vsp->v);
        if ( lclVec[2] == 0.0) {
          lclVec[0] = lclVec[1] = 0.0;
        } else {
          lclVec[0] = r2d( asin( vsp->v[2] / lclVec[2]) );   /* lat */
          lclVec[1] = r2d( atan2( -vsp->v[1], vsp->v[0]) );  /* WEST long */
        }
        if ( lclVec[1] < 0.0) lclVec[1] += 360.0;
        sprintf( tmpStr, "#%ld LatLonWr", i);
        newVec( lclVec, tmpStr);
        break;
  
      case 'L':                   /* vsp->v[0] = lat, [1]=long(w); [2]=radius */
        if ( nary) { 
          if ( !(vsp = findVsp(i))) break;
          if ( !(vsp->type & TYPE_VEC)) { printf( "Not a vector\n"); break; }
        } else { GETVSPBYNUM( "Vector # to convert from lat,long(w),r:  "
                          , TYPE_VEC) }
        lclVec[0] = vsp->v[2] * cos(d2r(vsp->v[1])) * cos(d2r(vsp->v[0]));
        lclVec[1] = -vsp->v[2] * sin(d2r(vsp->v[1])) * cos(d2r(vsp->v[0]));
        lclVec[2] = vsp->v[2] * sin(d2r(vsp->v[0]));
        sprintf( tmpStr, "xyz of #%ldLatLonWr", i);
        newVec( lclVec, tmpStr);
        break;
  
      case 'r':
        GET2VSP( "Vector # to rotate:  "
               , "Vector # of rotation axis:  "
               , TYPE_VEC)
        if ( !nary) {
          printf( "Enter rotation angle (degrees):  ");
          fgets( inLine, INLEN, curin);
          if ( 1 != sscanf( inLine, "%lf", &rotd)) { 
            printf( "Bad rotation angle\n");
            break;
          }
        }
        rotr = d2r(rotd);
        vrotv( v1, vsp->v, &rotr, lclVec);
        sprintf( tmpStr, "#%ld around #%ld by %lg", ii, i, rotd);
        newVec( lclVec, tmpStr);
        break;
  
      case '?':
      case 'h':
        if ( nary) {
          switch (*tmpStr) {

          case '?':
          case 'h':
            printf( "\n\
h [OP]\n\
\n\
  h => commands' summary\n\
  h OP => help on specific operator (e.g. 'h .' or 'h m'\n\
            \n");
            break;

          case 'n':
            printf( "\n\
n [X Y Z C]\n\
  new vector X Y Z; C => comments\n\
            \n");
            break;

          case 'd':
            printf( "\n\
d [N] | d [N1-N2]\n\
  delete vector(s) #N(#N1 to #N2)\n\
            \n");
            break;

          case 'u':
            printf( "\n\
u [N]\n\
  unit vector parallel to vector #N\n\
            \n");
            break;

          case '+':
            printf( "\n\
N1 + N2\n\
  add vectors #N1 & #N2\n\
            \n");
            break;

          case '-':
            printf( "\n\
N1 - N2\n\
  subtract vector #N2 from #N1\n\
            \n");
            break;

          case '*':
            printf( "\n\
N * F\n\
  scale vector #N by F (F is floating point number or #N where N is a scalar)\n\
\n\
M * N\n\
  multiply vec #N by matrix #M\n\
\n\
M1 * M2\n\
  multiply matrix #M2 by matrix #M1\n\
            \n");
            break;

          case '/':
            printf( "\n\
N / F\n\
  divide vec #N by F (F=float or scalar #)\n\
            \n");
            break;

          case 'm':
            printf( "\n\
m FROM1 TO1 FROM2 TO2\n\
  create rotation matrix to rotate vectors #FROM1 & #FROM2 in one reference \n\
  frame to vectors #TO1 & #TO2, respectively, in a second reference frame.\n\
  The resultant matrix will rotate #FROM1 to #TO1 exactly, and #FROM2 as \n\
  close as possible to #TO2.\n\
  Example:  if vectors #1 & #2 represent an instrument boresight & solar panel \n\
    normal, respectively, in a spacecraft reference frame, and vectors #3 & #4\n\
    represent the s/c-to-target & s/c-to-sun vectors, in the J2k inertial \n\
    reference frame, then the command 'm 1 3 2 4' will create a rotation matrix\n\
    that represents a s/c attitude with the instrument boresight (#1) pointed \n\
    at the target (#3) & the solar panel normal (#2) pointing as close as \n\
    possible toward the sun (#4) and that rotates vectors from that s/c \n\
    reference frame to the J2k reference frame via the command 'M * N' where \n\
    #M is the resulant matrix and #N is a spacecraft vector.\n\
            \n");
            break;

          case 't':
            printf( "\n\
t [M]\n\
  transpose matrix #M\n\
            \n");
            break;

          case 'x':
            printf( "\n\
N1 x N2\n\
  cross product of vector #N1 with #N2\n\
            \n");
            break;

          case '.':
            printf( "\n\
N1 . N2\n\
  dot product of vector #N1 with #N2\n\
            \n");
            break;

          case 'r':
            printf( "\n\
N1 r N2 D\n\
  rotate vector #N1 around  #N2 by D degrees\n\
            \n");
            break;

          case 's':
          case 'S':
            printf( "\n\
s [N]\n\
  convert vector #N to Range, RA, DEC (RA & DEC in degrees)\n\
\n\
S [N]\n\
  convert `vector' #N (Range, RA (deg), DEC (deg)) to XYZ\n\
            \n");
            break;

          case 'l':
          case 'L':
            printf( "\n\
l [N]\n\
  convert vector #N to Lat, Long(W), Range (Lat & Long in degrees)\n\
\n\
L [N]\n\
  convert `vector' #N (Lat (deg), Long(W) (deg), Range) to XYZ\n\
            \n");
            break;

          case '<':
            printf( "\n\
< [file]\n\
  import commands & vectors from file\n\
\n");
            break;

          case '>':
            printf( "\n\
> [file]\n\
  export current state to file\n\
\n");
            break;

          case '!':
            printf( "\n\
! [comment]\n\
  create a comment\n\
\n");
            break;

          case 'q':
            printf( "\n\
q\n\
  quit matriculator\n\
\n");
            break;

          default:
            printf( "\n\
%c\n\
  no help available (invalid command); will result in printout of current state\n\
            \n", *tmpStr);
          }
          break;
        }
        *inLine = 'h';
        printf( "\n\
Enter an operator (program will prompt for arguments):\n\
 (n) new vector   (d) delete vector  (u) normalize vector   (-) subtract vectors\n\
 (+) add vectors  (x) cross product  (*) scale/mult vector  (/) divide vector\n\
 (!) comment      (.) dot product    (m) create matrix      (t) transpose matrix\n\
 (<) import vecs  (s) xyz=>R,RA,DEC  (l) xyz=>lat,long(w),r\n\
 (>) export vecs  (S) R,RA,DEC=>xyz  (L) lat,long(w),r=>xyz\n\
 (q) quit         (h) help           (r) rotate vec\n\
\n\
Alternate binary & unary syntaces:\n\
 n X Y Z C       (new vec)  d N[-N]         (delete)  m N N N N C   (create mtx)\n\
 u N           (normalize)  N + N              (add)  N - N           (subtract)\n\
 N * F             (scale)  N / F           (divide)         ***N.B. F is scalar\n\
 M * N           (mtx*vec)  M * M          (mtx*mtx)\n\
 N x N             (cross)  N . N              (dot)  N r Naxis D       (rotate)\n\
 s N         (to R,RA,DEC)  l N   (to lat,long(w),r)  < importfile (import vecs)\n\
 S N       (from R,RA,DEC)  L N (from lat,long(w),r)  > exportfile (export vecs)\n\
 X Y Z C (new vec, C reqd)  X C (new scalar, C reqd)  ! C              (comment)\n\
 h OP   (help re operator)\n\
\n\
 XYZ = floating point numbers  N = vector/scalar #       M = matrix #\n\
 F = floating pt number or #N  C = descriptive string    D = rotation in degrees\n\
\n\
        ");
        break;
  
      case 'V':   /* test for MATRICULATOR V..., else drop through to default */
        if ( curInpNum) {
        int hdrLen = strlen( MTRCLATOR_FIXVERSION);
          if ( !strncmp( inLine, MTRCLATOR_FIXVERSION, hdrLen)) {
            fprintf( stderr, " - import file format is %s ...\n", inLine);
          }
          break;
        }
        
      default:  /* case 'V':  should precede this */
        nextVspToPrintP = &V;
        break;
      }
      if ( !curInpNum) {
        while ( *nextVspToPrintP) {
          printVsp( *nextVspToPrintP);
          nextVspToPrintP = &(*nextVspToPrintP)->next;
        }
        if ( *inLine == '.') printf( tmpStr);
      }
    }
    if ( feof( curin)) vecInpClose();
    if ( !curInpNum) { VPROMPT; }
  } /* while ( curInpNum > -1) */
  printf( "\n");
  return(0);
}
