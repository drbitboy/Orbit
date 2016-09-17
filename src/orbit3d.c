/* orbit 3d routines */

#include <math.h>
#include <stdio.h>
#include <stdlib.h> /* for getenv() */

#include "orbitfort.h"
#include "orbit_spice_names.h"
#include "orbit3d.h"
#include "pointing.h"
#include "orbit_cas.h"

#include "debug.h"

#ifdef DEBUG
static VEC dbgvec;
#endif

static SC *firstsc;
static AP *firstap;

static double degpr, radpd;
static int notfirst = 0;

VEC6 nulVec = { 0., 0., 0., 0., 0., 0. };

/* matrix or vector times matrix 
 * ***N.B. this seems to do the opposite of SPICELIB MXV
 *         i.e. 
 *         vxm( vecin, mtxin, vecout) => MXV( mtxinT, vecin, vecout)
 *         where mtxinT = TRANSPOSE(mtxin)
 *         which means matrices here and in SPICELIB are stored differently
 *
 *                    2
 *     vecout[i] = SUM   (vecin[j] * mtxin[i][j])
 *                    j=0
 */

void vxm( VEC Vin, MTX mtxIn, VEC Vout) { 
double *lclmtxIn, *lclVout;
int i, j;
VEC lclV;

  for ( i=0,lclVout=lclV,lclmtxIn=mtxIn; i<3; i++, lclVout++)
    for ( j=0, *lclVout=0.0; j<3; j++, lclmtxIn++)
      *lclVout += Vin[j] * (*lclmtxIn);

  CPYVEC( lclV, Vout);
  return;
}

void mxm( MTX Min, MTX mtxIn, MTX Mout) { 
double *lclMin, *lclMout;
int i, j;
MTX lclM;
VEC lclV;

  for ( i=0, lclMin=Min, lclMout=lclM; i<3; i++, lclMin++, lclMout += 3) {
    *lclV = *lclMin;
    lclV[1] = lclMin[3];
    lclV[2] = lclMin[6];
    vxm( lclV, mtxIn, lclMout);
  }
  MT2( lclM, Mout);
  return;
}

void Rot2AbyB( char A, char B, VEC v, MTX Mout) {
int ipos = (A=='x') ? 0 : ((A=='y') ? 1 : 2);
int irot = (B=='x') ? 0 : ((B=='y') ? 1 : 2);
int ioth = 3 - (irot+ipos);

double pos = v[ipos];
double oth = v[ioth];
double c, s;
double x2y2 = sqrt((pos*pos) + (oth*oth));

  if ( x2y2 > 0.0) {
    c = pos / x2y2;
    s = oth / x2y2;
    Mout[irot*4] = 1.0;
    Mout[ipos*4] = Mout[ioth*4] = c;
    Mout[ipos*3 + ioth] = +s;
    Mout[ioth*3 + ipos] = -s;
    Mout[irot*3+ipos] = Mout[irot*3+ioth] = Mout[ioth*3+irot] =
    Mout[ipos*3+irot] = 0.0;
  } else {
    Mout[0] = Mout[4] = Mout[8] = 1.0;
    Mout[1] = Mout[2] = Mout[3] = 
    Mout[5] = Mout[6] = Mout[7] = 0.0;
  }
}

/* solve the spacecraft pointing based on aimpoints & boresights */
/* INPUTS:   sc->_vb0    Virtual Boresight Vector, s/c coordinates
 *           ap->ba0     Virtual Boresight aimpoint, j2000 coord's
 *           sc->_rb0    S/C Roll Vector, s/c coord's
 *           ap->ra0     Roll Reference Vector, j2000 coord's
 *
 * OUTPUT:   sc->msctoj2k  matrix to convert vectors from s/c to j2000 coord's
 */

void solveSC( SC *sc, AP *ap) {
MTX m1, m2, m3;
VEC v1, v2, v3;
MTX mr1, mr2, mr3;

  /* rotate s/c virt. boresight to +X axis */

  Rot2AbyB( 'x', 'z', sc->_vb0, m1);
  vxm( sc->_vb0, m1, v1);
  Rot2AbyB( 'x', 'y', v1, m2);
  mxm( m1, m2, m3);

  /* rotate s/c roll boresight to +Y in XZ plane */

  vxm( sc->_rb0, m3, v1);
  Rot2AbyB( 'y', 'x', v1, m2);

  /* save all s/c boresight rot's into m1 */

  mxm( m3, m2, m1);

  /* rotate boresight aimpt to +X axis */

  Rot2AbyB( 'x', 'z', ap->ba0, mr1);
  vxm( ap->ba0, mr1, v1);
  Rot2AbyB( 'x', 'y', v1, mr2);
  mxm( mr1, mr2, mr3);

  /* rotate roll aimpt to +Y in XZ plane */

  vxm( ap->ra0, mr3, v1);
  Rot2AbyB( 'y', 'x', v1, mr2);

  /* save all aimpt rot's into mr1 */

  mxm( mr3, mr2, mr1);

  /* reverse aimpt rot's, combine s/c and aimpt rot's */

  MT(mr1);
  mxm( m1, mr1, sc->_msctoj2k);

  return;
} /* end solveSC */

double vlen( VEC v) {
  return sqrt( v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
}

void vcross( VEC vin1, VEC vin2, VEC vout) {
VEC lclvec;
  lclvec[0] = (vin1[1] * vin2[2]) - (vin2[1] * vin1[2]);
  lclvec[1] = (vin1[2] * vin2[0]) - (vin2[2] * vin1[0]);
  lclvec[2] = (vin1[0] * vin2[1]) - (vin2[0] * vin1[1]);
  CPYVEC( lclvec, vout);
  return;
}

void vunit( VEC vin, VEC vout, double *lll) {
/* double recip = 0.0; *lll = vlen(vin); if ( *lll != 0.0) recip = 1.0 / *lll;*/
double recip = vlen(vin);
  if ( lll ) *lll = recip;
  if ( recip > 0.0) recip = 1.0 / recip;
  else return;
  vout[0] = vin[0] * recip;
  vout[1] = vin[1] * recip;
  vout[2] = vin[2] * recip;
  return;
}

void vmxpb( double m, VEC x, VEC b, VEC y) {
  y[0] = m * x[0] + b[0];
  y[1] = m * x[1] + b[1];
  y[2] = m * x[2] + b[2];
  return;
}

/* from info in arguments, solve for pointing, return camera vertices,
   sub spacecraft lat/lon, all in km and in ABF coordinates */

int
pointing_solve( char *utc, double *et, SC *scsoln, POINTING *bore, POINTING *roll)
{

VEC6 astFromScJ2k, scFromSunJ2k, sunFromAstJ2k, earthFromScJ2k;
VEC uvx, uvy, uvz, uvnadir, uvsun, lclvec;
double tmpdbl, azang, cosaz, sinaz;
MTX scmx, sunmx, earthmx, nadirmx;
MTX mabftoj2k;
AP ap;
fortint success, besthope;
int i;

MTX cmat;
double etout;
fortint ckfound;

CAS *startCAS;

  if ( !notfirst) {
    notfirst = 1;
    degpr = 45.0 / atan(1.0);
    radpd = 1.0 / degpr;
  }

  /* get vectors and matrices at et/utc */

  orbit_spkez( et, utc, scsoln, scFromSunJ2k, scmx
             , astFromScJ2k, mabftoj2k, sunFromAstJ2k, sunmx
             , earthFromScJ2k, earthmx, &success, &besthope);

  if ( besthope != success) return 0;

  CPYMTX( mabftoj2k, scsoln->_mabftoj2k);

  /* get first CAS, load bore & roll if ds40 CAS applies */

  startCAS = orbitgui_get1stCASFromCurItem( scsoln->_cur_item);
  orbit_CAS_ds40Vec( bore, roll, startCAS, *et);

  /* special case for bore->aimpt.vec - allow ds56 to modify it 
   * - ds56 routine will not change bore if ds56 aimpoint ref frame is 
   *       s/c body fixed because bore->aimpt.type will not match
   */

  if ( !startCAS) { 
    COPYB0( bore->aimpt.vec, bore->aimpt.type, lclvec) 
  } else {
    orbit_CAS_ds56Vec( bore->aimpt.vec, startCAS, bore->aimpt.type, *et,lclvec);
  }

  /* load up AP structure with aimpt and roll reference vector
   * ***N.B. These vectors must be in J2000 coordinates and have
   *         the spacecraft as their origin
   */

  /* use macros for input aimpt in aimpt.type reference frame (AVEC) and 
   * for output aimpt in j2k (AAP) - by doing this, the following 
   * switch statement may be duplicated below for the roll reference calculation
   * ***N.B. No longer true, only Inadir, Ieci, Isci & Ij2k are used 
   *         for the roll reference, and of those only Ij2k will use 
   *         a non-(0,0,0) input roll reference vector
   */

#define AVEC lclvec
#define AAP ap.ba0

  switch (bore->aimpt.type) {

  /* abf => asteroid body fixed - take a point that rotates with the asteroid,
   * rotate it with asteroid to get that point's j2000 location, then
   * add the s/c-to-asteroid j2000 vector to it
   */
  case Iabf:
    vxm( AVEC, scsoln->_mabftoj2k, AAP);
    VADD2( AAP, astFromScJ2k, AAP);
    break;

  /* nadir:  AVEC is vector in nadir-sun inertial frame, converted from 
   *         elevation/azimuth in ->aimpt.vec in orbit_cas_ds56Vec() above
   *         - aimpt.vec[0] = elevation (altitude) away from nadir pointing, deg
   *         - aimpt.vec[1] = azimuth clockwise away from sun azimuth, deg
   */
  case Inadir:
    /* get J2k components of Nadir inertial frame */
    vunit( astFromScJ2k, nadirmx, &tmpdbl);                    /* "X":  Nadir */
    vcross( nadirmx, sunFromAstJ2k, nadirmx+6);            /* "Z":  "X" x sun */
    vunit( nadirmx+6, nadirmx+6, &tmpdbl);
    vcross( nadirmx+6, nadirmx, nadirmx+3);      /* "Y":  ZxX (toward sun az) */
    MT( nadirmx);                      /* transpose so matrix converts to J2k */
    vxm( AVEC, nadirmx, AAP);                   /* convert nadir aimpt to J2k */
    break;

  /* aci:  asteroid centered inertial - same as abf only without the rotation
   */
  case Iaci:
    VADD2( AVEC, astFromScJ2k, AAP);
    break;

  /* j2k:  s/c centered inertial - trivial
   */
  case Ij2k:
    CPYVEC( AVEC, AAP);
    break;

  /* eci:  earth centered inertial - similar to aci
   */
  case Ieci:
    VADD2( AVEC, earthFromScJ2k, AAP);
    break;

  /* sci:  sun centered inertial - similar to eci, only notice change of
   *       sign of s/c-sun vector
   */
  case Isci:
    VMINUS2( AVEC, scFromSunJ2k, AAP);
    break;
  }

#undef AVEC
#undef AAP
#define AVEC roll->aimpt.vec
#define AAP ap.ra0

  switch (roll->aimpt.type) {

  case Iabf:
  case Iaci:
    fprintf( stderr, "***WARNING:  ABF or ACI roll ref converted to Nadir\n");
    roll->aimpt.type = Inadir;

  case Inadir:
    VADD2( AVEC, astFromScJ2k, AAP);
    break;

  /* j2k:  s/c centered inertial - trivial
   */
  case Ij2k:
    CPYVEC( AVEC, AAP);
    break;

  /* eci:  earth centered inertial - similar to aci
   */
  case Ieci:
    VADD2( AVEC, earthFromScJ2k, AAP);
    break;

  /* sci:  sun centered inertial - similar to eci, only notice change of
   *       sign of s/c-sun vector
   */
  case Isci:
    VMINUS2( AVEC, scFromSunJ2k, AAP);
    break;
  }

/* load up SC structure with virtual boresight and s/c roll vector */

#define VB lclvec

  switch (bore->scvec.type) {
  case Iinstr:
    CPYVEC( scsoln->_scB0, VB);
    break;
  case Ipanel:
    CPYVEC( scsoln->_panel, VB);
    break;
  case Ix:
    VB[0]=1.0; VB[1]=VB[2]=0.0;
    break;
  case Iy:
    VB[1]=1.0; VB[0]=VB[2]=0.0;
    break;
  case Iz:
    VB[2]=1.0; VB[0]=VB[1]=0.0;
    break;
  case Iuser:
    CPYVEC( bore->scvec.vec, VB);
    break;
  }
#undef VB

  /* special case for virtual boresight - allow ds56 to modify it 
   * - ds56 routine will only change bore here if ds56 aimpoint ref frame is 
   *       s/c body fixed because Isbf will match
   */

  if ( !startCAS) { 
    CPYVEC( lclvec, scsoln->_vb0); 
  } else {
    orbit_CAS_ds56Vec( lclvec, startCAS, Isbf, *et, scsoln->_vb0);
  }

#define VB scsoln->_rb0

  switch (roll->scvec.type) {
  case Iinstr:
    CPYVEC( scsoln->_scB0, VB);
    break;
  case Ipanel:
    CPYVEC( scsoln->_panel, VB);
    break;
  case Ix:
    VB[0]=1.0; VB[1]=VB[2]=0.0;
    break;
  case Iy:
    VB[1]=1.0; VB[0]=VB[2]=0.0;
    break;
  case Iz:
    VB[2]=1.0; VB[0]=VB[1]=0.0;
    break;
  case Iuser:
    CPYVEC( roll->scvec.vec, VB);
    break;
  }

/* DPR((stderr,"before solveSC...")); /**/

/* finally, do the solution i.e. get msctoj2k in "Vsc x msctoj2k = Vj2k" */
  solveSC( scsoln, &ap);

/* DPR((stderr,"...after solveSC\n")); /**/

  if ( scsoln->_tryck) {
  double sclk;
  static int skipCMtxPrint; /* set to 2/1 to/{to not} print C-matrix to stderr*/

    if ( !skipCMtxPrint) {
      if ( getenv("PRINT_CMTX") ) {              /* ... controlled by env var */
        skipCMtxPrint= 2;
        /* fprintf( stderr, "will print C matrices, %d\n", skipCMtxPrint); */
      } else {
        skipCMtxPrint = 1;
        /* fprintf( stderr, "will not print C matrices, %d\n", skipCMtxPrint);*/
      }
    }

    sce2t( &scsoln->_scid, et, &sclk);
    orbit_loadSpiceOpts( sclk, SPICEOPTS_CKO);

    ospice_et_ckgp( et, &scsoln->_scid, cmat, &etout, &ckfound);
  
    if ( ckfound) {
      if ( skipCMtxPrint != 1) {
        fprintf( stderr, MTXOUT( "solveSC - SC to J2k:\n", scsoln->_msctoj2k));
        fprintf( stderr, MTXOUT( "SPICE   - SC to J2k:\n", cmat));
        fflush( stderr);
      }
      CPYMTX( cmat, scsoln->_msctoj2k);
    } else {
      fprintf( stderr, "didn't find a c-matrix\n");
      fflush( stderr);
    }
  } else ckfound = 0;

/* do transposes */

  MT2( scsoln->_mabftoj2k, scsoln->_mj2ktoabf);
  MT2( scsoln->_msctoj2k, scsoln->_mj2ktosc);

/* combine s/c-to-j2k and j2k-to-abf to
 * get matrix to transform sc vectors to abf vectors */

  mxm( scsoln->_msctoj2k, scsoln->_mj2ktoabf, scsoln->_msctoabf);

  MT2( scsoln->_msctoabf, scsoln->_mabftosc);

/* combine s/c-to-camera and abf-to-s/c to
 * get matrix to transform abf vectors to camera vectors */

  mxm( scsoln->_mabftosc, scsoln->_msctocam, scsoln->_mabftocam);

  MT2( scsoln->_mabftocam, scsoln->_mcamtoabf);

/* convert vectors to abf */

  vxm( scsoln->_b0, scsoln->_msctoabf, scsoln->_babf);
  vxm( scsoln->_vb0, scsoln->_msctoabf, scsoln->_vbabf);
  vxm( scsoln->_rb0, scsoln->_msctoabf, scsoln->_rbabf);

/* DPR(( stderr, VEC3OUT( "astFromScJ2k:\n", astFromScJ2k))); /**/

  vxm( astFromScJ2k, scsoln->_mj2ktoabf, scsoln->_scFromAstAbf);
  VNEG( scsoln->_scFromAstAbf);

#if 0
/* #ifdef DEBUG /**/
DPR(( stderr, VEC3OUT( "scFromAstAbf:\n", scsoln->_scFromAstAbf)));
vxm( scsoln->_scFromAstAbf, scsoln->_mabftocam, dbgvec);
DPR(( stderr, VEC3OUT( "scFromAstCam:\n", dbgvec)));
dbgvec[0] = 1.0; dbgvec[1] = dbgvec[2] = 0.0;
vxm( dbgvec, scsoln->_mabftocam, dbgvec);
DPR(( stderr, VEC3OUT( "XAstToCam:\n", dbgvec)));
dbgvec[1] = 1.0; dbgvec[0] = dbgvec[2] = 0.0;
vxm( dbgvec, scsoln->_mabftocam, dbgvec);
DPR(( stderr, VEC3OUT( "YAstToCam:\n", dbgvec)));
dbgvec[2] = 1.0; dbgvec[0] = dbgvec[1] = 0.0;
vxm( dbgvec, scsoln->_mabftocam, dbgvec);
DPR(( stderr, VEC3OUT( "XAstToCam:\n", dbgvec)));
#endif

  vxm( sunFromAstJ2k, scsoln->_mj2ktoabf, scsoln->_sunFromAstAbf);
  vxm( scFromSunJ2k, scsoln->_mj2ktoabf, scsoln->_sunFromScAbf);
  VNEG( scsoln->_sunFromScAbf);
  vxm( earthFromScJ2k, scsoln->_mj2ktoabf, scsoln->_earthFromScAbf);

/* DPR(( stderr, "pointing_solve: ncampts=%ld\n", scsoln->_ncampts)); /**/

  for ( i=0; i < (3*scsoln->_ncampts); i += 3) {

    vxm( scsoln->_camvert+i, scsoln->_msctoabf, scsoln->_camabf+i);
  }

/* DPR(( stderr, "pointing_solve: i,ncampts=%ld, %ld\n", i, scsoln->_ncampts)); /**/

  return ckfound ? 1 : -1;                  /* return -1 if no c-matrix found */
}

/* find matrix that rotates  other points around a given vector */
/* BTC version */

void solveAxisRot( VEC Axis, double ang, MTX Mout)
{
VEC ax1;
MTX m1, m2, m3;

  /* find matrix that will rotate Axis to Z axis */

  Rot2AbyB( 'z', 'x', Axis, m1);
  vxm( Axis, m1, ax1);
  Rot2AbyB( 'z', 'y', ax1, m2);

  mxm( m1, m2, m3);

  /* now rotate by ang around Z */

  m2[0] = m2[4] =cos(ang);
  m2[1] = -( m2[3] = sin(ang));
  m2[2] = m2[5] = m2[6] = m2[7] = 0.0;
  m2[8] = 1.0;

  mxm( m3, m2, m1);

  /* reverse (transpose) m3 & output */

  MT( m3);
  mxm( m1, m3, Mout);

  return;
}

/* find matrix that rotates other points around a given Axis vector */
/* graphics gems version */

void rotation3D(VEC Axis, const double angleRad, MTX m3x3) {
    double  c = cos(angleRad),
	    s = sin(angleRad),
	    t = 1.0 - c;
    double lenax;

/* DPR((stderr, "angleRad = %lg //", angleRad)); /**/

#define VX 0
#define VY 1
#define VZ 2
#define V3( A, B, C, M0) *(M0) = A; *((M0)+1) = B; *((M0)+2) = C

  /* normalize Axis */

  lenax = VLEN(Axis);
  if ( lenax > 0.0) {
    lenax = 1.0 / lenax;
    Axis[VX] *= lenax;  Axis[VY] *= lenax;  Axis[VZ] *= lenax;
  }

  V3(t * Axis[VX] * Axis[VX] + c,
     t * Axis[VX] * Axis[VY] - s * Axis[VZ],
     t * Axis[VX] * Axis[VZ] + s * Axis[VY], m3x3);

  V3(t * Axis[VX] * Axis[VY] + s * Axis[VZ],
     t * Axis[VY] * Axis[VY] + c,
     t * Axis[VY] * Axis[VZ] - s * Axis[VX], m3x3+3);

  V3(t * Axis[VX] * Axis[VZ] - s * Axis[VY],
     t * Axis[VY] * Axis[VZ] + s * Axis[VX],
     t * Axis[VZ] * Axis[VZ] + c,            m3x3+6);
  return;
}
#undef VX
#undef VY
#undef VZ
#undef V3

#define px pt[ix]
#define py pt[iy]
#define v1x vtx1[ix]
#define v1y vtx1[iy]
#define v2x vtx2[ix]
#define v2y vtx2[iy]

/**********************************************************************/
/* find if a point is inside a polygon
 *
 * INPUTS
 * pt      is the point to test
 * poly[]  contains the vertices of the polygon
 * npoly   is the number of vertices (& sides)
 * ix      is the index into the vector of the "x" values
 * iy      is the index into the vector of the "y" values
 *
 * OUTPUTS
 * <NONE>
 *
 * RETURN
 * 1       if pt is inside the polygon
 * 0       if pt is NOT inside the polygon
 */

int 
orbit_inside( VEC pt, double *poly, int npoly, int ix, int iy) {
double *vtx1, *vtx2;
int ipoly;
double frac, pxe;
int islefts;
double maxx, minx, maxy, miny;

  /* step through sides connecting vertices of polygon
   * - increment islefts for each time a +x vector from pt crosses a side of
   *     the polygon
   * - return(1) if pt is ON the side of the polygon
   * - don't count a crossing if it is at the top of a vector so only one
   *     side is counted as crossed if pt is even with a vertex
   */

  vtx2 = poly;
  islefts = 0;

  for ( ipoly = 1; ipoly <= npoly; ipoly++) {
    vtx1 = vtx2;
    vtx2 = poly + (3 * (ipoly%npoly));

    if ( v1y < v2y) { maxy = v2y; miny = v1y; }   /* find side's min/max y */
    else { maxy = v1y; miny = v2y; }

    if ( py > maxy ) ;                            /* pt above side */
    else if ( py < miny) ;                        /* below */
    else {

      if ( v1x < v2x) { maxx = v2x; minx = v1x; } /* find side's min/max x */
      else { maxx = v1x; minx = v2x; }

      if ( px > maxx) ;                             /* right */
      else if ( px < minx && py < maxy) islefts++;  /* left of side, below top*/
      else if ( px < minx) ;                        /* left of side, at top */
      else if ( v2y == v1y) return(1);              /* on horizontal side */
      else {
        frac = (py-v1y) / (v2y - v1y);  /* frac of side where pt is vertically*/
        pxe = frac * (v2x - v1x) + v1x; /* x value of side even with pt */
        if ( px == pxe) return(1);      /* pt on side */
        else if ( px < pxe) islefts++;  /* pt to left of side */
      }
    }
  } /* for (ipoly */
  return( islefts % 2); /* return 0 for even number of crossings, 1 for odd */
}
#undef px
#undef py
#undef v1x
#undef v1y
#undef v2x
#undef v2y
