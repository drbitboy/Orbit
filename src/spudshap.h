/* spudshap.h */

#ifndef _SPUDSHAP_H_
#define _SPUDSHAP_H_

#include "orbit3d.h"

/* R=f(lat,lon) shape model */

#define SPUDRNLAT 181
#define SPUDRNLON 361

typedef struct {
double Rmodel[SPUDRNLAT][SPUDRNLON];  /* R[ilat][ilon] model */
unsigned long nlatR, nlonR;          /* number lats, lons in R, 
                                         includes duplicates at lon=0,360 */
double londel, latdel;                /* delta lat, delta lon for R */
int eastlon;                         /* 1 for east, -1 for west longitude */
} SPUDR;

/* spudf2 structure - for alternate spudview() (spudview_spudf2_()) */

typedef struct SPUDF2str {
  void *_spudf;
  unsigned long *_plateVerts;            /* [nface*3] vertices for each plate */
  unsigned long *_adjPlates;                     /* [nface*3] adjacent plates */
  double *_coneOSilence;    /* [nv*4] cone within which vertex cannot be seen */
  double *_coneOVis;    /* [nv*4] cone within which vertex cannot be occluded */
                              /* cone:  unit vector (x,y,z) + cos(cone angle) */
  unsigned long *_ivV0;                   /* [nv+1] vertex # for in spudv->v0 */
} SPUDF2;

/* facet model representation */

typedef struct {
  unsigned long nv;        /* # vertices */
  unsigned long nface;     /* # facets (plates or triangles) */
  unsigned long nseg;      /* # line segments in */
  double *Rxyz;            /*[nv] vertices' xyz in model coords */
  unsigned long *oe;       /*[nseg] other ends of segments for each vertex */
                           /*        where other end has higher index in [nv] */
  unsigned long *oeidx;    /*[nv] index to oe for each vertex */
  unsigned long *faceapices; /*[nface] lowest numbered vertex of each face */
  long *faceoeidx;         /*[nface] index into oe of first of each pair */
                           /*        of segments of a face */
  unsigned long *platenum; /* [nface] plate number of face from plate model */
  double *platecolor;      /* [nface] plate color of face from plate model */
  double hicolor, locolor; /* hi & low plate colors */
  double avgcolor, sigcolor; /* average & std deviation of plate colors */
  double maxseg2;          /* square of length of longest segment */

  double *uvnorms;         /* [nface] unit normals to facets pointing outward */
  double *midpts;          /* [nface] midpoints of facets such that sphere of */
                           /*         radius sqrt(r2) contains facet vertices */
  double *r2;              /* [nface] radius^2 of smallest sphere containing  */
                           /*         facet verts */

  long _didNotMallocLatInfo;  /* set to 0 unless lat info was not malloc'ed */
  unsigned long nlat;      /* # latitudes in */
  double *lats;            /* [nlat] latitude at each lat */
  unsigned long *latidx;   /* [nlat] index into [nv] arrays */
                           /*        of lon=0 at each lat */
  unsigned long *nlon;     /* [nlat] # lons at each lat */

  int eastlon;
  SPUDF2 *_spudf2;
} SPUDF;

typedef struct {
  SPUDF *_spudf;
  double _VR[4][4];        /*matrix to convert from body to camera coord's */
  double _SR[4][4];        /*matrix to cvt from body to sun-view coord's */
                           /* - for shadow checking */
  double *_xyz;            /*[nv] vertices' xyz in viewing coordinates */
  short *_segstat;         /*[nseg] bit flags of each segment */
  short *_vstat;           /*[nv] bit flags of each pt */
  short *_pstat;           /*[nface] bit flags of each face */
  double *_v0;             /*[nv] xyz[][0] or azimuth of each pt */
  long int *_sortv0;       /*[nv] index into sorted _v0 */
  double *_valt;           /*[nv] sin(altitude) of each pt; used for persp.*/
# if 0
  double *_facemin0;       /*[nface] min xyz[][0] of each face */
  double *_facemin1;       /*[nface] min xyz[][1] of each face */
  long int *_sortfmin0;    /*[nface] index into sorted _facemin0 */
# endif
  int _hidden;             /* 1 for hidden grid pts; 0 for all points viewable*/
  int _persp;              /* whether to project using perspective or not */
  int _track_imager_frame; /* 1 to center imager frame; 0 to center shape */
  int _display_northup;    /* 1 for noraz=0; 0 for noraz from camera */
  double _range;           /* range to object, =0 for no perspective e.g. sun */
  double _viewabf[3];      /*reverse boresight, ABF (calculated by spudview*)*/
  double _viewUpAbf[3];    /* unit vector camera up, ABF (calulated by ") */
  double _scCam[3];        /*position of viewer wrt body Cam Coord (calc by ")*/
  double _scAbf[3];        /* position of viewer, ABF (input, persp only) */
} SPUDV;

#define SPUDV_HID 1
#define SPUDV_SHAD (SPUDV_HID<<1)
#define SPUDV_SHADHID (SPUDV_SHAD|SPUDV_HID)

int getviewByname(double *, unsigned long *, unsigned long *, char *, char **);
#define getview(A,B,C) getviewByname( A, B, C, (char *) 0, (char **) 0)

SPUDF *getplatByname(SPUDR *, char *, char **);
#define getplat(A) getplatByname( A, (char *) 0, (char **) 0)

double lltor0();
double lltor1();
void rmod2face();
void spudview();
void spudviewPersp();
SPUDV *newspudv();
void spudf_colorStats( SPUDF *);
void spudf_calcs( SPUDF *);
void spudf_intersect(
       SPUDF         *in_facetModel
     , VEC            in_spacecraftABF
     , VEC            in_unitVecBoresightABF
     , double        *out_distanceAlongBoresightToIntersection
     , unsigned long *out_plateIndexIntersected
     );
void spud_freeSpudf( SPUDF *, int);

void calcVisConeUpdate( double *uv, double *outCone);
void calcVisConeMulti(double *uv,unsigned long n, VEC outUVec,double *outCone);
void freeSpudf2( SPUDF2 *);
SPUDV *getSpudvByname( char *filename);
SPUDF2 *mallocSpudf2( SPUDF *);
SPUDF2 *newSpudf2( SPUDF *);

#define IDENT3D(A) \
A[0][0]=A[1][1]=A[2][2]=1.0; \
A[0][1]=A[0][2]=A[1][0]=A[1][2]=A[2][0]=A[2][1]=0.

#define ROTANG(FROM,DEL,TO,ANGDEG,IAXIS) \
{static int i,j,k, IFROM,ITO; static double ANGRAD; \
  ANGRAD = ANGDEG deg; IFROM=(IAXIS<2)?IAXIS+1:0; ITO=(IFROM<2)?IFROM+1:0; \
  DEL[IAXIS][IAXIS]=1.; DEL[IFROM][IFROM]=DEL[ITO][ITO]=cos(ANGRAD); \
  DEL[ITO][IFROM]=sin(ANGRAD); DEL[IFROM][ITO]= -DEL[ITO][IFROM]; \
  DEL[IAXIS][ITO]=DEL[IAXIS][IFROM]=DEL[IFROM][IAXIS]=DEL[ITO][IAXIS]=0.; \
  for (i= 0; i < 3; i++) for (j= 0; j < 3; j++) \
  for (TO[i][j]=k= 0; k < 3; k++) TO[i][j]+= DEL[i][k]*FROM[k][j]; }
  
#endif	/* _SPUDSHAP_H_ */
