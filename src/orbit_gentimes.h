/* orbit_gentimes.h */

#ifndef _ORBIT_GENTIMES_H_
#define _ORBIT_GENTIMES_H_

#include "orbit3d.h"

#define MAXLENFILNAM 256

/* valid bit positions for GENTIME.timeFormat */
enum { GTFMT_KMET=0L   /* double precision NEAR MET - multiply by 1000 to use */
     , GTFMT_INTMET    /* integer MET - read as DP, use directly */
     , GTFMT_SCLK      /* SPICE SCLK string - may span several columns */
     , GTFMT_DPET      /* s past J2000 */
     , GTFMT_UTC       /* SPICE UTC string - may span several columns */
     } ;
#define GTFMT_LAST GTFMT_UTC
#define GTFMT_ALLBITS ( (1L<<GTFMT_KMET) \
                      | (1L<<GTFMT_INTMET) \
                      | (1L<<GTFMT_SCLK) \
                      | (1L<<GTFMT_DPET) \
                      | (1L<<GTFMT_UTC) \
                      )

typedef struct {
  SC *_sc;
  char _filnam[MAXLENFILNAM];
  long _timeFormat;
  long _timeColumns[2];
  long _instrColumn;
  long _mirrorPosnColumn;
  long _headerLines;
} GENTIME;

int orbit_gentimes_act( GENTIME *);

#endif
