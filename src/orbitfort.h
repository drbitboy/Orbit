#ifndef _ORBITFORT_H_
#define _ORBITFORT_H_

#include <string.h>
#include <stdio.h>

typedef long fortint;

/* Structure to contain optional SPICE files' info from SPICESPEC file
 *   - are to be loaded only when they cover relevant et
 */

typedef 
struct SPICEOPTSstr {
  int _type;             /* spk, ck or pck see enum { SPICEOPTS_... below */
  fortint _handle;      /* SPICE handle - set to SPICEOUT when not loaded */
  char *_filename;                                             /* ???(;-) */
  char *_absPathName;
  double _ets[2];                                     /* et range covered */
  struct SPICEOPTSstr *next;                   /* pointer for linked list */
} SPICEOPTS;

#define SPICEOUT -9999

enum { SPICEOPTS_SPKO=0L, SPICEOPTS_CKO, SPICEOPTS_ORIBPCKO
     , SPICEOPTS_SPK, SPICEOPTS_CK, SPICEOPTS_ORIBPCK, SPICEOPTS_TK
     , SPICEOPTS_ASTID, SPICEOPTS_EARTHID, SPICEOPTS_SUNID, SPICEOPTS_SCID
     , SPICEOPTS_OTHERBODYID
     , SPICEOPTS_UTC, SPICEOPTS_PLATES, SPICEOPTS_SPUD, SPICEOPTS_INCLUDE
     , SPICEOPTS_COMMENT
     , SPICEOPTS_ENDSO
     , SPICEOPTS_LAST
     };

typedef struct { long _type; char *_flag; } IDFLAGS;

static IDFLAGS spiceFlags[] = {
  SPICEOPTS_CKO, "CKO:"
, SPICEOPTS_SPKO, "SPKO:"
, SPICEOPTS_ORIBPCKO, "ORIBPCKO:"
, SPICEOPTS_SPK, "SPK:"
, SPICEOPTS_CK, "CK:"
, SPICEOPTS_ORIBPCK, "ORIBPCK:"
, SPICEOPTS_TK, "TK:"
, SPICEOPTS_ASTID, "ASTID:"
, SPICEOPTS_EARTHID, "EARTHID:"
, SPICEOPTS_SUNID, "SUNID:"
, SPICEOPTS_SCID, "SCID:"
, SPICEOPTS_OTHERBODYID, "OTHERBODYID:"
, SPICEOPTS_UTC, "UTC:"
, SPICEOPTS_PLATES, "PLATES:"
, SPICEOPTS_SPUD, "SPUD:"
, SPICEOPTS_INCLUDE, "INCLUDE:"
, SPICEOPTS_TK, "PCK:"
, SPICEOPTS_COMMENT, "COMMENT:"
, SPICEOPTS_ENDSO, "ENDSO:"
, SPICEOPTS_TK, "LEAPSEC:"
, SPICEOPTS_TK, "SCLK:"
, SPICEOPTS_LAST, (char *) 0
};

static IDFLAGS *
char2Id( char *inpLine, IDFLAGS *idFlags) {
IDFLAGS *lclIDF = idFlags;
char c[256];
  for ( lclIDF=idFlags; lclIDF->_flag; ++lclIDF) {
    if ( strncmp( inpLine, lclIDF->_flag, strlen(lclIDF->_flag))) continue;
    if ( 1 != sscanf(inpLine, "%254s", c)) continue;
    if ( !strcmp( c, lclIDF->_flag)) break;
  }
  return lclIDF;
}

static IDFLAGS *
id2Char( long id, IDFLAGS *idFlags) {
IDFLAGS *lclIDF = idFlags;
  for ( lclIDF=idFlags; lclIDF->_flag; ++lclIDF) {
    if ( id == lclIDF->_type) break;
  }
  return lclIDF;
}

#ifndef _ORBITFORT_H_TYPESONLY_
void orbit_loadSpiceOpts( double, int);
#endif

#endif /* _ORBITFORT_H_ */
