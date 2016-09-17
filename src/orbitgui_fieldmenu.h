/* orbitgui_fieldmenu.h - structure for gui for menu of fields */

#ifndef _ORBITGUI_FIELDMENU_H_
#define _ORBITGUI_FIELDMENU_H_

#include "orbitfort.h"

#include "orbit_params.h"   /* to get ORBIT_NOCHANGEVAL */

enum { FLD_FILE_SAVE   /* save info to file */
     , FLD_FILE_LOAD   /* load info from file */
     } ;

enum { FLD_END,     /* Ending field */
       FLD_TXT,     /* Text field */
       FLD_TXTFILEREAD,     /* Text field - filename to read */
       FLD_TXTFILEWRITE,     /* Text field - filename to write */
       FLD_INT,     /* int field */
       FLD_LNG,     /* long field */
       FLD_LNG8,     /* multiple long fields, pass # values with ->subtype,
                     *   limit with fld_txt_maxlen
                     */
       FLD_FORTINT, /* fortran integer field */
       FLD_DBL,     /* double field */
       FLD_DBL8,    /* multiple double fields, pass # values with ->subtype, 
                     *   limit with fld_txt_maxlen
                     */
       FLD_BIT,     /* 1 bit out of many field */
       FLD_BITS,    /* any number of bits out of many */
       FLD_SLIDER_UPLEFT,  /* slider - increasing right to left (NIS mirror) */
       FLD_SLIDER_UPRIGHT ,  /* slider increasing left to right */
       FLD_UTC      /* UTC time */
     } ;
#define FLD_SLIDER FLD_SLIDER_UPLEFT   /* default */

#define FLD_TXTFMT "%s"
#define FLD_INTFMT "%d"
#define FLD_LNGFMT "%ld"
#define FLD_DBLFMT "%.9lg"
#define FLD_MAXTXTLENPERDBL 20  /* was 17; made a multiple of 4 for alignment */
/* 17 = 9 + 1(sign) + 1(decimal pt) + 5(E+xxx) + 1(space) */
#define FLD_MAXTXTLENPERLNG 12
/* 12 = 10 + 1(sign) + 1(space) */

#define FLD_MAXBITS 10 /* maximum number of bits in a set of buttons */

typedef struct FLDMENUstr {
  union {
    Widget _fld_w;
    Widget _fld_ws[FLD_MAXBITS];
  } _widget_union;
  char *lbl_txt;
  char *fld_txt0;
  int fld_txt_maxlen;
  int type;
  int subtype;
  union {
    char *_fld_txt;
    int *_fld_int;
    long *_fld_lng;
    fortint *_fld_fortint;
    double *_fld_dbl;
  } _val_union;
  union {
    long _fld_lowbit;
    long _fld_loend;
  } _lo_range_union;
  union {
    long _fld_numbits;
    long _fld_hiend;
  } _hi_range_union;
  int (*client_call)();
  void *client_data;
} FLDMENU;

/* special function that should never be called, only purpose 
 * (when (FLDMENU *)->client_call above is set to it) 
 * is to instruct orbitgui_fldmenu_create_dialog() to disable the OK button
 */
int orbitgui_fldmenu_readOnlyFlagFunction( void *, FLDMENU *);

#define fld_txt _val_union._fld_txt
#define fld_int _val_union._fld_int
#define fld_lng _val_union._fld_lng
#define fld_fortint _val_union._fld_fortint
#define fld_dbl _val_union._fld_dbl

#define fld_w _widget_union._fld_w
#define fld_ws _widget_union._fld_ws

#define fld_lowbit _lo_range_union._fld_lowbit
#define fld_loend  _lo_range_union._fld_loend

#define fld_numbits _hi_range_union._fld_numbits
#define fld_hiend  _hi_range_union._fld_hiend

void orbitgui_create_fldmenu_dialog( Widget, char *, FLDMENU *);

# define MKFLDANYBIT(FM,BITS,ZEROBIT) \
  (FM)->type = FLD_BIT; (FM)->fld_lng = &BITS; \
  (FM)->fld_lowbit = ZEROBIT

# define MKFLDANYBITS(FM,BITS,ZEROBIT) \
  MKFLDANYBIT(FM,BITS,ZEROBIT);               /* duplicate one-bit code ... */ \
  (FM)->type = FLD_BITS                       /* ... and then override ->type */

# define MKFLDANYLNG(FM,A) (FM)->type = FLD_LNG; (FM)->fld_lng = &A
# define MKFLDANYLNGP(FM,A) (FM)->type = FLD_LNG; (FM)->fld_lng = (A)

#endif /* #ifndef _ORBITGUI_FIELDMENU_H_ */

/* end of orbitgui_fieldmenu.h */
