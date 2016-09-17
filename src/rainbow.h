/* RGB sources:  IDL Rainbow color table:  approx every 10th color
 *
 * IDL> loadct,13 (?)
 * IDL> tvlct,r,g,b,/get
 * IDL> openw,1,'rgb.lst'
 * IDL> printf,1,transpose([[r],[g],[b]])
 * IDL> exit
 *
 * % grep -n '[0-9]' rgb.lst | grep 0: | sed -e 's/^.*://' -e 's:  *:, :g' > x.x
 *
 * copy x.x to rainbowRGB[] initialization below
 * - eliminate first line (too dark)
 * - eliminate first comma
 * - add ", RB_RGBTERM" at end as terminator
 * - value range 0-255, scale to 255-65525 below
 */

#define RB_RGBTERM 256

static unsigned short rainbowRGB[] = {
/*  45, 0, 43   too dark */
  80, 0, 100
, 85, 0, 154
, 66, 0, 209
, 16, 0, 255
, 0, 46, 255
, 0, 110, 255
, 0, 174, 255
, 0, 233, 255
, 0, 255, 216
, 0, 255, 152
, 0, 255, 89
, 0, 255, 25
, 33, 255, 0
, 97, 255, 0
, 161, 255, 0
, 225, 255, 0
, 255, 221, 0
, 255, 161, 0
, 255, 97, 0
, 255, 34, 0
, RB_RGBTERM};   /* terminator */

#define RB_RAINBOWOFFSET 1000
#define RB_GRAYOFFSET 2000
#define RB_NUMGRAYS 18
