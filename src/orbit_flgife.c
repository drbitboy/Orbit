/*****************************************************************
 * flgife.c: FBM Release 1.0 25-Feb-90 Michael Mauldin
 *
 * Modifications to GIFENCODE are Copyright (C) 1989,1990 by
 * Michael Mauldin.  Permission is granted to use this file in whole
 * or in part for any purpose, educational, recreational or commercial,
 * provided that this copyright notice is retained unchanged.
 * This software is available to all free of charge by anonymous
 * FTP and in the UUNET archives.
 *
 * CONTENTS
 *	GIFEncode( wfile, GHeight, GWidth, GInterlace, Background, 
 *		   BitsPerPixel, Red, Green, Blue, GetPixel, voidStar )
 *
 * EDITLOG
 *	LastEditDate = Mon Jun 25 00:09:49 1990 - Michael Mauldin
 *	LastFileName = /usr2/mlm/src/misc/fbm/flgifc.c
 *
 * HISTORY
 * 25-Jun-90  Michael Mauldin (mlm@cs.cmu.edu) Carnegie Mellon
 *	Package for Release 1.0
 *
 * 07-Mar-89  Michael Mauldin (mlm) at Carnegie Mellon University
 *	Beta release (version 0.9) mlm@cs.cmu.edu
 *
 * 19-Feb-89  Michael Mauldin (mlm) at Carnegie Mellon University
 *	Adapted to FBM package.  Now takes FILE pointer instead of
 *	character name for output file.
 *
 * 13-Feb-89  David Rowley (mgardi@watdcsu.waterloo.edu)
 *	Created (sent by mail on 2/13/89)
 *	original name: GIFENCODE.C - GIF Image compression interface
 *
 *****************************************************************************/
 
#include <stdio.h>

/*
 * Pointer to function returning an int
 */
typedef int (* ifunptr)();

#define TRUE 1
#define FALSE 0

static int Width, Height;
static int curx, cury;
static long CountDown;
static int Pass = 0;
static int Interlace;

static Putword();

#ifndef lint
static char *fbmid =
"$FBM flgife.c <1.0> 25-Jun-90  (C) 1989,1990 by Michael Mauldin, source \
code available free from MLM@CS.CMU.EDU and from UUNET archives$";
#endif


/*
 * Bump the 'curx' and 'cury' to point to the next pixel
 */
static
BumpPixel()
{
	/*
	 * Bump the current X position
	 */
	curx++;

	/*
	 * If we are at the end of a scan line, set curx back to the beginning
	 * If we are interlaced, bump the cury to the appropriate spot,
	 * otherwise, just increment it.
	 */
	if( curx == Width ) {
		curx = 0;

	        if( !Interlace ) 
			cury++;
		else {
		     switch( Pass ) {
	     
	               case 0:
        	          cury += 8;
                	  if( cury >= Height ) {
		  		Pass++;
				cury = 4;
		  	  }
                          break;
		  
	               case 1:
        	          cury += 8;
                	  if( cury >= Height ) {
		  		Pass++;
				cury = 2;
		  	  }
			  break;
		  
	               case 2:
	                  cury += 4;
	                  if( cury >= Height ) {
	                     Pass++;
	                     cury = 1;
	                  }
	                  break;
			  
	               case 3:
	                  cury += 2;
	                  break;
			}
		}
	}
}

/*
 * Return the next pixel from the image
 */
GIFNextPixel( getpixel, voidStar )
ifunptr getpixel;
void *voidStar;
{
	int r;

	if( CountDown == 0 )
		return EOF;

	CountDown--;

	r = ( * getpixel )( curx, cury, voidStar );

	BumpPixel();

	return r;
}

#ifdef MAINTEST
int main_getpix( int x, int y, void *vstar) { return ((y*4)+x)/8; }

main() { 
int lut[4] = { 0, 85, 170, 255 };
GIFEncode( stdout, 4, 8, 0, 0, 2, lut, lut, lut, main_getpix, (void *) 0);
return 0;
}
#endif

/* public */

GIFEncode( wfile, GWidth, GHeight, GInterlace, Background, 
	   BitsPerPixel, Red, Green, Blue, GetPixel, voidStar )
	 
FILE *wfile;
int GWidth, GHeight;
int GInterlace;
int Background;
int BitsPerPixel;
int Red[], Green[], Blue[];
ifunptr GetPixel;
void *voidStar;

{
	FILE *fp;
	int B;
	int RWidth, RHeight;
	int LeftOfs, TopOfs;
	int Resolution;
	int ColorMapSize;
	int InitCodeSize;
	int i;

	Interlace = GInterlace;
	
	ColorMapSize = 1 << BitsPerPixel;
	
	RWidth = Width = GWidth;
	RHeight = Height = GHeight;
	LeftOfs = TopOfs = 0;
	
	Resolution = BitsPerPixel;

	/*
	 * Calculate number of bits we are expecting
	 */
	CountDown = (long)Width * (long)Height;

	/*
	 * Indicate which pass we are on (if interlace)
	 */
	Pass = 0;

	/*
	 * The initial code size
	 */
	if( BitsPerPixel <= 1 )
		InitCodeSize = 2;
	else
		InitCodeSize = BitsPerPixel;

	/*
	 * Set up the current x and y position
	 */
	curx = cury = 0;

	/*
	 * Open the GIF file for binary write
	 */
	/* fp = fopen( FName, "wb" ); */

	fp = wfile;	/* Change for FBM - mlm 2/19/89 */

	if( fp == (FILE *)0 ) {
		printf( "error: could not open output file\n" );
		return (0);
	}

	/*
	 * Write the Magic header
	 */
	fwrite( "GIF87a", 1, 6, fp );

	/*
	 * Write out the screen width and height
	 */
	Putword( RWidth, fp );
	Putword( RHeight, fp );

	/*
	 * Indicate that there is a global colour map
	 */
	B = 0x80;	/* Yes, there is a color map */

	/*
	 * OR in the resolution
	 */
	B |= (Resolution - 1) << 5;

	/*
	 * OR in the Bits per Pixel
	 */
	B |= (BitsPerPixel - 1);

	/*
	 * Write it out
	 */
	fputc( B, fp );

	/*
	 * Write out the Background colour
	 */
	fputc( Background, fp );

	/*
	 * Byte of 0s (future expansion)
	 */
	fputc( 0, fp );

	/*
	 * Write out the Global Colour Map
	 */
     	for( i=0; i<ColorMapSize; i++ ) {
		fputc( Red[i], fp );
		fputc( Green[i], fp );
		fputc( Blue[i], fp );
	}

	/*
	 * Write an Image separator
	 */
	fputc( ',', fp );

	/*
	 * Write the Image header
	 */

	Putword( LeftOfs, fp );
	Putword( TopOfs, fp );
	Putword( Width, fp );
	Putword( Height, fp );

	/*
	 * Write out whether or not the image is interlaced
	 */
	if( Interlace )
		fputc( 0x40, fp );
	else
		fputc( 0x00, fp );

	/*
	 * Write out the initial code size
	 */
	fputc( InitCodeSize, fp );

	/*
	 * Go and actually compress the data
	 */
	GIFcompress( InitCodeSize+1, fp, GetPixel, voidStar );

	/*
	 * Write out a Zero-length packet (to end the series)
	 */
	fputc( 0, fp );

	/*
	 * Write the GIF file terminator
	 */
	fputc( ';', fp );

	/*
	 * And close the file
	 */
	/* fclose( fp ); */
	
	return (1);	/* success - mlm 2/19/89 */
}

/*
 * Write out a word to the GIF file
 */
static
Putword( w, fp )
int w;
FILE *fp;
{
	fputc( w & 0xff, fp );
	fputc( (w / 256) & 0xff, fp );
}
