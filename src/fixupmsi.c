/* 
 * add times in lines that don't have them for week#.msi files
 * e.g. for files that have lines like this:
 *
 * 323 03:39:59   86857008 0 MSI     Automatic exposure image take...
 *                           MSI     Quaterion: 0.592720 -0.527217...
 * 323 03:44:22   86857271 0 MSI     Automatic exposure image take...
 *                           MSI     Quaterion: 0.592719 -0.527209...
 * 323 03:48:45   86857534 0 MSI     Automatic exposure image take...
 *                           MSI     Quaterion: 0.592718 -0.527201...
 *
 * find " MSI " in each line, if next line has spaces up to that 
 * same position and has same text there, then copy previous line 
 * up to that position
 */

#include <stdio.h>
#include <string.h>

int
main() {
char currLine[1024];
char lastLine[1024];
char *cPtr, *lPtr;
#define MATCHSTRING " MSI "
int matchLen = strlen( MATCHSTRING);

  /* fill lastLine with spaces
   * set lPtr (pos'n of MATCHSTRING in prev line) to null
   */ 
  for ( cPtr = lastLine; (cPtr-lastLine) < 1024; ++cPtr) *cPtr = ' ';
  lPtr = (char *) 0;

  /* for each line */
  while ( fgets( currLine, 1024, stdin)) {

    if ( cPtr = strchr( currLine, '\n')) *cPtr = '\0';      /* null terminate */

    /* check for leading spaces up to position of last MATCHSTRING */

    for ( cPtr=currLine; *cPtr == ' ' && cPtr < lPtr; cPtr++) ;

    /* if current line has all leading spaces up to position of previous line's
     *  MATCHSTRING  && also has MATCHSTRING at that point, 
     * then copy last line to fill in leading spaces in current line
     */
    /*
    fprintf( stderr, "cPtr, lPtr = %08xx %08xx\n", (long) cPtr, (long) lPtr);
    fprintf( stderr, "currline = >%.60s ...<\n", currLine);
    fprintf( stderr, "lastline = >%.60s ...<\n", lastLine);
    */
    if ( cPtr == lPtr) if ( !strncmp( cPtr, MATCHSTRING, matchLen)) {
      strncpy( currLine, lastLine, (size_t) (cPtr - currLine));
    }

    fprintf( stdout, "%s\n", currLine);    /* print out (poss. modified) line */
    lPtr = strstr( currLine, MATCHSTRING);                /* find MATCHSTRING */
    strcpy( lastLine, currLine);                         /* save current line */
  }
  return 0;
}
