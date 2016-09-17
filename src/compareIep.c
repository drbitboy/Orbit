#include <math.h>
#include <stdio.h>
extern char *_XmVersionString;
main( int argc, char **argv) {
FILE *f1 = fopen(argv[1],"r");
FILE *f2 = fopen(argv[2],"r");
int i1, i2;
char s1[256];
char s2[256];
double d1, d2;
double err = 0.0;
double newerr;
double denom;
char sav1[256];
char sav2[256];
long count = 0;
long counterr = -1;

#define MAX(A,B) ((A)>(B)) ? (A) : (B)

  *s1 = *s2 = '\0';
  i1=fscanf(f1,"%s",s1);
  i2=fscanf(f2,"%s",s2);
  while ( i1 == 1 && i2 == 1 ) {
    i1=sscanf(s1,"%lf",&d1);
    i2=sscanf(s2,"%lf",&d2);
    if ( i1 == 1 && i2 == 1) {
      denom = MAX(fabs(d1),fabs(d2));
      newerr = ( denom > 0.0) ? (fabs(d1-d2)/denom) : 0.0;
      if ( newerr > err) {
        err = newerr;
        strcpy( sav1, s1);
        strcpy( sav2, s2);
        counterr = count;
      }
    } else {
      if ( i1 || i2) {
        printf("inside:  i1/s1/i2/s2=%d/%s/%d/%s/\n", i1, s1, i2, s2);
      }
    }
    *s1 = *s2 = '\0';
    count++;
    i1=fscanf(f1,"%s",s1);
    i2=fscanf(f2,"%s",s2);
  }
  fclose(f1);
  fclose(f2);
  if ( i1 != i2 ) {
    printf("end:  i1/s1/i2/s2=%d/%s/%d/%s/\n", i1, s1, i2, s2);
  }
  printf( "%ld = count\n", count);
  printf( "%.9lg = max frac error\ns1/s2/count=%s/%s/%ld/\n"
        , err, s1, s2, counterr);
  return 0;
}
