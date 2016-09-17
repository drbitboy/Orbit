#include <math.h>

#include <sys/time.h>

/*
#ifndef sun
#include <time.h>
#else
#include <sys/time.h>
#endif
/**/
#include <stdio.h>

#ifdef DEBUG
#define DPR(A) fprintf A , fflush(stderr)
#else
#define DPR(A)
#endif

/* 	newspud sub.for spud subroutines */
/* 	12-1 for fine grid */
/* 	7-1991 btcarcich performance, misc, additions */

/* *********************************************************************** */
/* sort an index to an array - verbatim from Numerical Recipes */

/* Subroutine */ int indexx(n, arrin, indx)
long int *n;
double *arrin;
long int *indx;
{
    /* System generated locals */
    long int i__1;

    /* Local variables */
    static long int i, j, l;
    static double q;
    static long int ir, indxt;

/* DPR((stderr," entering indexx ... // ")); /**/

/* Parameter adjustments */
    --indx;
    --arrin;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	indx[j] = j;
/* L11: */
    }
    if (*n == 1) {
	indx[1]=indx[1]-1;
	/* DPR((stderr, " leaving indexx at 1 // ")); /**/
        return 0;
    }
    l = *n / 2 + 1;
    ir = *n;

  while (1) {
    if (l > 1) {
	--l;
	indxt = indx[l];
	q = arrin[indxt];
    } else {
	indxt = indx[ir];
	q = arrin[indxt];
	indx[ir] = indx[1];
	--ir;
	if (ir == 1) {
	    indx[1] = indxt;
	    for(j=1;j<= i__1; j++) indx[j]--;
	    /* DPR((stderr, " leaving indexx at 2 // ")); /**/
	    return 0;
	}
    }
    i = l;
    j = l + l;
/* L20: 
    if (j <= ir) { */

    while (j <= ir) {
	if (j < ir) {
	    if (arrin[indx[j]] < arrin[indx[j + 1]]) {
		++j;
	    }
	}
	if (q < arrin[indx[j]]) {
	    indx[i] = indx[j];
	    i = j;
	    j += j;
	} else {
	    j = ir + 1;
	}
/*	goto L20; */
    }
    indx[i] = indxt;

  } /* while(1) */
  /* DPR((stderr, " leaving indexx at huh? // ")); /**/
} /* indexx_ */

/***************************************/

#ifdef vms

double seknds( oldt) double oldt; {
double newt;
unsigned long t[2];
int sys$gettim();

  sys$gettim( t);

  newt = ( t[1] * pow(2.,32.) + t[0]) * 1e-7;
  return( newt - oldt);
}

#else /*!vms*/

double seknds( old) double old; { 
struct timeval lcltime;
  gettimeofday( &lcltime, (struct timezone *) 0);
  old = (lcltime.tv_sec + 1e-6 * lcltime.tv_usec) - old;
  return( old);
}

#endif /*!vms*/
