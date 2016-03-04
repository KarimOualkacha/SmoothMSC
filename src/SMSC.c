/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2012	The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

static double dokern(double x, int kern)
{
    if(kern == 1) return(1.0);
    if(kern == 2) return(exp(-0.5*x*x));
    return(0.0); /* -Wall */
}

static void BDRksmooth2(double *x, double *y, R_xlen_t n,
		       double *xp, double *yp, R_xlen_t np,
		       int kern, double *bw)
{
    R_xlen_t imin = 0;
    double cutoff = 0.0, num, den, x0, w, bwf;
    
    
    /* bandwidth is in units of half inter-quartile range. */
    for(R_xlen_t j = 0; j < np; j++) {
        
        /*cutoff = 0.0; imin = 0;*/
        bwf = bw[j];
       if(kern == 1) {bwf *= 0.5; cutoff = bwf;}
       if(kern == 2) {bwf *= 0.3706506; cutoff = 4*bwf;}
       if (j == 0) { while(x[imin] < xp[0] - cutoff && imin < n) imin++;};
	   num = den = 0.0;
	   x0 = xp[j];
	   for(R_xlen_t i = imin; i < n; i++) {
	       if(x[i] < x0 - cutoff) imin = i;
	       else {
		   if(x[i] > x0 + cutoff) break;
		   
		   w = dokern(fabs(x[i] - x0)/bwf, kern);
		   num += w*y[i];
		   den += w;
	       }
	   }
	   if(den > 0) yp[j] = num/den; else yp[j] = NA_REAL;
    }
}

SEXP ksmooth2(SEXP x, SEXP y, SEXP xp, SEXP skrn, SEXP bandVar)
{
    int krn = asInteger(skrn);
    bandVar = PROTECT(coerceVector(bandVar, REALSXP));
    x = PROTECT(coerceVector(x, REALSXP));
    y = PROTECT(coerceVector(y, REALSXP));
    xp = PROTECT(coerceVector(xp, REALSXP));
    R_xlen_t nx = XLENGTH(x), np = XLENGTH(xp);
    SEXP yp = PROTECT(allocVector(REALSXP, np));
    BDRksmooth2(REAL(x), REAL(y), nx, REAL(xp), REAL(yp), np, krn, REAL(bandVar));
    SEXP ans = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, xp);
    SET_VECTOR_ELT(ans, 1, yp);
    SEXP nm = allocVector(STRSXP, 2);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("x"));
    SET_STRING_ELT(nm, 1, mkChar("y"));
    UNPROTECT(6);
    return ans;
}


