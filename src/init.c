#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP pinbasic_eho(SEXP, SEXP, SEXP);
extern SEXP pinbasic_linke(SEXP, SEXP, SEXP);
extern SEXP pinbasic_simBS(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"pinbasic_eho",   (DL_FUNC) &pinbasic_eho,   3},
  {"pinbasic_linke", (DL_FUNC) &pinbasic_linke, 3},
  {"pinbasic_simBS", (DL_FUNC) &pinbasic_simBS, 2},
  {NULL, NULL, 0}
};

void R_init_pinbasic(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
