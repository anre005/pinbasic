#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _pinbasic_eho(SEXP, SEXP, SEXP);
extern SEXP _pinbasic_linke(SEXP, SEXP, SEXP);
extern SEXP _pinbasic_simulateBS(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_pinbasic_eho",   (DL_FUNC) &_pinbasic_eho,   3},
  {"_pinbasic_linke", (DL_FUNC) &_pinbasic_linke, 3},
  {"_pinbasic_simulateBS", (DL_FUNC) &_pinbasic_simulateBS, 2},
  {NULL, NULL, 0}
};

void R_init_pinbasic(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
