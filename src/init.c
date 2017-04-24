#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP trackdem_cb(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP trackdem_getCoords(SEXP, SEXP);
extern SEXP trackdem_muP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP trackdem_sb(SEXP, SEXP, SEXP, SEXP);
extern SEXP trackdem_sdP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"trackdem_cb",        (DL_FUNC) &trackdem_cb,        5},
    {"trackdem_getCoords", (DL_FUNC) &trackdem_getCoords, 2},
    {"trackdem_muP",       (DL_FUNC) &trackdem_muP,       6},
    {"trackdem_sb",        (DL_FUNC) &trackdem_sb,        4},
    {"trackdem_sdP",       (DL_FUNC) &trackdem_sdP,       6},
    {NULL, NULL, 0}
};

void R_init_trackdem(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
