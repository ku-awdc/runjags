#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h> // for SEXP

extern void testrunjags(int *disttype, int *dpqr, int *uselog, int *lower, int *N, double *x, int *npars, double *parameters, double *values, int *status);

static R_NativePrimitiveArgType testrunjags_t[] = {
    INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP, INTSXP
};

static const R_CMethodDef cMethods[] = {
    {"testrunjags", (DL_FUNC) &testrunjags, 10, testrunjags_t},
    {NULL, NULL, 0, NULL}
};

void
R_init_runjags(DllInfo *dll)
{
    R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
