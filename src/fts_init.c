#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP diff(SEXP, SEXP);
extern SEXP ema(SEXP, SEXP);
extern SEXP expandingMax(SEXP);
extern SEXP expandingMin(SEXP);
extern SEXP fillBackward(SEXP);
extern SEXP fillForward(SEXP);
extern SEXP fillValue(SEXP, SEXP);
extern SEXP lead(SEXP, SEXP);
extern SEXP monthlySum(SEXP);
extern SEXP movingCor(SEXP, SEXP, SEXP);
extern SEXP movingCov(SEXP, SEXP, SEXP);
extern SEXP movingMax(SEXP, SEXP);
extern SEXP movingMean(SEXP, SEXP);
extern SEXP movingMin(SEXP, SEXP);
extern SEXP movingProduct(SEXP, SEXP);
extern SEXP movingRank(SEXP, SEXP);
extern SEXP movingStdev(SEXP, SEXP);
extern SEXP movingSum(SEXP, SEXP);
extern SEXP pad(SEXP, SEXP);
extern SEXP sinceNA(SEXP);
extern SEXP stopr(SEXP, SEXP, SEXP);
extern SEXP toDaily(SEXP);
extern SEXP toHourly(SEXP);
extern SEXP toMinute(SEXP);
extern SEXP toMonthly(SEXP);
extern SEXP toQuarterly(SEXP);
extern SEXP toSecond(SEXP);
extern SEXP toWeekly(SEXP);
extern SEXP toYearly(SEXP);
extern SEXP lag(SEXP, SEXP);
extern SEXP lead(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {{"diff", (DL_FUNC)&diff, 2},
                                              {"ema", (DL_FUNC)&ema, 2},
                                              {"expandingMax", (DL_FUNC)&expandingMax, 1},
                                              {"expandingMin", (DL_FUNC)&expandingMin, 1},
                                              {"fillBackward", (DL_FUNC)&fillBackward, 1},
                                              {"fillForward", (DL_FUNC)&fillForward, 1},
                                              {"fillValue", (DL_FUNC)&fillValue, 2},
                                              {"lead", (DL_FUNC)&lead, 2},
                                              {"monthlySum", (DL_FUNC)&monthlySum, 1},
                                              {"movingCor", (DL_FUNC)&movingCor, 3},
                                              {"movingCov", (DL_FUNC)&movingCov, 3},
                                              {"movingMax", (DL_FUNC)&movingMax, 2},
                                              {"movingMean", (DL_FUNC)&movingMean, 2},
                                              {"movingMin", (DL_FUNC)&movingMin, 2},
                                              {"movingProduct", (DL_FUNC)&movingProduct, 2},
                                              {"movingRank", (DL_FUNC)&movingRank, 2},
                                              {"movingStdev", (DL_FUNC)&movingStdev, 2},
                                              {"movingSum", (DL_FUNC)&movingSum, 2},
                                              {"pad", (DL_FUNC)&pad, 2},
                                              {"sinceNA", (DL_FUNC)&sinceNA, 1},
                                              {"stopr", (DL_FUNC)&stopr, 3},
                                              {"toDaily", (DL_FUNC)&toDaily, 1},
                                              {"toHourly", (DL_FUNC)&toHourly, 1},
                                              {"toMinute", (DL_FUNC)&toMinute, 1},
                                              {"toMonthly", (DL_FUNC)&toMonthly, 1},
                                              {"toQuarterly", (DL_FUNC)&toQuarterly, 1},
                                              {"toSecond", (DL_FUNC)&toSecond, 1},
                                              {"toWeekly", (DL_FUNC)&toWeekly, 1},
                                              {"toYearly", (DL_FUNC)&toYearly, 1},
                                              {"lag", (DL_FUNC)&lag, 2},
                                              {"lead", (DL_FUNC)&lead, 2},
                                              {NULL, NULL, 0}};

void R_init_fts(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
