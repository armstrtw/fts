#include <tslib/tseries.hpp>
#include <tslib/vector.summary.hpp>
#include <tslib/vector.transform.hpp>

#include <R.tseries.data.backend.hpp>
#include <R.tseries.object.convert.hpp>
#include <Rsexp.allocator.templates.hpp>
#include "interface.h"
#include "r.window.template.h"
#include "r.transform.template.h"
#include "analog.h"


SEXP movingMean(SEXP x, SEXP periods) {
  return windowSpecializer<Mean, meanTraits>(x,periods);
}

SEXP movingSum(SEXP x, SEXP periods) {
  return windowSpecializer<Sum, sumTraits>(x,periods);
}

SEXP movingProduct(SEXP x, SEXP periods) {
  return windowSpecializer<Prod, prodTraits>(x,periods);
}

SEXP movingMax(SEXP x, SEXP periods) {
  return windowSpecializer<Max, maxTraits>(x,periods);
}

SEXP movingMin(SEXP x, SEXP periods) {
    return windowSpecializer<Min, minTraits>(x,periods);
}

SEXP movingStdev(SEXP x, SEXP periods) {
    return windowSpecializer<Stdev, stdevTraits>(x,periods);
}

SEXP movingRank(SEXP x, SEXP periods) {
  return windowSpecializer<Rank, rankTraits>(x,periods);
}

SEXP expandingMax(SEXP x) {
  return transformSpecializer<ExpandingMaximum, fillTraits>(x);
}

SEXP expandingMin(SEXP x) {
  return transformSpecializer<ExpandingMinimum, fillTraits>(x);
}

SEXP sinceNA(SEXP x) {
  return transformSpecializer<SinceNA, SinceNATraits>(x);
}

SEXP fillForward(SEXP x) {
  return transformSpecializer<FillFwd, fillTraits>(x);
}

SEXP fillBackward(SEXP x) {
  return transformSpecializer<FillBwd, fillTraits>(x);
}

SEXP fillValue(SEXP x, SEXP y) {
  return transformSpecializer_1arg<FillValue, fillTraits>(x, y);
}

SEXP lag(SEXP x, SEXP periods) {
  return transformSpecializer_1arg<Lag, lagleadTraits>(x, periods);
}

SEXP lead(SEXP x, SEXP periods) {
  return transformSpecializer_1arg<Lead, lagleadTraits>(x, periods);
}

SEXP movingCov(SEXP x, SEXP y, SEXP periods) {
  return windowSpecializer_2args<Cov,covTraits>(x,y,periods);
}

SEXP movingCor(SEXP x, SEXP y, SEXP periods) {
  return windowSpecializer_2args<Cov,covTraits>(x,y,periods);
}

SEXP monthlySum(SEXP x) {
  return timeWindowSpecializer<Sum, sumTraits, yyyymm>(x);
}

SEXP analog(SEXP x, SEXP y, SEXP periods) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return analogFunction<REALSXP>::apply(x,y,periods);
  case INTSXP:
    return analogFunction<INTSXP>::apply(x,y,periods);
  case LGLSXP:
    return analogFunction<LGLSXP>::apply(x,y,periods);
  default:
    return R_NilValue;
  }
}

SEXP toQuarterly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).toQuarterly().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).toQuarterly().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).toQuarterly().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toMonthly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).toMonthly().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).toMonthly().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).toMonthly().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toWeekly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).toWeekly().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).toWeekly().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).toWeekly().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}
