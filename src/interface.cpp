#include <iostream>

#include <tslib/tseries.hpp>
#include <tslib/vector.summary.hpp>
#include <tslib/vector.transform.hpp>

#include <R.tseries.data.backend.hpp>
#include <R.tseries.object.convert.hpp>
#include <Rsexp.allocator.templates.hpp>
#include <r.window.template.h>
#include <r.transform.template.h>

extern "C" SEXP movingMean(SEXP x, SEXP periods) {
  return windowSpecializer<Mean, meanTraits>(x,periods);
}

extern "C" SEXP movingSum(SEXP x, SEXP periods) {
  return windowSpecializer<Sum, sumTraits>(x,periods);
}

extern "C" SEXP movingProduct(SEXP x, SEXP periods) {
  return windowSpecializer<Prod, prodTraits>(x,periods);
}

extern "C" SEXP movingMax(SEXP x, SEXP periods) {
  return windowSpecializer<Max, maxTraits>(x,periods);
}

extern "C" SEXP movingMin(SEXP x, SEXP periods) {
    return windowSpecializer<Min, minTraits>(x,periods);
}

extern "C" SEXP movingStdev(SEXP x, SEXP periods) {
    return windowSpecializer<Stdev, stdevTraits>(x,periods);
}

extern "C" SEXP movingRank(SEXP x, SEXP periods) {
  return windowSpecializer<Rank, rankTraits>(x,periods);
}

extern "C" SEXP expandingMax(SEXP x) {
  return transformSpecializer<ExpandingMaximum, fillTraits>(x);
}

extern "C" SEXP expandingMin(SEXP x) {
  return transformSpecializer<ExpandingMinimum, fillTraits>(x);
}

extern "C" SEXP sinceNA(SEXP x) {
  return transformSpecializer<SinceNA, SinceNATraits>(x);
}

extern "C" SEXP fillForward(SEXP x) {
  return transformSpecializer<FillFwd, fillTraits>(x);
}

extern "C" SEXP fillBackward(SEXP x) {
  return transformSpecializer<FillBwd, fillTraits>(x);
}

extern "C" SEXP fillValue(SEXP x, SEXP y) {
  return transformSpecializer<FillValue, fillTraits>(x, y);
}

extern "C" SEXP movingCov(SEXP x, SEXP y, SEXP periods) {
  return windowSpecializer<Cov,covTraits>(x,y,periods);
}

extern "C" SEXP movingCor(SEXP x, SEXP y, SEXP periods) {
  return windowSpecializer<Cor,corTraits>(x,y,periods);
}

extern "C" SEXP dailySum(SEXP x) {
  return timeWindowSpecializer<Sum, sumTraits, yyyymmdd>(x);
}

extern "C" SEXP monthlySum(SEXP x) {
  return timeWindowSpecializer<Sum, sumTraits, yyyymm>(x);
}

extern "C" SEXP toYearly(SEXP x) { return freqSpecializer<yyyy>(x); }
extern "C" SEXP toQuarterly(SEXP x) { return freqSpecializer<yyyyqq>(x); }
extern "C" SEXP toMonthly(SEXP x) { return freqSpecializer<yyyymm>(x); }
extern "C" SEXP toWeekly(SEXP x) { return freqSpecializer<yyyyww>(x); }
extern "C" SEXP toDaily(SEXP x) { return freqSpecializer<yyyymmdd>(x); }
extern "C" SEXP toHourly(SEXP x) { return freqSpecializer<yyyymmddHH>(x); }
extern "C" SEXP toMinute(SEXP x) { return freqSpecializer<yyyymmddHHMM>(x); }
extern "C" SEXP toSecond(SEXP x) { return freqSpecializer<yyyymmddHHMMSS>(x);}  

extern "C" SEXP ema(SEXP x, SEXP periods) {
  return transformSpecializer<EMA, emaTraits>(x, periods);
}
