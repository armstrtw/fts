#include <iostream>

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
  REprintf("%p\n",getAttrib(x,R_ClassSymbol));
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

SEXP pad(SEXP x, SEXP padDates) {
  double* dts = REAL(padDates);
  switch(TYPEOF(x)) {
    case REALSXP:
      return r_convert<REALSXP>::apply(x).pad(dts, dts + length(padDates)).getIMPL()->R_object;
    case INTSXP:
      return r_convert<INTSXP>::apply(x).pad(dts, dts + length(padDates)).getIMPL()->R_object;
    case LGLSXP:
      return r_convert<LGLSXP>::apply(x).pad(dts, dts + length(padDates)).getIMPL()->R_object;
    default:
      return R_NilValue;
  }
}

SEXP lag(SEXP x, SEXP periods_sexp) {
  R_len_t periods = Rtype<INTSXP>::scalar(periods_sexp);

  if(periods < 0) {
    Rprintf("only positive values of k are allowed.\n");
  }
  try {
    switch(TYPEOF(x)) {
    case REALSXP:
      return r_convert<REALSXP>::apply(x).lag(periods).getIMPL()->R_object;
    case INTSXP:
      return r_convert<INTSXP>::apply(x).lag(periods).getIMPL()->R_object;
    case LGLSXP:
      return r_convert<LGLSXP>::apply(x).lag(periods).getIMPL()->R_object;
    default:
      return R_NilValue;
    }
  } catch(TSeriesError& e) {
    Rprintf("%s\n",e.what());
    return R_NilValue;
  }
}

SEXP lead(SEXP x, SEXP periods_sexp) {
  R_len_t periods = Rtype<INTSXP>::scalar(periods_sexp);

  if(periods < 0) {
    Rprintf("only positive values of k are allowed.\n");
  }
  try {
    switch(TYPEOF(x)) {
    case REALSXP:
      return r_convert<REALSXP>::apply(x).lead(periods).getIMPL()->R_object;
    case INTSXP:
      return r_convert<INTSXP>::apply(x).lead(periods).getIMPL()->R_object;
    case LGLSXP:
      return r_convert<LGLSXP>::apply(x).lead(periods).getIMPL()->R_object;
    default:
      return R_NilValue;
    }
  } catch(TSeriesError& e) {
    Rprintf("%s\n",e.what());
    return R_NilValue;
  }
}

SEXP diff(SEXP x, SEXP periods_sexp) {
  R_len_t periods = Rtype<INTSXP>::scalar(periods_sexp);

  if(periods < 0) {
    Rprintf("only positive values of k are allowed.\n");
  }
  try {
    switch(TYPEOF(x)) {
    case REALSXP:
      return r_convert<REALSXP>::apply(x).diff(periods).getIMPL()->R_object;
    case INTSXP:
      return r_convert<INTSXP>::apply(x).diff(periods).getIMPL()->R_object;
    case LGLSXP:
      return r_convert<LGLSXP>::apply(x).diff(periods).getIMPL()->R_object;
    default:
      return R_NilValue;
    }
  } catch(TSeriesError& e) {
    Rprintf("%s\n",e.what());
    return R_NilValue;
  }
}

SEXP movingCov(SEXP x, SEXP y, SEXP periods) {
  return windowSpecializer_2args<Cov,covTraits>(x,y,periods);
}

SEXP movingCor(SEXP x, SEXP y, SEXP periods) {
  return windowSpecializer_2args<Cor,corTraits>(x,y,periods);
}

SEXP dailySum(SEXP x) {
  return timeWindowSpecializer<Sum, sumTraits, yyyymmdd>(x);
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

SEXP toYearly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyy>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyy>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyy>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toQuarterly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyyqq>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyyqq>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyyqq>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toMonthly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyymm>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyymm>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyymm>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toWeekly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyyww>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyyww>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyyww>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toDaily(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyymmdd>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyymmdd>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyymmdd>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toHourly(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyymmddHH>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyymmddHH>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyymmddHH>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toMinute(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyymmddHHMM>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyymmddHHMM>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyymmddHHMM>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP toSecond(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_convert<REALSXP>::apply(x).freq<yyyymmddHHMMSS>().getIMPL()->R_object;
  case INTSXP:
    return r_convert<INTSXP>::apply(x).freq<yyyymmddHHMMSS>().getIMPL()->R_object;
  case LGLSXP:
    return r_convert<LGLSXP>::apply(x).freq<yyyymmddHHMMSS>().getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP ema(SEXP x, SEXP periods) {
  return transformSpecializer_1arg<EMA, emaTraits>(x, periods);
}
