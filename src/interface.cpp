#include <tslib/tseries.hpp>
#include <tslib/vector.summary.hpp>
#include <tslib/vector.transform.hpp>

#include <R.tseries.data.backend.hpp>
#include <R.tseries.object.convert.hpp>
#include <Rsexp.allocator.templates.hpp>
#include "interface.hpp"
#include "r.window.template.hpp"
#include "r.transform.template.hpp"


SEXP movingMean(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Mean, meanTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Mean, meanTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

SEXP movingSum(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Sum, sumTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Sum, sumTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

SEXP movingProduct(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Prod, prodTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Prod, prodTraits>(x,periods);
  default:
    return R_NilValue;
  }
}


SEXP movingMax(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Max, maxTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Max, maxTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

SEXP movingMin(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Min, minTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Min, minTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

SEXP movingStdev(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Stdev, stdevTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Stdev, stdevTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

SEXP movingRank(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Rank, rankTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Rank, rankTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

SEXP expandingMax(SEXP x) {
  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<ExpandingMaximum, fillTraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<ExpandingMaximum, fillTraits>(x);
  default:
    return R_NilValue;
  }
}

SEXP expandingMin(SEXP x) {
  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<ExpandingMinimum, fillTraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<ExpandingMinimum, fillTraits>(x);
  default:
    return R_NilValue;
  }
}


SEXP movingCov(SEXP x, SEXP y, SEXP periods) {

  if(TYPEOF(x)!=TYPEOF(y)) {
    std::cerr << "movingCov: x and y must be the same type" << endl;
  }

  switch(TYPEOF(x)) {
  case REALSXP:
    return window_function<covTraits< Rtype<REALSXP>::ValueType >::ReturnType, Cov>(r_convert<REALSXP>::apply(x),
                                                                                    r_convert<REALSXP>::apply(y),
                                                                                    Rtype<INTSXP>::scalar(periods)).getIMPL()->R_object;
  case INTSXP:
    return window_function<covTraits< Rtype<INTSXP>::ValueType >::ReturnType, Cov>(r_convert<INTSXP>::apply(x),
                                                                                   r_convert<INTSXP>::apply(y),
                                                                                   Rtype<INTSXP>::scalar(periods)).getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP movingCor(SEXP x, SEXP y, SEXP periods) {

  if(TYPEOF(x)!=TYPEOF(y)) {
    std::cerr << "movingCor: x and y must be the same type" << endl;
  }

  switch(TYPEOF(x)) {
  case REALSXP:
    return window_function<corTraits< Rtype<REALSXP>::ValueType >::ReturnType, Cor>(r_convert<REALSXP>::apply(x),
                                                                                    r_convert<REALSXP>::apply(y),
                                                                                    Rtype<INTSXP>::scalar(periods)).getIMPL()->R_object;
  case INTSXP:
    return window_function<corTraits< Rtype<INTSXP>::ValueType >::ReturnType, Cor>(r_convert<INTSXP>::apply(x),
                                                                                   r_convert<INTSXP>::apply(y),
                                                                                   Rtype<INTSXP>::scalar(periods)).getIMPL()->R_object;
  default:
    return R_NilValue;
  }
}

SEXP sinceNA(SEXP x) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<SinceNA, SinceNATraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<SinceNA, SinceNATraits>(x);
  default:
    return R_NilValue;
  }
}


SEXP fillForward(SEXP x) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<FillFwd, fillTraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<FillFwd, fillTraits>(x);
  default:
    return R_NilValue;
  }
}

SEXP fillBackward(SEXP x) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<FillBwd, fillTraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<FillBwd, fillTraits>(x);
  default:
    return R_NilValue;
  }
}

SEXP fillValue(SEXP x, SEXP y) {

  switch(TYPEOF(x)) {
  case REALSXP:
    return r_transform_1arg<REALSXP>::apply<FillValue, fillTraits>(x, y);
  case INTSXP:
    return r_transform_1arg<INTSXP>::apply<FillValue, fillTraits>(x, y);
  default:
    return R_NilValue;
  }
}

SEXP lag(SEXP x, SEXP periods) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_transform_1arg<REALSXP>::apply<Lag, lagleadTraits>(x, periods);
  case INTSXP:
    return r_transform_1arg<INTSXP>::apply<Lag, lagleadTraits>(x, periods);
  default:
    return R_NilValue;
  }
}

SEXP lead(SEXP x, SEXP periods) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_transform_1arg<REALSXP>::apply<Lead, lagleadTraits>(x, periods);
  case INTSXP:
    return r_transform_1arg<INTSXP>::apply<Lead, lagleadTraits>(x, periods);
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
  default:
    return R_NilValue;
  }
}
