#include <tslib/tseries.hpp>
#include <tslib/vector.summary.hpp>
#include <tslib/vector.transform.hpp>

#include "interface.hpp"
#include "R.tseries.data.backend.hpp"
#include "sexp.allocator.templates.hpp"
#include "r.window.template.hpp"
#include "r.transform.template.hpp"

SEXP movingMean(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Mean, meanTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Mean, meanTraits>(x,periods);
  }
}

SEXP movingSum(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Sum, sumTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Sum, sumTraits>(x,periods);
  }
}


SEXP movingMax(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Max, maxTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Max, maxTraits>(x,periods);
  }
}

SEXP movingMin(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Min, minTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Min, minTraits>(x,periods);
  }
}

SEXP movingStdev(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Stdev, stdevTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Stdev, stdevTraits>(x,periods);
  }
}

SEXP movingRank(SEXP x, SEXP periods) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_window<REALSXP>::apply<Rank, rankTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<Rank, rankTraits>(x,periods);
  }
}

SEXP fillForward(SEXP x) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<FillFwd, fillTraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<FillFwd, fillTraits>(x);
  }
}

SEXP fillBackward(SEXP x) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform<REALSXP>::apply<FillBwd, fillTraits>(x);
  case INTSXP:
    return r_transform<INTSXP>::apply<FillBwd, fillTraits>(x);
  }
}

SEXP fillValue(SEXP x, SEXP y) {

  switch(TYPEOF(x)) {

  case REALSXP:
    return r_transform_1arg<REALSXP>::apply<FillValue, fillTraits>(x, y);
  case INTSXP:
    return r_transform_1arg<INTSXP>::apply<FillValue, fillTraits>(x, y);
  }
}
