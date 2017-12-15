#define R_NO_REMAP
#include <R.tseries.data.backend.hpp>
#include <Rinternals.h>
#include <Rsexp.allocator.templates.hpp>
#include <Rtype.hpp>
#include <fts.factory.hpp>
#include <tslib/tseries.hpp>

using namespace tslib;

template <typename TDATE, typename TDATA, typename TSDIM, template <typename, typename, typename> class TSDATABACKEND,
          template <typename> class DatePolicy>
SEXP stoprFun(SEXP x, SEXP level_, SEXP periods_) {
  double level = Rtype<REALSXP>::scalar(level_);
  int p        = Rtype<INTSXP>::scalar(periods_);
  if (level >= 0) {
    REprintf("level: periods is not < 0.");
    return R_NilValue;
  }

  if (p < 0) {
    REprintf("lagFun: periods is not >= 0.");
    return R_NilValue;
  }
  // build tseries from SEXP x
  TSDATABACKEND<TDATE, TDATA, TSDIM> tsData(x);
  TSeries<TDATE, TDATA, TSDIM, TSDATABACKEND, DatePolicy> ts(tsData);
  TSeries<TDATE, TDATA, TSDIM, TSDATABACKEND, DatePolicy> ans(tsData.nrow(), tsData.ncol());
  std::copy(ts.getDates(), ts.getDates() + ts.nrow(), ans.getDates());

  int stop_count_down(0);
  double cummax(0), cumpnl(0), dd(0);
  for (TSDIM i = 0; i < ts.nrow(); ++i) {

    // if we are not stopped then increment
    // otherwise keep
    if (stop_count_down == 0) {
      // increment cumpnl
      cumpnl += ts(i, 0);
      ans(i, 0) = ts(i, 0);
    } else {
      --stop_count_down;
      ans(i, 0) = 0;
    }

    // test/set cummax
    if (cumpnl > cummax) { cummax = cumpnl; }
    // calc drawdown
    dd = cumpnl - cummax;

    // if dd exceeds level (to the downside), then set the stop day couter
    // pnl will be set to 0 starting tomorrow (i.e. preserve the stopout loss for today)
    // reset pnls, so we don't immediately stop out again upon resumption of trading
    // i.e. if 1st day of new trading is a loss, then stop out happens again
    if (dd < level) {
      stop_count_down = p;
      cumpnl          = 0;
      cummax          = 0;
    }
  }

  return ans.getIMPL()->Robject;
}

SEXP stoprSpecializer(SEXP x, SEXP level, SEXP p) {
  const TsTypeTuple tsTypeInfo(x);

  if (tsTypeInfo.dateSEXPTYPE == REALSXP && tsTypeInfo.dataSEXPTYPE == REALSXP && tsTypeInfo.datePolicy == dateT) {
    return stoprFun<double, double, R_len_t, JulianBackend, JulianDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == REALSXP && tsTypeInfo.dataSEXPTYPE == INTSXP && tsTypeInfo.datePolicy == dateT) {
    return stoprFun<double, int, R_len_t, JulianBackend, JulianDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == REALSXP && tsTypeInfo.dataSEXPTYPE == LGLSXP && tsTypeInfo.datePolicy == dateT) {
    return stoprFun<double, int, R_len_t, JulianBackend, JulianDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == INTSXP && tsTypeInfo.dataSEXPTYPE == REALSXP && tsTypeInfo.datePolicy == dateT) {
    return stoprFun<int, double, R_len_t, JulianBackend, JulianDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == INTSXP && tsTypeInfo.dataSEXPTYPE == INTSXP && tsTypeInfo.datePolicy == dateT) {
    return stoprFun<int, int, R_len_t, JulianBackend, JulianDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == INTSXP && tsTypeInfo.dataSEXPTYPE == LGLSXP && tsTypeInfo.datePolicy == dateT) {
    return stoprFun<int, int, R_len_t, JulianBackend, JulianDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == REALSXP && tsTypeInfo.dataSEXPTYPE == REALSXP && tsTypeInfo.datePolicy == posixT) {
    return stoprFun<double, double, R_len_t, PosixBackend, PosixDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == REALSXP && tsTypeInfo.dataSEXPTYPE == INTSXP && tsTypeInfo.datePolicy == posixT) {
    return stoprFun<double, int, R_len_t, PosixBackend, PosixDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == REALSXP && tsTypeInfo.dataSEXPTYPE == LGLSXP && tsTypeInfo.datePolicy == posixT) {
    return stoprFun<double, int, R_len_t, PosixBackend, PosixDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == INTSXP && tsTypeInfo.dataSEXPTYPE == REALSXP && tsTypeInfo.datePolicy == posixT) {
    return stoprFun<int, double, R_len_t, PosixBackend, PosixDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == INTSXP && tsTypeInfo.dataSEXPTYPE == INTSXP && tsTypeInfo.datePolicy == posixT) {
    return stoprFun<int, int, R_len_t, PosixBackend, PosixDate>(x, level, p);
  } else if (tsTypeInfo.dateSEXPTYPE == INTSXP && tsTypeInfo.dataSEXPTYPE == LGLSXP && tsTypeInfo.datePolicy == posixT) {
    return stoprFun<int, int, R_len_t, PosixBackend, PosixDate>(x, level, p);
  } else {
    REprintf("stoprFun: unable to classify time series.");
    return R_NilValue;
  }
}

extern "C" SEXP stopr(SEXP x, SEXP level, SEXP periods) { return stoprSpecializer(x, level, periods); }
