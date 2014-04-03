#define R_NO_REMAP
#include <Rinternals.h>
#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rsexp.allocator.templates.hpp>
#include <Rtype.hpp>
#include <fts.factory.hpp>

using namespace tslib;


template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy>
SEXP diffFun(SEXP x, SEXP periods) {
  int p = Rtype<INTSXP>::scalar(periods);
  if(p <= 0) {
    REprintf("diffFun: periods is not positive.");
    return R_NilValue;
  }
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template diff(p);
  return ans.getIMPL()->R_object;
}

SEXP diffSpecializer(SEXP x, SEXP p) {
  const TsTypeTuple tsTypeInfo(x);

  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return diffFun<double,double,R_len_t,JulianBackend,JulianDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return diffFun<double,int,R_len_t,JulianBackend,JulianDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return diffFun<double,int,R_len_t,JulianBackend,JulianDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return diffFun<int,double,R_len_t,JulianBackend,JulianDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return diffFun<int,int,R_len_t,JulianBackend,JulianDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return diffFun<int,int,R_len_t,JulianBackend,JulianDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return diffFun<double,double,R_len_t,PosixBackend,PosixDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return diffFun<double,int,R_len_t,PosixBackend,PosixDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return diffFun<double,int,R_len_t,PosixBackend,PosixDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return diffFun<int,double,R_len_t,PosixBackend,PosixDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return diffFun<int,int,R_len_t,PosixBackend,PosixDate>(x,p);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return diffFun<int,int,R_len_t,PosixBackend,PosixDate>(x,p);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("diffSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}

extern "C" SEXP diff(SEXP x, SEXP periods) {
  return diffSpecializer(x,periods);
}

