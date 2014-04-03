#ifndef WINDOW_TEMPLATE_HPP
#define WINDOW_TEMPLATE_HPP

#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rtype.hpp>
#include <fts.factory.hpp>

using namespace tslib;

template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy,
         template<class> class windowFunction,
         template<class> class windowFunctionTraits>
SEXP windowFun(SEXP x, SEXP periods) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename windowFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  int p = Rtype<INTSXP>::scalar(periods);
  if(p <= 0) {
    REprintf("windowFun: periods is not positive.");
    return R_NilValue;
  }
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template window<ReturnTDATA,windowFunction>(p);
  return ans.getIMPL()->Robject;
}

template<template<class> class windowFunction, template<class> class windowFunctionTraits>
SEXP windowSpecializer(SEXP x, SEXP periods) {
  if(TYPEOF(periods)!=INTSXP) {
    REprintf("windowSpecializer: periods is not an integer.");
  };
  const TsTypeTuple tsTypeInfo(x);
  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return windowFun<double,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return windowFun<int,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return windowFun<double,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return windowFun<int,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("windowSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}
#endif // WINDOW_TEMPLATE_HPP
