#ifndef TIME_WINDOW_TEMPLATE_HPP
#define TIME_WINDOW_TEMPLATE_HPP

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
         template<class> class windowFunctionTraits,
         template<class, template<typename> class> class PFUNC>
SEXP timeWindowFun(SEXP x) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename windowFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template time_window<ReturnTDATA,windowFunction,PFUNC>();
  return ans.getIMPL()->R_object;
}

template<template<class> class windowFunction, template<class> class windowFunctionTraits,template<class, template<typename> class> class PFUNC>
SEXP timeWindowSpecializer(SEXP x) {
  const TsTypeTuple tsTypeInfo(x);
  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return timeWindowFun<double,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return timeWindowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return timeWindowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return timeWindowFun<int,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return timeWindowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return timeWindowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return timeWindowFun<double,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return timeWindowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return timeWindowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return timeWindowFun<int,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return timeWindowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return timeWindowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("timeWindowSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}

#endif // TIME_WINDOW_TEMPLATE_HPP
