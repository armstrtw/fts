// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; tab-width: 8 -*-
#ifndef WINDOW_INTERSECTION_TEMPLATE_HPP
#define WINDOW_INTERSECTION_TEMPLATE_HPP

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
SEXP windowFun(SEXP x_, SEXP y_, SEXP periods) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename windowFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  int p = Rtype<INTSXP>::scalar(periods);
  if(p <= 0) {
    REprintf("windowFun: periods is not positive.");
    return R_NilValue;
  }
  // build tseries from SEXP x_
  TSDATABACKEND<TDATE,TDATA,TSDIM> x_backend(x_);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> x(x_backend);

  // build tseries from SEXP y_
  TSDATABACKEND<TDATE,TDATA,TSDIM> y_backend(y_);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> y(y_backend);

  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = window_function<ReturnTDATA,windowFunction>(x,y,p);
  return ans.getIMPL()->R_object;
}

template<template<class> class windowFunction, template<class> class windowFunctionTraits>
SEXP windowSpecializer(SEXP x, SEXP y, SEXP periods) {
  if(TYPEOF(periods)!=INTSXP) {
    REprintf("windowSpecializer: periods is not an integer.");
  };
  const TsTypeTuple tsTypeInfoX(x);
  const TsTypeTuple tsTypeInfoY(y);
  if(tsTypeInfoX!=tsTypeInfoY) {
    REprintf("windowSpecializer_2args: x and y must be same time series types.");
    return R_NilValue;
  }

  if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("windowSpecializer_2args: unable to classify time series.");
    return R_NilValue;
  }
}

#endif // WINDOW_INTERSECTION_TEMPLATE_HPP
