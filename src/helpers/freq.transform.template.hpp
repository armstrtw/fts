#ifndef FREQ_TRANSFORM_TEMPLATE_HPP
#define FREQ_TRANSFORM_TEMPLATE_HPP

#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rsexp.allocator.templates.hpp>
#include <Rtype.hpp>
#include <fts.factory.hpp>

using namespace tslib;

template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy,
         template<class, template<typename> class> class PFUNC>
SEXP freqFun(SEXP x) {
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template freq<PFUNC>();
  return ans.getIMPL()->R_object;
}

template<template<class, template<typename> class> class PFUNC>
SEXP freqSpecializer(SEXP x) {
  const TsTypeTuple tsTypeInfo(x);

  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return freqFun<double,double,R_len_t,JulianBackend,JulianDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return freqFun<double,int,R_len_t,JulianBackend,JulianDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return freqFun<double,int,R_len_t,JulianBackend,JulianDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return freqFun<int,double,R_len_t,JulianBackend,JulianDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return freqFun<int,int,R_len_t,JulianBackend,JulianDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return freqFun<int,int,R_len_t,JulianBackend,JulianDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return freqFun<double,double,R_len_t,PosixBackend,PosixDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return freqFun<double,int,R_len_t,PosixBackend,PosixDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return freqFun<double,int,R_len_t,PosixBackend,PosixDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return freqFun<int,double,R_len_t,PosixBackend,PosixDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return freqFun<int,int,R_len_t,PosixBackend,PosixDate,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return freqFun<int,int,R_len_t,PosixBackend,PosixDate,PFUNC>(x);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("transformSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}

#endif // FREQ_TRANSFORM_TEMPLATE_HPP
