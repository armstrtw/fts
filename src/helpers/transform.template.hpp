#ifndef TRANSFORM_TEMPLATE_HPP
#define TRANSFORM_TEMPLATE_HPP

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
         template<class> class transformFunction,
         template<class> class transformFunctionTraits>
SEXP transformFun(SEXP x) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename transformFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template transform<ReturnTDATA,transformFunction>();
  return ans.getIMPL()->R_object;
}

template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy,
         template<class> class transformFunction,
         template<class> class transformFunctionTraits>
SEXP transformFun(SEXP x, SEXP arg1) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename transformFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);

  // use policy class to discover argument type
  typedef typename transformFunctionTraits<TDATA>::ArgType ArgType;


  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template transform_1arg<ReturnTDATA,transformFunction>(Rallocator<ArgType>::scalar(arg1));
  return ans.getIMPL()->R_object;
}

template<template<class> class transformFunction, template<class> class transformFunctionTraits>
SEXP transformSpecializer(SEXP x) {
  const TsTypeTuple tsTypeInfo(x);

  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<double,double,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<double,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<double,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<int,double,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<int,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<int,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<double,double,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<double,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<double,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<int,double,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<int,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<int,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("transformSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}

template<template<class> class transformFunction, template<class> class transformFunctionTraits>
SEXP transformSpecializer(SEXP x, SEXP arg1) {
  const TsTypeTuple tsTypeInfo(x);

  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<double,double,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<double,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<double,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<int,double,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<int,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==dateT) {
    return transformFun<int,int,R_len_t,JulianBackend,JulianDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<double,double,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<double,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<double,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<int,double,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<int,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==posixT) {
    return transformFun<int,int,R_len_t,PosixBackend,PosixDate,transformFunction,transformFunctionTraits>(x,arg1);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("transformSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}

#endif // TRANSFORM_TEMPLATE_HPP
