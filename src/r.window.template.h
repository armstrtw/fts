#ifndef R_WINDOW_TEMPLATE_HPP
#define R_WINDOW_TEMPLATE_HPP

#include <Rinternals.h>
#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rtype.hpp>


using namespace tslib;

template<SEXPTYPE RTYPE>
class r_window {
    typedef typename Rtype<RTYPE>::ValueType VT;
public:
  template<template<class> class windowFunction, template<class> class windowFunctionTraits>
  static SEXP apply(SEXP x, SEXP periods) {

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits<VT>::ReturnType ansType;

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    // build tseries from SEXP x
    R_Backend_TSdata<double,VT,int>* tsData = R_Backend_TSdata<double,VT,int>::init(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts(tsData);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.template window<ansType,windowFunction>(p);

    return ans.getIMPL()->R_object;
  }
};

template<SEXPTYPE RTYPE>
class r_window_2args {
    typedef typename Rtype<RTYPE>::ValueType VT;
public:
  template<template<class> class windowFunction, template<class> class windowFunctionTraits>
  static SEXP apply(SEXP x, SEXP y, SEXP periods) {
    if(TYPEOF(x)!=TYPEOF(y)) {
      std::cerr << "movingCov: x and y must be the same type" << std::endl;
      return R_NilValue;
    }

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits<VT>::ReturnType ansType;

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    // build tseries from SEXP
    R_Backend_TSdata<double,VT,int>* tsData1 = R_Backend_TSdata<double,VT,int>::init(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts1(tsData1);

    R_Backend_TSdata<double,VT,int>* tsData2 = R_Backend_TSdata<double,VT,int>::init(y);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts2(tsData2);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = window_function<ansType,windowFunction>(ts1,ts2,p);

    return ans.getIMPL()->R_object;
  }
};


template<template<class> class windowFunction, template<class> class windowFunctionTraits>
SEXP windowSpecializer(SEXP x, SEXP periods) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_window<REALSXP>::apply<windowFunction, windowFunctionTraits>(x,periods);
  case INTSXP:
    return r_window<INTSXP>::apply<windowFunction, windowFunctionTraits>(x,periods);
  case LGLSXP:
    return r_window<LGLSXP>::apply<windowFunction, windowFunctionTraits>(x,periods);
  default:
    return R_NilValue;
  }
}

template<template<class> class windowFunction, template<class> class windowFunctionTraits>
SEXP windowSpecializer_2args(SEXP x, SEXP y, SEXP periods) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_window_2args<REALSXP>::apply<windowFunction, windowFunctionTraits>(x,y,periods);
  case INTSXP:
    return r_window_2args<INTSXP>::apply<windowFunction, windowFunctionTraits>(x,y,periods);
  case LGLSXP:
    return r_window_2args<LGLSXP>::apply<windowFunction, windowFunctionTraits>(x,y,periods);
  default:
    return R_NilValue;
  }
}



#endif // R_WINDOW_TEMPLATE_HPP
