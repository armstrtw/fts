// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; tab-width: 8 -*-
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
    R_Backend_TSdata<double,VT,int> tsData(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts(tsData);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.template window<ansType,windowFunction>(p);

    return ans.getIMPL()->R_object;
  }
};

template<SEXPTYPE RTYPE>
class r_time_window {
  typedef typename Rtype<RTYPE>::ValueType VT;
public:
  template<template<class> class windowFunction, template<class> class windowFunctionTraits, template<class, template<typename> class> class PFUNC>
  static SEXP apply(SEXP x) {

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits<VT>::ReturnType ansType;

    // build tseries from SEXP x
    R_Backend_TSdata<double,VT,int> tsData(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts(tsData);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.template time_window<ansType, windowFunction, PFUNC>();

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
      Rprintf("x and y must be the same type.");
      return R_NilValue;
    }

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits<VT>::ReturnType ansType;

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    // build tseries from SEXP
    R_Backend_TSdata<double,VT,int> tsData1(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts1(tsData1);

    R_Backend_TSdata<double,VT,int> tsData2(y);
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

template<template<class> class windowFunction, template<class> class windowFunctionTraits, template<class, template<typename> class> class PFUNC>
SEXP timeWindowSpecializer(SEXP x) {
  switch(TYPEOF(x)) {
  case REALSXP:
    return r_time_window<REALSXP>::apply<windowFunction, windowFunctionTraits, PFUNC>(x);
  case INTSXP:
    return r_time_window<INTSXP>::apply<windowFunction, windowFunctionTraits, PFUNC>(x);
  case LGLSXP:
    return r_time_window<LGLSXP>::apply<windowFunction, windowFunctionTraits, PFUNC>(x);
  default:
    return R_NilValue;
  }
}


#endif // R_WINDOW_TEMPLATE_HPP
