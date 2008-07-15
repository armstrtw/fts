#ifndef R_WINDOW_TEMPLATE_HPP
#define R_WINDOW_TEMPLATE_HPP

#include <Rinternals.h>
#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rtype.hpp>


using namespace tslib;

template<SEXPTYPE RTYPE>
class r_window;


template<>
class r_window<REALSXP> {
public:
  template<template<class> class windowFunction, template<class> class windowFunctionTraits>
  static SEXP apply(SEXP x, SEXP periods) {

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<REALSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits< Rtype<REALSXP>::ValueType  >::ReturnType ansType;

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.window<ansType,windowFunction>(p);

    return ans.getIMPL()->R_object;
  }
};

template<>
class r_window<INTSXP> {
public:
  template<template<class> class windowFunction, template<class> class windowFunctionTraits>
  static SEXP apply(SEXP x, SEXP periods) {

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<INTSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits< Rtype<INTSXP>::ValueType  >::ReturnType ansType;

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.window<ansType,windowFunction>(p);

    return ans.getIMPL()->R_object;
  }
};


#endif // R_WINDOW_TEMPLATE_HPP
