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

#endif // R_WINDOW_TEMPLATE_HPP
