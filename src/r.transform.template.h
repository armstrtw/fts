#ifndef R_TRANSFORM_TEMPLATE_HPP
#define R_TRANSFORM_TEMPLATE_HPP

#include <Rinternals.h>
#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rsexp.allocator.templates.hpp>
#include <Rtype.hpp>


using namespace tslib;

template<SEXPTYPE RTYPE>
class r_transform {
  typedef typename Rtype<RTYPE>::ValueType VT;
public:
  template<template<class> class transformFunction, template<class> class transformFunctionTraits>
  static SEXP apply(SEXP x) {

    // define our answer type based on windowFunctionTraits return type
    typedef typename transformFunctionTraits<VT>::ReturnType ansType;

    // build tseries from SEXP x
    R_Backend_TSdata<double,VT,int>* tsData = R_Backend_TSdata<double,VT,int>::init(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts(tsData);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.template transform<ansType,transformFunction>();

    return ans.getIMPL()->R_object;
  }
};

template<SEXPTYPE RTYPE>
class r_transform_1arg {
  typedef typename Rtype<RTYPE>::ValueType VT;
public:
  template<template<class> class transformFunction, template<class> class transformFunctionTraits>
  static SEXP apply(SEXP x, SEXP arg1) {

    // define our answer type based on windowFunctionTraits return type
    typedef typename transformFunctionTraits<VT>::ReturnType ansType;

    // use policy class to discover argument type
    typedef typename transformFunctionTraits<VT>::ArgType ArgType;

    // build tseries from SEXP x
    R_Backend_TSdata<double,VT,int>* tsData = R_Backend_TSdata<double,VT,int>::init(x);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> ts(tsData);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.template transform_1arg<ansType,transformFunction>(R_allocator<ArgType>::scalar(arg1));

    return ans.getIMPL()->R_object;
  }
};


#endif // R_TRANSFORM_TEMPLATE_HPP
