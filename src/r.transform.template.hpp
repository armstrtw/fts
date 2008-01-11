#ifndef R_TRANSFORM_TEMPLATE_HPP
#define R_TRANSFORM_TEMPLATE_HPP

#include <Rinternals.h>
#include <tslib/tseries.hpp>

#include "R.tseries.data.backend.hpp"
#include "sexp.allocator.templates.hpp"
#include "Rtype.hpp"


using namespace tslib;

template<SEXPTYPE RTYPE>
class r_transform;


template<>
class r_transform<REALSXP> {
public:
  template<template<class> class transformFunction, template<class> class transformFunctionTraits>
  static SEXP apply(SEXP x) {

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<REALSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);


    // define our answer type based on windowFunctionTraits return type
    typedef typename transformFunctionTraits< Rtype<REALSXP>::ValueType  >::ReturnType ansType;

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.transform<ansType,transformFunction>();

    return ans.getIMPL()->R_object;
  }
};

template<>
class r_transform<INTSXP> {
public:
  template<template<class> class transformFunction, template<class> class transformFunctionTraits>
  static SEXP apply(SEXP x) {

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<INTSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);


    // define our answer type based on windowFunctionTraits return type
    typedef typename transformFunctionTraits< Rtype<INTSXP>::ValueType  >::ReturnType ansType;

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.transform<ansType,transformFunction>();

    return ans.getIMPL()->R_object;
  }
};


template<SEXPTYPE RTYPE>
class r_transform_1arg;


template<>
class r_transform_1arg<REALSXP> {
public:
  template<template<class> class transformFunction, template<class> class transformFunctionTraits>
  static SEXP apply(SEXP x, SEXP arg1) {

    // use policy class to discover argument type
    typedef typename transformFunctionTraits< Rtype<REALSXP>::ValueType  >::ArgType ArgType;

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<REALSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);


    // define our answer type based on windowFunctionTraits return type
    typedef typename transformFunctionTraits< Rtype<REALSXP>::ValueType  >::ReturnType ansType;

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.transform_1arg<ansType,transformFunction>( R_allocator<ArgType>::scalar(arg1) );

    return ans.getIMPL()->R_object;
  }
};

template<>
class r_transform_1arg<INTSXP> {
public:
  template<template<class> class transformFunction, template<class> class transformFunctionTraits>
  static SEXP apply(SEXP x, SEXP arg1) {

    // use policy class to discover argument type
    typedef typename transformFunctionTraits< Rtype<INTSXP>::ValueType  >::ArgType ArgType;

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<INTSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);


    // define our answer type based on windowFunctionTraits return type
    typedef typename transformFunctionTraits< Rtype<INTSXP>::ValueType  >::ReturnType ansType;

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = ts.transform_1arg<ansType,transformFunction>( R_allocator<ArgType>::scalar(arg1) );

    return ans.getIMPL()->R_object;
  }
};



#endif // R_TRANSFORM_TEMPLATE_HPP
