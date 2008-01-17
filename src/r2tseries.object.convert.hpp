#ifndef R2TSERIES_OBJECT_CONVERT_HPP
#define R2TSERIES_OBJECT_CONVERT_HPP

#include <Rinternals.h>
#include <tslib/tseries.hpp>

#include "R.tseries.data.backend.hpp"
#include "sexp.allocator.templates.hpp"
#include "Rtype.hpp"


using namespace tslib;

template<SEXPTYPE RTYPE>
class r_convert;

template<>
class r_convert<REALSXP> {
public:
  static TSeries<double,Rtype<REALSXP>::ValueType,int,R_Backend_TSdata,PosixDate> apply(SEXP x) {

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<REALSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<REALSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);
    return ts;
  }
};

template<>
class r_convert<INTSXP> {
public:
  static TSeries<double,Rtype<INTSXP>::ValueType,int,R_Backend_TSdata,PosixDate> apply(SEXP x) {

    // build tseries from SEXP x
    R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>* tsData = R_Backend_TSdata<double,Rtype<INTSXP>::ValueType,int>::init(x);
    TSeries<double,Rtype<INTSXP>::ValueType,int,R_Backend_TSdata,PosixDate> ts(tsData);
    return ts;
  }
};


#endif // R2TSERIES_OBJECT_CONVERT_HPP
