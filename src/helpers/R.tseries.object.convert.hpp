#ifndef R2TSERIES_OBJECT_CONVERT_HPP
#define R2TSERIES_OBJECT_CONVERT_HPP

#include <Rinternals.h>
#include <tslib/tseries.hpp>

#include <R.tseries.data.backend.hpp>
#include <Rsexp.allocator.templates.hpp>
#include <Rtype.hpp>

using namespace tslib;

template<SEXPTYPE RTYPE>
class r_convert;

template<>
class r_convert<REALSXP> {
public:
  static TSeries<double,Rtype<REALSXP>::ValueType,int,PosixBackend,PosixDate> apply(SEXP x) {

    // build tseries from SEXP x
    PosixBackend<double,Rtype<REALSXP>::ValueType,int> tsData(x);
    TSeries<double,Rtype<REALSXP>::ValueType,int,PosixBackend,PosixDate> ts(tsData);
    return ts;
  }
};

template<>
class r_convert<INTSXP> {
public:
  static TSeries<double,Rtype<INTSXP>::ValueType,int,PosixBackend,PosixDate> apply(SEXP x) {

    // build tseries from SEXP x
    PosixBackend<double,Rtype<INTSXP>::ValueType,int> tsData(x);
    TSeries<double,Rtype<INTSXP>::ValueType,int,PosixBackend,PosixDate> ts(tsData);
    return ts;
  }
};

template<>
class r_convert<LGLSXP> {
public:
  static TSeries<double,Rtype<LGLSXP>::ValueType,int,PosixBackend,PosixDate> apply(SEXP x) {

    // build tseries from SEXP x
    PosixBackend<double,Rtype<LGLSXP>::ValueType,int> tsData(x);
    TSeries<double,Rtype<LGLSXP>::ValueType,int,PosixBackend,PosixDate> ts(tsData);
    return ts;
  }
};


#endif // R2TSERIES_OBJECT_CONVERT_HPP
