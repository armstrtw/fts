#ifndef RTYPE_HPP
#define RTYPE_HPP

#include <Rinternals.h>


template<SEXPTYPE T>
class Rtype;

template<>
class Rtype<REALSXP> {
public:
  typedef double ValueType;
  static double scalar(SEXP x) {
    return REAL(x)[0];
  }
};

template<>
class Rtype<INTSXP> {
public:
  typedef int ValueType;
  static int scalar(SEXP x) {
    return INTEGER(x)[0];
  }
};

template<>
class Rtype<LGLSXP> {
public:
  typedef int ValueType;
  static int scalar(SEXP x) {
    return INTEGER(x)[0];
  }
};



#endif // RTYPE_HPP
