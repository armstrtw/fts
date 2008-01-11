#ifndef SEXP_ALLOCATOR_TEMPLATES_HPP
#define SEXP_ALLOCATOR_TEMPLATES_HPP

#include <Rinternals.h>

template<typename T>
class R_allocator {
public:
  static SEXP Matrix(const int nr, const int nc);
  static SEXP Vector(const int len);
  static T* R_dataPtr(const SEXP x);
  static T scalar(const SEXP x);
};

template<>
class R_allocator<double> {
public:
  static SEXP Matrix(const int nr, const int nc) {
    SEXP ans = allocMatrix(REALSXP,nr,nc);
    return ans;
  }

  static SEXP Vector(const int len) {
    SEXP ans = allocVector(REALSXP,len);
    return ans;
  }

  static double* R_dataPtr(const SEXP x) {
    return REAL(x);
  }

  static double scalar(const SEXP x) {
    return REAL(x)[0];
  }
};

template<>
class R_allocator<int> {
public:
  static SEXP Matrix(const int nr, const int nc) {
    SEXP ans = allocMatrix(INTSXP,nr,nc);
    return ans;
  }

  static SEXP Vector(const int len) {
    SEXP ans = allocVector(INTSXP,len);
    return ans;
  }

  static int* R_dataPtr(const SEXP x) {
    return INTEGER(x);
  }

  static int scalar(const SEXP x) {
    return INTEGER(x)[0];
  }
};

#endif // SEXP_ALLOCATOR_TEMPLATES_HPP
