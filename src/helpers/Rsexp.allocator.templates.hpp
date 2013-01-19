///////////////////////////////////////////////////////////////////////////
// Copyright (C) 2008  Whit Armstrong                                    //
//                                                                       //
// This program is free software: you can redistribute it and/or modify  //
// it under the terms of the GNU General Public License as published by  //
// the Free Software Foundation, either version 3 of the License, or     //
// (at your option) any later version.                                   //
//                                                                       //
// This program is distributed in the hope that it will be useful,       //
// but WITHOUT ANY WARRANTY; without even the implied warranty of        //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         //
// GNU General Public License for more details.                          //
//                                                                       //
// You should have received a copy of the GNU General Public License     //
// along with this program.  If not, see <http://www.gnu.org/licenses/>. //
///////////////////////////////////////////////////////////////////////////

#ifndef RSEXP_ALLOCATOR_TEMPLATES_HPP
#define RSEXP_ALLOCATOR_TEMPLATES_HPP

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

template<>
class R_allocator<bool> {
public:
  static SEXP Matrix(const int nr, const int nc) {
    SEXP ans = allocMatrix(LGLSXP,nr,nc);
    return ans;
  }

  static SEXP Vector(const int len) {
    SEXP ans = allocVector(LGLSXP,len);
    return ans;
  }

  static int* R_dataPtr(const SEXP x) {
    return LOGICAL(x);
  }

  static int scalar(const SEXP x) {
    return LOGICAL(x)[0];
  }
};

#endif // RSEXP_ALLOCATOR_TEMPLATES_HPP
