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
  static SEXP Matrix(const R_len_t nr, const R_len_t nc);
  static SEXP Vector(const R_len_t len);
  static T* R_dataPtr(const SEXP x);
  static T scalar(const SEXP x);
};

template<>
class R_allocator<double> {
public:
  static SEXP Matrix(const R_len_t nr, const R_len_t nc) { return allocMatrix(REALSXP,nr,nc); }
  static SEXP Vector(const R_len_t len) { return allocVector(REALSXP,len); }
  static double* R_dataPtr(const SEXP x) { return REAL(x); }
  static double scalar(const SEXP x) { return REAL(x)[0]; }
};

template<>
class R_allocator<int> {
public:
  static SEXP Matrix(const R_len_t nr, const R_len_t nc) { return allocMatrix(INTSXP,nr,nc); }
  static SEXP Vector(const R_len_t len) { return allocVector(INTSXP,len); }
  static int* R_dataPtr(const SEXP x) { return INTEGER(x); }
  static int scalar(const SEXP x) { return INTEGER(x)[0]; }
};

template<>
class R_allocator<bool> {
public:
  static SEXP Matrix(const R_len_t nr, const R_len_t nc) { return allocMatrix(LGLSXP,nr,nc); }
  static SEXP Vector(const R_len_t len) { return allocVector(LGLSXP,len); }
  static int* R_dataPtr(const SEXP x) { return LOGICAL(x); }
  static int scalar(const SEXP x) { return LOGICAL(x)[0]; }
};

#endif // RSEXP_ALLOCATOR_TEMPLATES_HPP
