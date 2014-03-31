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

#define R_NO_REMAP
#include <Rinternals.h>

template<typename T>
class Rallocator {
public:
  static SEXPTYPE getType();
  static SEXP Matrix(const R_len_t nr, const R_len_t nc);
  static SEXP Vector(const R_len_t len);
  static T* R_dataPtr(const SEXP x);
  static T scalar(const SEXP x);
};

template<>
class Rallocator<double> {
public:
  static SEXPTYPE getType() { return REALSXP; }
  static SEXP Matrix(const R_len_t nr, const R_len_t nc) { return Rf_allocMatrix(REALSXP,nr,nc); }
  static SEXP Vector(const R_len_t len) { return Rf_allocVector(REALSXP,len); }
  static double* R_dataPtr(const SEXP x) { return REAL(x); }
  static double scalar(const SEXP x) { return REAL(x)[0]; }
};

template<>
class Rallocator<int> {
public:
  static SEXPTYPE getType() { return INTSXP; }
  static SEXP Matrix(const R_len_t nr, const R_len_t nc) { return Rf_allocMatrix(INTSXP,nr,nc); }
  static SEXP Vector(const R_len_t len) { return Rf_allocVector(INTSXP,len); }
  static int* R_dataPtr(const SEXP x) { return INTEGER(x); }
  static int scalar(const SEXP x) { return INTEGER(x)[0]; }
};

template<>
class Rallocator<bool> {
public:
  static SEXPTYPE getType() { return LGLSXP; }
  static SEXP Matrix(const R_len_t nr, const R_len_t nc) { return Rf_allocMatrix(LGLSXP,nr,nc); }
  static SEXP Vector(const R_len_t len) { return Rf_allocVector(LGLSXP,len); }
  static int* R_dataPtr(const SEXP x) { return LOGICAL(x); }
  static int scalar(const SEXP x) { return LOGICAL(x)[0]; }
};

#endif // RSEXP_ALLOCATOR_TEMPLATES_HPP
