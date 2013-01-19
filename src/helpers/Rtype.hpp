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

#ifndef RTYPE_HPP
#define RTYPE_HPP

#include <Rinternals.h>

template<SEXPTYPE T>
class Rtype;

template<>
class Rtype<REALSXP> {
public:
  typedef double ValueType;
  static double& scalar(SEXP x) {
    return REAL(x)[0];
  }
  static double& index(SEXP x, R_len_t i) {
    return REAL(x)[i];
  }
};

template<>
class Rtype<INTSXP> {
public:
  typedef int ValueType;
  static int& scalar(SEXP x) {
    return INTEGER(x)[0];
  }
  static int& index(SEXP x, R_len_t i) {
    return INTEGER(x)[i];
  }
};

template<>
class Rtype<LGLSXP> {
public:
  typedef int ValueType;
  static int& scalar(SEXP x) {
    return LOGICAL(x)[0];
  }
  static int& index(SEXP x, R_len_t i) {
    return LOGICAL(x)[i];
  }
};

template<>
class Rtype<STRSXP> {
public:
  typedef SEXP ValueType;
  static SEXP& scalar(SEXP x) {
    return STRING_PTR(x)[0];
  }
  static SEXP& index(SEXP x, R_len_t i) {
    return STRING_PTR(x)[i];
  }
};


template<>
class Rtype<VECSXP> {
public:
  typedef SEXP ValueType;
  static SEXP scalar(SEXP x) {
    return VECTOR_ELT(x,0);
  }
  static SEXP index(SEXP x, R_len_t i) {
    return VECTOR_ELT(x,i);
  }
};



#endif // RTYPE_HPP
