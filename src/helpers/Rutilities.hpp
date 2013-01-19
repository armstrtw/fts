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

#ifndef RUTILTIES_HPP
#define RUTILTIES_HPP


#include <string>
#include <Rinternals.h>

template<typename T>
SEXP string2sexp(T first, T last) {
  SEXP ans;
  const int ans_size = static_cast<const int>(std::distance(first,last));

  PROTECT(ans = allocVector(STRSXP,ans_size));

  int i = 0;
  while(first != last) {
    SET_STRING_ELT(ans, i, mkChar(first->c_str()));
    ++first; ++i;
  }
  UNPROTECT(1);
  return ans;
}

template<typename T>
void sexp2string(const SEXP str_sexp, T insert_iter) {

  if(str_sexp==R_NilValue)
    return;

  for(int i=0; i < length(str_sexp); i++) {
    //insert_iter->assign(CHAR(STRING_ELT(str_sexp,i)));
    *insert_iter = std::string(CHAR(STRING_ELT(str_sexp,i)));
    ++insert_iter;
  }
}

#endif // RUTILTIES_HPP
