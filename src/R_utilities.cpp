#include "R_utilities.hpp"

void addPOSIXattributes(SEXP x) {
  SEXP r_dates_class;

  PROTECT(r_dates_class = allocVector(STRSXP, 2));
  SET_STRING_ELT(r_dates_class, 0, mkChar("POSIXt"));
  SET_STRING_ELT(r_dates_class, 1, mkChar("POSIXct"));
  classgets(x, r_dates_class);
  UNPROTECT(1);
}

void addFtsClass(SEXP x) {
  SEXP r_tseries_class;
  PROTECT(r_tseries_class = allocVector(STRSXP, 2));
  SET_STRING_ELT(r_tseries_class, 0, mkChar("fts"));
  SET_STRING_ELT(r_tseries_class, 1, mkChar("matrix"));
  classgets(x, r_tseries_class);
  UNPROTECT(1);
}

void addDates(SEXP r_object,SEXP r_dates) {
  if(r_dates==R_NilValue) {
    return;
  }
  setAttrib(r_object,install("dates"),r_dates);
}

void setDates(SEXP x, SEXP dates) {
  setAttrib(x,install("dates"),dates);
}

SEXP getDatesSEXP(const SEXP x) {
  return getAttrib(x,install("dates"));
}


SEXP string2sexp(const vector<string>& x) {
  SEXP ans;
  PROTECT(ans = allocVector(STRSXP,x.size()));

  for(int i=0; i < static_cast<int>(x.size()); i++) {
    SET_STRING_ELT(ans, i, mkChar(x[i].c_str()));
  }
  UNPROTECT(1);
  return ans;
}

void setColnamesMatrix(const SEXP x, const vector<string>& s) {

  SEXP dimnames, cnames;

  int cn_size = s.size();

  if(cn_size!=ncols(x))
    return;

  PROTECT(cnames = string2sexp(s));
  PROTECT(dimnames = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(dimnames, 0, R_NilValue);
  SET_VECTOR_ELT(dimnames, 1, cnames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(2);
}

void getColnamesMatrix(const SEXP x, vector<string>& ans) {

  SEXP dimnames, cnames;

  PROTECT(dimnames = getAttrib(x, R_DimNamesSymbol));

  if(dimnames==R_NilValue) {
    UNPROTECT(1);  // dimnames
    return;
  }

  PROTECT(cnames = VECTOR_ELT(dimnames, 1));

  if(cnames==R_NilValue) {
    UNPROTECT(2); // dimnames and cnames
    return;
  }

  for(int i = 0; i < length(cnames); i++) {
    ans.push_back( CHAR(STRING_ELT(cnames,i)) );
  }

  UNPROTECT(2); // dimnames and cnames
}
