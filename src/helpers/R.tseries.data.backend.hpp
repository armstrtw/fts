#ifndef R_TSERIES_DATA_BACKEND_HPP
#define R_TSERIES_DATA_BACKEND_HPP

#include <vector>
#include <string>
#include <algorithm>
#include <Rinternals.h>
#include <Rsexp.allocator.templates.hpp>

template <typename TDATE,typename TDATA, typename TSDIM>
class R_Backend_TSdata {
public:
  SEXP R_object;

  ~R_Backend_TSdata() {
    if(R_object!=R_NilValue) {
      UNPROTECT_PTR(R_object);
    }
  }

  R_Backend_TSdata(): R_object(R_NilValue) {}
  R_Backend_TSdata(const R_Backend_TSdata& t): R_object(t.R_object) { PROTECT(R_object); }

  R_Backend_TSdata(const TSDIM rows, const TSDIM cols) {
    // this object stays protected after we exit the constructor
    PROTECT(R_object = R_allocator<TDATA>::Matrix(rows, cols));

    // attach dates to R_object
    SEXP R_dates = PROTECT(R_allocator<TDATE>::Vector(rows));
    setAttrib(R_object,install("index"),R_dates);
    UNPROTECT(1); // we can unprotect dates now

    // create and add dates class to dates object
    SEXP r_dates_class = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(r_dates_class, 0, mkChar("POSIXct"));
    SET_STRING_ELT(r_dates_class, 1, mkChar("POSIXt"));
    classgets(R_dates, r_dates_class);
    UNPROTECT(1); // r_dates_class

    // add fts class to R_object
    SEXP r_tseries_class = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(r_tseries_class, 0, mkChar("fts"));
    SET_STRING_ELT(r_tseries_class, 1, mkChar("zoo"));
    classgets(R_object, r_tseries_class);
    UNPROTECT(1); // r_tseries_class
  }

  R_Backend_TSdata(const SEXP x): R_object(x) { PROTECT(R_object); }

  TSDIM nrow() const { return nrows(R_object); }
  TSDIM ncol() const { return ncols(R_object); }
  TDATA* getData() const { return R_allocator<TDATA>::R_dataPtr(R_object); }
  TDATE* getDates() const { return R_allocator<TDATE>::R_dataPtr(getAttrib(R_object,install("index"))); }

  void setColnames(const std::vector<std::string>& cnames) {
    int protect_count(0);

    if(static_cast<TSDIM>(cnames.size()) != ncols(R_object)) {
      return;
    }

    // check if we have existing dimnames
    SEXP dimnames = getAttrib(R_object, R_DimNamesSymbol);
    if(dimnames == R_NilValue) {
      PROTECT(dimnames = allocVector(VECSXP, 2)); ++protect_count;
      SET_VECTOR_ELT(dimnames, 0, R_NilValue);
    }
    SEXP cnames_sexp = PROTECT(allocVector(STRSXP,cnames.size())); ++protect_count;
    for(R_len_t i = 0; i < cnames.size(); ++i) {
      SET_STRING_ELT(cnames_sexp, i, mkChar(cnames[i].c_str()));
    }
    SET_VECTOR_ELT(dimnames, 1, cnames_sexp);
    setAttrib(R_object, R_DimNamesSymbol, dimnames);
    UNPROTECT(protect_count);
  }

  std::vector<std::string> getColnames() const {
    std::vector<std::string> ans;
    if(getAttrib(R_object, R_DimNamesSymbol)!=R_NilValue &&
       VECTOR_ELT(getAttrib(R_object, R_DimNamesSymbol), 1)!=R_NilValue) {
      SEXP cnames = VECTOR_ELT(getAttrib(R_object, R_DimNamesSymbol), 1);
      for(R_len_t i = 0; i < length(cnames);++i) {
        ans.push_back(CHAR(STRING_ELT(cnames,i)));
      }
    }
    return ans;
  }

  const size_t getColnamesSize() const {
    if(getAttrib(R_object, R_DimNamesSymbol)!=R_NilValue &&
       VECTOR_ELT(getAttrib(R_object, R_DimNamesSymbol), 1)!=R_NilValue) {
      return length(VECTOR_ELT(getAttrib(R_object, R_DimNamesSymbol), 1));
    }
    return 0;
  }
};

#endif // R_TSERIES_DATA_BACKEND_HPP
