#ifndef R_TSERIES_DATA_BACKEND_HPP
#define R_TSERIES_DATA_BACKEND_HPP

#include <vector>
#include <string>
#include <algorithm>
#include <exception>
#include <Rinternals.h>
#include <Rsexp.allocator.templates.hpp>


class BackendBase {
public:
  SEXP R_object;
  ~BackendBase() {
    if(R_object!=R_NilValue) {
      UNPROTECT_PTR(R_object);
    }
  }

  BackendBase(): R_object(R_NilValue) {}

  // deep copy (do we need this)?
  // BackendBase(const BackendBase& t): R_object(PROTECT(duplicate(t.R_object))) {};

  // delegating constructor
  BackendBase(const BackendBase& t): BackendBase(t.R_object) {}

  // SEXP constructor assumes an existing fts object
  // throw if fts class is missing or index is missing
  BackendBase(const SEXP x): R_object(PROTECT(x)) {
    if(getAttrib(R_object,R_ClassSymbol)==R_NilValue) {
      throw std::logic_error("BackendBase(const SEXP x): Object has no classname.");
    }
    if(strcmp(CHAR(STRING_ELT(getAttrib(R_object,R_ClassSymbol),0)),"fts")!=0) {
      throw std::logic_error("BackendBase(const SEXP x): not an fts object.");
    }
    if(getAttrib(R_object,install("index"))==R_NilValue) {
      throw std::logic_error("BackendBase(const SEXP x): Object has no index.");
    }
  }

  // use this constructor for new fts objects
  BackendBase(SEXPTYPE rtype, R_len_t nr, R_len_t nc): R_object(PROTECT(allocMatrix(rtype,nr,nc))) {
    // add fts class to R_object
    SEXP r_tseries_class = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(r_tseries_class, 0, mkChar("fts"));
    SET_STRING_ELT(r_tseries_class, 1, mkChar("zoo"));
    classgets(R_object, r_tseries_class);
    UNPROTECT(1); // r_tseries_class
  }

  R_len_t nrow() const { return nrows(R_object); }
  R_len_t ncol() const { return ncols(R_object); }

  void setColnames(const std::vector<std::string>& cnames) {
    int protect_count(0);

    if(static_cast<R_len_t>(cnames.size()) != ncols(R_object)) {
      REprintf("setColnames: colnames size does not match ncols(R_object)."); 
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

template <typename TDATE,typename TDATA, typename TSDIM>
class PosixBackend : public BackendBase {
public:
  PosixBackend() {}
  PosixBackend(const PosixBackend& t): BackendBase(t.R_object) {}
  PosixBackend(const TSDIM rows, const TSDIM cols): BackendBase(R_allocator<TDATA>::getType(),rows, cols) {
    // create dates
    SEXP R_dates = PROTECT(R_allocator<TDATE>::Vector(rows));

    // create and add dates class to dates object
    SEXP r_dates_class = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(r_dates_class, 0, mkChar("POSIXct"));
    SET_STRING_ELT(r_dates_class, 1, mkChar("POSIXt"));
    classgets(R_dates, r_dates_class);

    // attach dates to R_object
    setAttrib(R_object,install("index"),R_dates);
    UNPROTECT(2); // R_dates, r_dates_class
  }
  PosixBackend(const SEXP x): BackendBase(x) {}

  TSDIM nrow() const { return BackendBase::nrow(); }
  TSDIM ncol() const { return BackendBase::ncol(); }
  TDATA* getData() const { return R_allocator<TDATA>::R_dataPtr(R_object); }
  TDATE* getDates() const { return R_allocator<TDATE>::R_dataPtr(getAttrib(R_object,install("index"))); }
};

template <typename TDATE,typename TDATA, typename TSDIM>
class JulianBackend : public BackendBase {
public:
  JulianBackend() {}
  JulianBackend(const JulianBackend& t): BackendBase(t.R_object) {}
  JulianBackend(const TSDIM rows, const TSDIM cols): BackendBase(R_allocator<TDATA>::getType(),rows, cols) {
    // create dates
    SEXP R_dates = PROTECT(R_allocator<TDATE>::Vector(rows));

    // create and add dates class to dates object
    SEXP r_dates_class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(r_dates_class, 0, mkChar("Date"));
    classgets(R_dates, r_dates_class);

    // attach dates to R_object
    setAttrib(R_object,install("index"),R_dates);
    UNPROTECT(2); // R_dates, r_dates_class
  }
  JulianBackend(const SEXP x): BackendBase(x) {}

  TSDIM nrow() const { return BackendBase::nrow(); }
  TSDIM ncol() const { return BackendBase::ncol(); }
  TDATA* getData() const { return R_allocator<TDATA>::R_dataPtr(R_object); }
  TDATE* getDates() const { return R_allocator<TDATE>::R_dataPtr(getAttrib(R_object,install("index"))); }
};

#endif // R_TSERIES_DATA_BACKEND_HPP
