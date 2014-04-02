#ifndef R_TSERIES_DATA_BACKEND_HPP
#define R_TSERIES_DATA_BACKEND_HPP

#include <vector>
#include <string>
#include <algorithm>
#include <exception>
#define R_NO_REMAP
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
  BackendBase(const BackendBase& t): R_object(PROTECT(t.R_object)) {
    if(Rf_getAttrib(R_object,R_ClassSymbol)==R_NilValue) {
      throw std::logic_error("BackendBase(const SEXP x): Object has no classname.");
    }
    if(strcmp(CHAR(STRING_ELT(Rf_getAttrib(R_object,R_ClassSymbol),0)),"fts")!=0) {
      throw std::logic_error("BackendBase(const SEXP x): not an fts object.");
    }
    if(Rf_getAttrib(R_object,Rf_install("index"))==R_NilValue) {
      throw std::logic_error("BackendBase(const SEXP x): Object has no index.");
    }
  }

  // SEXP constructor assumes an existing fts object
  // throw if fts class is missing or index is missing
  BackendBase(const SEXP x): R_object(PROTECT(x)) {
    if(Rf_getAttrib(R_object,R_ClassSymbol)==R_NilValue) {
      throw std::logic_error("BackendBase(const SEXP x): Object has no classname.");
    }
    if(strcmp(CHAR(STRING_ELT(Rf_getAttrib(R_object,R_ClassSymbol),0)),"fts")!=0) {
      throw std::logic_error("BackendBase(const SEXP x): not an fts object.");
    }
    if(Rf_getAttrib(R_object,Rf_install("index"))==R_NilValue) {
      throw std::logic_error("BackendBase(const SEXP x): Object has no index.");
    }
  }

  // use this constructor for new fts objects
  BackendBase(SEXPTYPE rtype, R_len_t nr, R_len_t nc): R_object(PROTECT(Rf_allocMatrix(rtype,nr,nc))) {
    // add fts class to R_object
    SEXP r_tseries_class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(r_tseries_class, 0, Rf_mkChar("fts"));
    SET_STRING_ELT(r_tseries_class, 1, Rf_mkChar("zoo"));
    Rf_classgets(R_object, r_tseries_class);
    UNPROTECT(1); // r_tseries_class
  }

  R_len_t nrow() const { return Rf_nrows(R_object); }
  R_len_t ncol() const { return Rf_ncols(R_object); }

  void setColnames(const std::vector<std::string>& cnames) {
    int protect_count(0);

    if(static_cast<R_len_t>(cnames.size()) != Rf_ncols(R_object)) {
      REprintf("setColnames: colnames size does not match ncols(R_object)."); 
      return;
    }

    // check if we have existing dimnames
    SEXP dimnames = Rf_getAttrib(R_object, R_DimNamesSymbol);
    if(dimnames == R_NilValue) {
      PROTECT(dimnames = Rf_allocVector(VECSXP, 2)); ++protect_count;
      SET_VECTOR_ELT(dimnames, 0, R_NilValue);
    }
    SEXP cnames_sexp = PROTECT(Rf_allocVector(STRSXP,cnames.size())); ++protect_count;
    for(R_len_t i = 0; i < cnames.size(); ++i) {
      SET_STRING_ELT(cnames_sexp, i, Rf_mkChar(cnames[i].c_str()));
    }
    SET_VECTOR_ELT(dimnames, 1, cnames_sexp);
    Rf_setAttrib(R_object, R_DimNamesSymbol, dimnames);
    UNPROTECT(protect_count);
  }

  std::vector<std::string> getColnames() const {
    std::vector<std::string> ans;
    if(Rf_getAttrib(R_object, R_DimNamesSymbol)!=R_NilValue &&
       VECTOR_ELT(Rf_getAttrib(R_object, R_DimNamesSymbol), 1)!=R_NilValue) {
      SEXP cnames = VECTOR_ELT(Rf_getAttrib(R_object, R_DimNamesSymbol), 1);
      for(R_len_t i = 0; i < Rf_length(cnames);++i) {
        ans.push_back(CHAR(STRING_ELT(cnames,i)));
      }
    }
    return ans;
  }

  const size_t getColnamesSize() const {
    if(Rf_getAttrib(R_object, R_DimNamesSymbol)!=R_NilValue &&
       VECTOR_ELT(Rf_getAttrib(R_object, R_DimNamesSymbol), 1)!=R_NilValue) {
      return Rf_length(VECTOR_ELT(Rf_getAttrib(R_object, R_DimNamesSymbol), 1));
    }
    return 0;
  }
};

template <typename TDATE,typename TDATA, typename TSDIM>
class PosixBackend : public BackendBase {
public:
  PosixBackend() {}
  PosixBackend(const PosixBackend& t): BackendBase(t.R_object) {}
  PosixBackend(const TSDIM rows, const TSDIM cols): BackendBase(Rallocator<TDATA>::getType(),rows, cols) {
    // create dates
    SEXP R_dates = PROTECT(Rallocator<TDATE>::Vector(rows));

    // create and add dates class to dates object
    SEXP r_dates_class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(r_dates_class, 0, Rf_mkChar("POSIXct"));
    SET_STRING_ELT(r_dates_class, 1, Rf_mkChar("POSIXt"));
    Rf_classgets(R_dates, r_dates_class);

    // attach dates to R_object
    Rf_setAttrib(R_object,Rf_install("index"),R_dates);
    UNPROTECT(2); // R_dates, r_dates_class
  }
  PosixBackend(const SEXP x): BackendBase(x) {}

  TSDIM nrow() const { return BackendBase::nrow(); }
  TSDIM ncol() const { return BackendBase::ncol(); }
  TDATA* getData() const { return Rallocator<TDATA>::R_dataPtr(R_object); }
  TDATE* getDates() const { return Rallocator<TDATE>::R_dataPtr(Rf_getAttrib(R_object,Rf_install("index"))); }
};

template <typename TDATE,typename TDATA, typename TSDIM>
class JulianBackend : public BackendBase {
public:
  JulianBackend() {}
  JulianBackend(const JulianBackend& t): BackendBase(t.R_object) {}
  JulianBackend(const TSDIM rows, const TSDIM cols): BackendBase(Rallocator<TDATA>::getType(),rows, cols) {
    // create dates
    SEXP R_dates = PROTECT(Rallocator<TDATE>::Vector(rows));

    // create and add dates class to dates object
    SEXP r_dates_class = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(r_dates_class, 0, Rf_mkChar("Date"));
    Rf_classgets(R_dates, r_dates_class);

    // attach dates to R_object
    Rf_setAttrib(R_object,Rf_install("index"),R_dates);
    UNPROTECT(2); // R_dates, r_dates_class
  }
  JulianBackend(const SEXP x): BackendBase(x) {}

  TSDIM nrow() const { return BackendBase::nrow(); }
  TSDIM ncol() const { return BackendBase::ncol(); }
  TDATA* getData() const { return Rallocator<TDATA>::R_dataPtr(R_object); }
  TDATE* getDates() const { return Rallocator<TDATE>::R_dataPtr(Rf_getAttrib(R_object,Rf_install("index"))); }
};

#endif // R_TSERIES_DATA_BACKEND_HPP
