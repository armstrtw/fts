#ifndef R_TSERIES_DATA_BACKEND_HPP
#define R_TSERIES_DATA_BACKEND_HPP

#include <vector>
#include <string>
#include <algorithm>
#include <Rinternals.h>
#include <Rsexp.allocator.templates.hpp>
#include <Rutilities.hpp>

template <typename TDATE,typename TDATA, typename TSDIM>
class R_Backend_TSdata {
public:
  SEXP R_object;
  ~R_Backend_TSdata();
  R_Backend_TSdata();
  R_Backend_TSdata(const R_Backend_TSdata& t);
  R_Backend_TSdata(const TSDIM rows, const TSDIM cols);
  R_Backend_TSdata(const SEXP x);

  R_Backend_TSdata& operator=(const R_Backend_TSdata& rhs);

  TSDIM nrow() const;
  TSDIM ncol() const;
  TDATA* getData() const;
  TDATE* getDates() const;
  void setColnames(const std::vector<std::string>& cnames);
  std::vector<std::string> getColnames() const;
  const size_t getColnamesSize() const;
};

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::~R_Backend_TSdata() {
  if(R_object!=R_NilValue) {
    UNPROTECT_PTR(R_object);
  }
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata(): R_object(R_NilValue) {}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata(const R_Backend_TSdata& t): R_object(t.R_object) {
  PROTECT(R_object);
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata(const TSDIM rows, const TSDIM cols) {
  SEXP R_dates;

  // this object stays protected after we exit the constructor
  PROTECT(R_object = R_allocator<TDATA>::Matrix(rows, cols));  

  // attach dates to R_object
  PROTECT(R_dates  = R_allocator<TDATE>::Vector(rows));
  setAttrib(R_object,install("dates"),R_dates);
  UNPROTECT(1); // we can unprotect dates now

  // create and add dates class to dates object
  SEXP r_dates_class;
  PROTECT(r_dates_class = allocVector(STRSXP, 2));
  SET_STRING_ELT(r_dates_class, 0, mkChar("POSIXt"));
  SET_STRING_ELT(r_dates_class, 1, mkChar("POSIXct"));
  classgets(R_dates, r_dates_class);
  UNPROTECT(1); // r_dates_class

  // add fts class to R_object
  SEXP r_tseries_class;
  PROTECT(r_tseries_class = allocVector(STRSXP, 2));
  SET_STRING_ELT(r_tseries_class, 0, mkChar("fts"));
  SET_STRING_ELT(r_tseries_class, 1, mkChar("matrix"));
  classgets(R_object, r_tseries_class);
  UNPROTECT(1); // r_tseries_class
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata(SEXP x): R_object(x) {
  PROTECT(R_object);
}

template <typename TDATE,typename TDATA, typename TSDIM>
void R_Backend_TSdata<TDATE,TDATA,TSDIM>::setColnames(const std::vector<std::string>& cnames) {
  if(static_cast<TSDIM>(cnames.size()) != ncols(R_object)) {
    return;
  }
  SEXP dimnames, cnames_sexp;
  int protect_count = 0;

  std::vector<std::string>::const_iterator beg = cnames.begin();
  std::vector<std::string>::const_iterator end = cnames.end();

  PROTECT(cnames_sexp = string2sexp(beg,end));
  ++protect_count;

    // check if we have existing dimnames
  dimnames = getAttrib(R_object, R_DimNamesSymbol);
  if(dimnames == R_NilValue) {
    PROTECT(dimnames = allocVector(VECSXP, 2));
    ++protect_count;
    SET_VECTOR_ELT(dimnames, 0, R_NilValue);
  }
  SET_VECTOR_ELT(dimnames, 1, cnames_sexp);
  setAttrib(R_object, R_DimNamesSymbol, dimnames);
  UNPROTECT(protect_count);
}

template <typename TDATE,typename TDATA, typename TSDIM>
std::vector<std::string> R_Backend_TSdata<TDATE,TDATA,TSDIM>::getColnames() const {
  std::vector<std::string> ans;

  SEXP dimnames = getAttrib(R_object, R_DimNamesSymbol);

  if(dimnames==R_NilValue) {
    return ans;
  }

  SEXP cnames = VECTOR_ELT(dimnames, 1);

  if(cnames==R_NilValue) {
    return ans;
  }
  std::insert_iterator<std::vector<std::string> > insert_iter(ans,ans.begin());
  sexp2string(cnames,insert_iter);

  return ans;
}

template <typename TDATE,typename TDATA, typename TSDIM>
const size_t R_Backend_TSdata<TDATE,TDATA,TSDIM>::getColnamesSize() const {
  return length(getColnames(R_object));
}

template <typename TDATE,typename TDATA, typename TSDIM>
TDATA* R_Backend_TSdata<TDATE,TDATA,TSDIM>::getData() const {
  return R_allocator<TDATA>::R_dataPtr(R_object);
}

template <typename TDATE,typename TDATA, typename TSDIM>
TDATE* R_Backend_TSdata<TDATE,TDATA,TSDIM>::getDates() const {
  return R_allocator<TDATE>::R_dataPtr(getAttrib(R_object,install("dates")));
}

template <typename TDATE,typename TDATA, typename TSDIM>
TSDIM R_Backend_TSdata<TDATE,TDATA,TSDIM>::nrow() const {
  return nrows(R_object);
}

template <typename TDATE,typename TDATA, typename TSDIM>
TSDIM R_Backend_TSdata<TDATE,TDATA,TSDIM>::ncol() const {
  return ncols(R_object);
}

#endif // R_TSERIES_DATA_BACKEND_HPP
