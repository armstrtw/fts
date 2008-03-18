#ifndef R_TSERIES_DATA_BACKEND_HPP
#define R_TSERIES_DATA_BACKEND_HPP

#include <vector>
#include <string>
#include <Rinternals.h>
#include "sexp.allocator.templates.hpp"
#include "R_utilities.hpp"


using std::vector;
using std::string;

template <typename TDATE,typename TDATA, typename TSDIM = long>
class R_Backend_TSdata {
private:
  int refcount_;
  bool release_data_;

  R_Backend_TSdata();
  R_Backend_TSdata(const R_Backend_TSdata& t); // not allowed
  R_Backend_TSdata(const TSDIM rows, const TSDIM cols);

  R_Backend_TSdata(TDATA* external_data,
                   TDATE* external_dates,
                   const TSDIM rows,
                   const TSDIM cols,
                   const bool release = false);

  R_Backend_TSdata(const SEXP x);

  R_Backend_TSdata& operator=(const R_Backend_TSdata& right);  // not allowed

public:
  SEXP R_object;

  ~R_Backend_TSdata();

  static R_Backend_TSdata* init();
  static R_Backend_TSdata* init(const TSDIM rows, const TSDIM cols);
  static R_Backend_TSdata* init(const SEXP x);

  void attach();
  void detach();

  TSDIM nrow() const;
  TSDIM ncol() const;
  TDATA* getData() const;
  TDATE* getDates() const;
  void setColnames(const vector<string>& cnames);
  vector<string> getColnames() const;
  const size_t getColnamesSize() const;
};


template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::~R_Backend_TSdata() {
  if(release_data_) {
    if(R_object!=R_NilValue) {
      UNPROTECT_PTR(R_object);
    }
  }
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata() {
  refcount_ = 1;
  release_data_ = true;
  R_object = R_NilValue;
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata(const TSDIM rows, const TSDIM cols) {
  SEXP R_dates;

  refcount_ = 1;
  release_data_ = true;

  PROTECT(R_object = R_allocator<TDATA>::Matrix(rows, cols));
  PROTECT(R_dates  = R_allocator<TDATE>::Vector(rows));
  addPOSIXattributes(R_dates);
  setDates(R_object,R_dates);
  addFtsClass(R_object);

  UNPROTECT(1); // dates only
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>::R_Backend_TSdata(SEXP x) {  
  R_object = x;
  refcount_ = 1;
  release_data_ = false;
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>* R_Backend_TSdata<TDATE,TDATA,TSDIM>::init() {
  return new R_Backend_TSdata();
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>* R_Backend_TSdata<TDATE,TDATA,TSDIM>::init(const TSDIM rows, const TSDIM cols) {
  return new R_Backend_TSdata(rows, cols);
}

template <typename TDATE,typename TDATA, typename TSDIM>
R_Backend_TSdata<TDATE,TDATA,TSDIM>* R_Backend_TSdata<TDATE,TDATA,TSDIM>::init(const SEXP x) {
  return new R_Backend_TSdata(x);
}

template <typename TDATE,typename TDATA, typename TSDIM>
void R_Backend_TSdata<TDATE,TDATA,TSDIM>::attach() {
  ++refcount_;
}

template <typename TDATE,typename TDATA, typename TSDIM>
void R_Backend_TSdata<TDATE,TDATA,TSDIM>::detach() {
  if(--refcount_ == 0) {
    delete this;
  }
}

template <typename TDATE,typename TDATA, typename TSDIM>
void R_Backend_TSdata<TDATE,TDATA,TSDIM>::setColnames(const vector<string>& cnames) {
  if(cnames.size() != ncols(R_object)) {
    return;
  }
  setColnamesMatrix(R_object, cnames);
}

template <typename TDATE,typename TDATA, typename TSDIM>
inline
vector<string> R_Backend_TSdata<TDATE,TDATA,TSDIM>::getColnames() const {
  vector<string> ans;
  getColnamesMatrix(R_object,ans);
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
  return R_allocator<TDATE>::R_dataPtr(getDatesSEXP(R_object));
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
