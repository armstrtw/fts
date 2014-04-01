#ifndef FTS_FACTORY_HPP
#define FTS_FACTORY_HPP

#include <stdexcept>
#include <cstring>

#define R_NO_REMAP
#include <Rinternals.h>

enum class DatePolicyT { dateT, posixT, unknownDateTypeT };

class TsTypeTuple {
public:
  static DatePolicyT getIndexPolicyType(SEXP x) {
    SEXP klass = Rf_getAttrib(x,R_ClassSymbol);
    if(klass==R_NilValue) return DatePolicyT::unknownDateTypeT;
    if(strcmp(CHAR(STRING_ELT(klass,0)),"Date")==0) return DatePolicyT::dateT;
    if(strcmp(CHAR(STRING_ELT(klass,0)),"POSIXct")==0) return DatePolicyT::posixT;
    if(Rf_length(klass)>1 && strcmp(CHAR(STRING_ELT(klass,1)),"POSIXct")==0) return DatePolicyT::posixT;
    return DatePolicyT::unknownDateTypeT;
  }
  SEXPTYPE dateSEXPTYPE;
  SEXPTYPE dataSEXPTYPE;
  DatePolicyT datePolicy;
  TsTypeTuple(SEXP x):
  dateSEXPTYPE(TYPEOF(Rf_getAttrib(x,Rf_install("index")))),
  dataSEXPTYPE(TYPEOF(x)),
  datePolicy(getIndexPolicyType(Rf_getAttrib(x,Rf_install("index"))))
  {
    if(Rf_getAttrib(x,Rf_install("index"))==R_NilValue) {
      REprintf("Object has no index.");
    }
  }

  bool operator==(const TsTypeTuple& other) const {
    if(dateSEXPTYPE == other.dateSEXPTYPE &&
       dataSEXPTYPE == other.dataSEXPTYPE &&
       datePolicy == other.datePolicy) {
      return true;
    } else {
      return false;
    }
  }
  bool operator!=(const TsTypeTuple& other) const {
    return !(*this == other);
  }
};

#endif // FTS_FACTORY_HPP
