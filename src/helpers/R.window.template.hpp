// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; tab-width: 8 -*-
#ifndef R_WINDOW_TEMPLATE_HPP
#define R_WINDOW_TEMPLATE_HPP

#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rtype.hpp>
#include <fts.factory.hpp>

using namespace tslib;

template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy,
         template<class> class windowFunction,
         template<class> class windowFunctionTraits>
SEXP windowFun(SEXP x, SEXP periods) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename windowFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  int p = Rtype<INTSXP>::scalar(periods);
  if(p <= 0) {
    REprintf("windowFun: periods is not positive.");
    return R_NilValue;
  }
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template window<ReturnTDATA,windowFunction>(p);
  return ans.getIMPL()->R_object;
}

template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy,
         template<class> class windowFunction,
         template<class> class windowFunctionTraits>
SEXP windowFun(SEXP x_, SEXP y_, SEXP periods) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename windowFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  int p = Rtype<INTSXP>::scalar(periods);
  if(p <= 0) {
    REprintf("windowFun: periods is not positive.");
    return R_NilValue;
  }
  // build tseries from SEXP x_
  TSDATABACKEND<TDATE,TDATA,TSDIM> x_backend(x_);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> x(x_backend);

  // build tseries from SEXP y_
  TSDATABACKEND<TDATE,TDATA,TSDIM> y_backend(y_);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> y(y_backend);

  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = window_function<ReturnTDATA,windowFunction>(x,y,p);
  return ans.getIMPL()->R_object;
}

template<typename TDATE, typename TDATA,
         typename TSDIM,
         template<typename,typename,typename> class TSDATABACKEND,
         template<typename> class DatePolicy,
         template<class> class windowFunction,
         template<class> class windowFunctionTraits,
         template<class, template<typename> class> class PFUNC>
SEXP timeWindowFun(SEXP x) {
  // define our answer type based on windowFunctionTraits return type
  typedef typename windowFunctionTraits<TDATA>::ReturnType ReturnTDATA;
  // build tseries from SEXP x
  TSDATABACKEND<TDATE,TDATA,TSDIM> tsData(x);
  TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy> ts(tsData);
  TSeries<TDATE,ReturnTDATA,TSDIM,TSDATABACKEND,DatePolicy> ans = ts.template time_window<ReturnTDATA,windowFunction,PFUNC>();
  return ans.getIMPL()->R_object;
}

template<SEXPTYPE RTYPE>
class r_window_2args {
  typedef typename Rtype<RTYPE>::ValueType VT;
public:
  template<template<class> class windowFunction, template<class> class windowFunctionTraits>
  static SEXP apply(SEXP x, SEXP y, SEXP periods) {
    if(TYPEOF(x)!=TYPEOF(y)) {
      Rprintf("x and y must be the same type.");
      return R_NilValue;
    }

    // define our answer type based on windowFunctionTraits return type
    typedef typename windowFunctionTraits<VT>::ReturnType ansType;

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    // build tseries from SEXP
    PosixBackend<double,VT,int> tsData1(x);
    TSeries<double,VT,int,PosixBackend,PosixDate> ts1(tsData1);

    PosixBackend<double,VT,int> tsData2(y);
    TSeries<double,VT,int,PosixBackend,PosixDate> ts2(tsData2);

    TSeries<double,ansType,int,PosixBackend,PosixDate> ans = window_function<ansType,windowFunction>(ts1,ts2,p);

    return ans.getIMPL()->R_object;
  }
};


template<template<class> class windowFunction, template<class> class windowFunctionTraits>
SEXP windowSpecializer(SEXP x, SEXP periods) {
  if(TYPEOF(periods)!=INTSXP) {
    REprintf("windowSpecializer: periods is not an integer.");
  };
  const TsTypeTuple tsTypeInfo(x);
  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,periods);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("windowSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}

template<template<class> class windowFunction, template<class> class windowFunctionTraits>
SEXP windowSpecializer(SEXP x, SEXP y, SEXP periods) {
  if(TYPEOF(periods)!=INTSXP) {
    REprintf("windowSpecializer: periods is not an integer.");
  };
  const TsTypeTuple tsTypeInfoX(x);
  const TsTypeTuple tsTypeInfoY(y);
  if(tsTypeInfoX!=tsTypeInfoY) {
    REprintf("windowSpecializer_2args: x and y must be same time series types.");
    return R_NilValue;
  }

  if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy== DatePolicyT::dateT) {
    return windowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==REALSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==REALSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==INTSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else if(tsTypeInfoX.dateSEXPTYPE==INTSXP && tsTypeInfoX.dataSEXPTYPE==LGLSXP && tsTypeInfoX.datePolicy==DatePolicyT::posixT) {
    return windowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits>(x,y,periods);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("windowSpecializer_2args: unable to classify time series.");
    return R_NilValue;
  }
}

template<template<class> class windowFunction, template<class> class windowFunctionTraits,template<class, template<typename> class> class PFUNC>
SEXP timeWindowSpecializer(SEXP x) {
  const TsTypeTuple tsTypeInfo(x);
  if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return timeWindowFun<double,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return timeWindowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return timeWindowFun<double,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return timeWindowFun<int,double,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return timeWindowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy== DatePolicyT::dateT) {
    return timeWindowFun<int,int,R_len_t,JulianBackend,JulianDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return timeWindowFun<double,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return timeWindowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==REALSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return timeWindowFun<double,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==REALSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return timeWindowFun<int,double,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==INTSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return timeWindowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else if(tsTypeInfo.dateSEXPTYPE==INTSXP && tsTypeInfo.dataSEXPTYPE==LGLSXP && tsTypeInfo.datePolicy==DatePolicyT::posixT) {
    return timeWindowFun<int,int,R_len_t,PosixBackend,PosixDate,windowFunction,windowFunctionTraits,PFUNC>(x);
  } else {
    //throw std::logic_error("unable to classify time series.");
    REprintf("timeWindowSpecializer: unable to classify time series.");
    return R_NilValue;
  }
}


#endif // R_WINDOW_TEMPLATE_HPP
