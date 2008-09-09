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

#ifndef ANALOG_HPP
#define ANALOG_HPP

#include <vector>
#include <string>
#include <tslib/vector.summary/cor.hpp>
#include <tslib/tseries.hpp>
#include <R.tseries.data.backend.hpp>
#include <Rtype.hpp>

namespace tslib {

  template<typename ReturnType,
           class TDATE,
           class TDATA,
           class TSDIM,
           template<typename,typename,typename> class TSDATABACKEND,
           template<typename> class DatePolicy,
           template<class U, class V, class W, template<typename,typename,typename> class DATABACKEND, template<typename> class DP> class TSeries>
  const TSeries<TDATE,ReturnType,TSDIM,TSDATABACKEND,DatePolicy> analog(const TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy>& stationaryTS,
                                                                        const TSeries<TDATE,TDATA,TSDIM,TSDATABACKEND,DatePolicy>& movingTS,
                                                                        const size_t p) {
    TSeries<double,ReturnType,int,R_Backend_TSdata,PosixDate> ans = TSeries<double,ReturnType,int,R_Backend_TSdata,PosixDate>(movingTS.nrow() - (p - 1), 1);
    std::copy(movingTS.getDates() + (p - 1), movingTS.getDates()+movingTS.nrow(), ans.getDates());

    std::vector<std::string> colnames;
    colnames.push_back("analog");
    ans.setColnames(colnames);

    TDATA* stationary_begin = stationaryTS.getData() + stationaryTS.nrow() - p;
    TDATA* stationary_end = stationaryTS.getData() + stationaryTS.nrow();
    TDATA* moving_begin = movingTS.getData();
    TDATA* moving_end = movingTS.getData() + movingTS.nrow();
    ReturnType* ans_data = ans.getData();

    while(moving_begin != moving_end - (p - 1)) {
      *ans_data = tslib::Cor<ReturnType>::apply(stationary_begin, stationary_end, moving_begin, moving_begin + p);
      ++moving_begin;
      ++ans_data;
    }
    return ans;
  }
} // namespace tslib

template<SEXPTYPE RTYPE>
class analogFunction {
  typedef typename Rtype<RTYPE>::ValueType VT;
  typedef typename corTraits<VT>::ReturnType ansType;
public:
  static SEXP apply(SEXP stationary, SEXP moving, SEXP periods) {

    if(TYPEOF(stationary)!=TYPEOF(moving)) {
      std::cerr << "stationary and moving must be the same type" << std::endl;
      return R_NilValue;
    }

    int p = static_cast<int>(Rtype<INTSXP>::scalar(periods));

    if(p > nrows(stationary) || p > nrows(moving)) {
      std::cerr << "periods is greater than supplied timeseries." << std::endl;
      return R_NilValue;
    }

    if(ncols(stationary) > 1 || ncols(moving) > 1) {
      std::cerr << "don't know which column to use. please re-run using 1 column time series for moving and stationary." << std::endl;
      return R_NilValue;
    }
    
    // build tseries from SEXP
    R_Backend_TSdata<double,VT,int>* stationaryTSData = R_Backend_TSdata<double,VT,int>::init(stationary);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> stationaryTS(stationaryTSData);

    R_Backend_TSdata<double,VT,int>* movingTSData = R_Backend_TSdata<double,VT,int>::init(moving);
    TSeries<double,VT,int,R_Backend_TSdata,PosixDate> movingTS(movingTSData);

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = tslib::analog<ansType>(stationaryTS,movingTS,p);
    return ans.getIMPL()->R_object;
  }
};

#endif // ANALOG_HPP
