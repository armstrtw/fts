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

    TSeries<double,ansType,int,R_Backend_TSdata,PosixDate> ans = TSeries<double,ansType,int,R_Backend_TSdata,PosixDate>(movingTS.nrow() - (p - 1), 1);
    std::copy(movingTS.getDates() + (p - 1), movingTS.getDates()+movingTS.nrow(), ans.getDates());

    std::cout << "ans nrow" << ans.nrow() << std::endl;

    std::vector<std::string> colnames;
    colnames.push_back("analog");
    ans.setColnames(colnames);

    VT* stationary_begin = stationaryTS.getData() + stationaryTS.nrow() - p;
    VT* stationary_end = stationaryTS.getData() + stationaryTS.nrow();
    VT* moving_begin = movingTS.getData() + (p - 1);
    VT* moving_end = movingTS.getData() + movingTS.nrow();
    ansType* ans_data = ans.getData();

    std::cout << "stationary: " << std::distance(stationary_begin,stationary_end) << std::endl;
    std::cout << "moving: " << std::distance(moving_begin,moving_end) << std::endl;

    while(moving_begin != moving_end) {
      *ans_data = tslib::Cor<ansType>::apply(stationary_begin, stationary_end, moving_begin - (p - 1), moving_begin);
      // std::cout << *ans_data << std::endl;
      ++moving_begin;
      ++ans_data;
    }

    return ans.getIMPL()->R_object;
  }
};

#endif // ANALOG_HPP
