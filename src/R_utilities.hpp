#ifndef R_UTILTIES_HPP
#define R_UTILTIES_HPP

#include <vector>
#include <string>
#include <Rinternals.h>

using std::vector;
using std::string;

void addPOSIXattributes(SEXP x);
void addFtsClass(SEXP x);
void addDates(SEXP r_object,SEXP r_dates);
void setDates(SEXP x, SEXP dates);
SEXP getDatesSEXP(const SEXP x);
SEXP string2sexp(const vector<string>& x);
void setColnamesMatrix(const SEXP x, const vector<string>& s);
void getColnamesMatrix(const SEXP x, vector<string>& ans);

#endif // R_UTILTIES_HPP
