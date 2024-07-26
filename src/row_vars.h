#ifndef __ROW_VARS_H__
#define __ROW_VARS_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector row_vars(const NumericMatrix &mat);

#endif
