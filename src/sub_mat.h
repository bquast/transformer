#ifndef __SUB_MAT_H__
#define __SUB_MAT_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix sub_mat(const NumericMatrix &mat, const IntegerVector &col_Index);

#endif
