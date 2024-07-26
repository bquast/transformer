#ifndef __MAT_SUM_H__
#define __MAT_SUM_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix mat_sum(const NumericMatrix &mat1, const NumericMatrix &mat2);

#endif
