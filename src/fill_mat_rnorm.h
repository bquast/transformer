#ifndef __FILL_MAT_RNORM_H__
#define __FILL_MAT_RNORM_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
void fill_mat_rnorm(NumericMatrix &mat);

#endif
