#ifndef __PMAX_MAT_H__
#define __PMAX_MAT_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix pmax_mat(const NumericMatrix &mat);

#endif
