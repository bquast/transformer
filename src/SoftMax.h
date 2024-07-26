#ifndef __SOFTMAX_H__
#define __SOFTMAX_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector SoftMax(const NumericVector &x);
NumericMatrix SoftMax(const NumericMatrix &x);

#endif
