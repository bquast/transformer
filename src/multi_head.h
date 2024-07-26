#ifndef __MULTI_HEAD_H__
#define __MULTI_HEAD_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix multi_head(const NumericMatrix &Q,
                         const NumericMatrix &K,
                         const NumericMatrix &V,
                         int d_model,
                         int num_heads);

#endif
