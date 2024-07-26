#ifndef __LAYER_NORM_H__
#define __LAYER_NORM_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix layer_norm(const NumericMatrix &x, double epsilon);

#endif
