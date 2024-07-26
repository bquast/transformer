#ifndef __MAT_MULT_H__
#define __MAT_MULT_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix mat_mult(const NumericMatrix &mat1, const NumericMatrix &mat2);

#endif
