#ifndef __ROW_MEANS_H__
#define __ROW_MEANS_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// #' @name row_means
//#' @title Row Means
// #' @param x matrix
// #' @return vector with the mean of each of row of the input matrix
// [[Rcpp::export]]
NumericVector row_means(const NumericMatrix &mat);

#endif
