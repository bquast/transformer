#ifndef __FILL_MAT_ROW_H__
#define __FILL_MAT_ROW_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix fill_mat_row(int nb_Row, const NumericVector row_Vec);

#endif
