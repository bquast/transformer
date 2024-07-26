#ifndef __INSERT_SUB_MAT_H__
#define __INSERT_SUB_MAT_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
void insert_sub_mat(NumericMatrix &mat, const NumericMatrix &sub_Mat, const IntegerVector &col_Index);

#endif
