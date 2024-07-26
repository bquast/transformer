#ifndef __ATTENTION_H__
#define __ATTENTION_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix attention(const NumericMatrix &Q,
                        const NumericMatrix &K,
                        const NumericMatrix &V);


#endif
