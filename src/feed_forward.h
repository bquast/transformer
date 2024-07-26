#ifndef __FEED_FORWARD_H__
#define __FEED_FORWARD_H__

#include <cmath>
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

//#' @name feed_forward
//#' @title Feed Forward Layer
//#' @param x inputs
//#' @param dff dimensions of feed-forward model
//#' @param d_model dimensions of the model
//#' @return output of the feed-forward layer
// [[Rcpp::export]]
NumericMatrix feed_forward(const NumericMatrix &x, int dff, int d_model);

#endif
