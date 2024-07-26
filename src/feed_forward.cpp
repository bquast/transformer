#include "mat_mult.h"
#include "mat_sum.h"
#include "fill_mat_row.h"
#include "fill_mat_rnorm.h"
#include "pmax_mat.h"
#include "feed_forward.h"

NumericMatrix feed_forward(const NumericMatrix &x, int dff, int d_model)
{
  int nb_Row_x = x.nrow();
  NumericMatrix W1(d_model, dff);
  NumericMatrix W2(dff, d_model);
  NumericVector b1 = Rcpp::rnorm(dff);
  NumericVector b2 = Rcpp::rnorm(d_model);
  fill_mat_rnorm(W1);
  fill_mat_rnorm(W2);
  NumericMatrix hidden = mat_sum(mat_mult(x, W1), fill_mat_row(nb_Row_x, b1));
  NumericMatrix output = mat_sum(mat_mult(pmax_mat(hidden), W2), fill_mat_row(nb_Row_x, b2));

  return(output);
}
