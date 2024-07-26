#include "SoftMax.h"

NumericVector SoftMax(const NumericVector &x)
{
  NumericVector exp_x = Rcpp::exp(x - Rcpp::max(x));
  exp_x = exp_x / Rcpp::sum(exp_x);
  return(exp_x);
}

NumericMatrix SoftMax(const NumericMatrix &x)
{
  int nb_Row = x.nrow();
  int nb_Col = x.ncol();
  NumericMatrix x_SoftMax(nb_Row, nb_Col);

  for(int i = 0; i < nb_Row; i++)
  {
    x_SoftMax(i, _) = Rcpp::exp(x(i, _) - Rcpp::max(x(i, _)));
    x_SoftMax(i, _) = x_SoftMax(i, _) / Rcpp::sum(x_SoftMax(i, _));
  }

  return(x_SoftMax);
}
