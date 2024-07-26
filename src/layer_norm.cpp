#include "row_means.h"
#include "row_vars.h"
#include "layer_norm.h"

NumericMatrix layer_norm(const NumericMatrix &x, double epsilon)
{
  NumericVector mu = row_means(x);
  NumericVector sigma_sq = row_vars(x);
  int nb_Row = x.nrow();
  int nb_Col = x.ncol();
  NumericMatrix x_norm(nb_Row, nb_Col);

  for(int i = 0; i < nb_Row; i++)
  {
    x_norm(i, _) = (x(i, _) - mu[i]) / std::pow(sigma_sq[i] + epsilon, 0.5);
  }

  return(x_norm);
}
