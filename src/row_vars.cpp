#include "row_vars.h"

NumericVector row_vars(const NumericMatrix &mat)
{
  int nb_Row = mat.nrow();
  NumericVector mean_by_row(nb_Row);
  NumericVector var_by_row(nb_Row);

  for(int i = 0; i < nb_Row; i++)
  {
    mean_by_row[i] = Rcpp::mean(mat(i, _));
    var_by_row[i] = Rcpp::mean(mat(i, _) * mat(i, _)) - mean_by_row[i] * mean_by_row[i];
  }

  return(var_by_row);
}
