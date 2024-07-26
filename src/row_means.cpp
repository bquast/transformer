#include "row_means.h"

NumericVector row_means(const NumericMatrix &mat)
{
  int nb_Row = mat.nrow();
  NumericVector mean_by_row(nb_Row);

  for(int i = 0; i < nb_Row; i++)
  {
    mean_by_row[i] = Rcpp::mean(mat(i, _));
  }

  return(mean_by_row);
}
