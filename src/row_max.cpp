#include "row_max.h"

NumericVector row_max(const NumericMatrix &mat)
{
  int nb_Row = mat.nrow();
  NumericVector max_by_row(nb_Row);

  for(int i = 0; i < nb_Row; i++)
  {
    max_by_row[i] = Rcpp::max(mat(i, _));
  }

  return(max_by_row);
}
