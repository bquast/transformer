#include "fill_mat_rnorm.h"

void fill_mat_rnorm(NumericMatrix &mat)
{
  int nb_Row = mat.nrow();
  int nb_Col = mat.ncol();

  for(int i = 0; i < nb_Col; i++)
  {
    mat(_, i) = Rcpp::rnorm(nb_Row);
  }
}
