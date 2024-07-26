#include "pmax_mat.h"

NumericMatrix pmax_mat(const NumericMatrix &mat)
{
  int nb_Row = mat.nrow();
  int nb_Col = mat.ncol();
  NumericMatrix mat_Max(nb_Row, nb_Col);

  for(int i = 0; i < nb_Row; i++)
  {
    for(int j = 0; j < nb_Col; j++)
    {
      mat_Max(i, j) = std::max(mat(i, j), 0.0);
    }
  }

  return(mat_Max);
}
