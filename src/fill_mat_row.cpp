#include "fill_mat_row.h"

NumericMatrix fill_mat_row(int nb_Row, const NumericVector row_Vec)
{
  int nb_Col = row_Vec.size();
  NumericMatrix mat(nb_Row, nb_Col);

  for(int i = 0; i < nb_Row; i++)
  {
    mat(i, _) = row_Vec;
  }

  return(mat);
}

