#include "sub_mat.h"

NumericMatrix sub_mat(const NumericMatrix &mat, const IntegerVector &col_Index)
{
  int nb_Index = col_Index.size();
  int nb_Row = mat.nrow();
  NumericMatrix subset_Matrix(nb_Row, nb_Index);

  for(int i = 0; i < nb_Index; i++)
  {
    subset_Matrix(_, i) = mat(_, col_Index[i]);
  }

  return(subset_Matrix);
}
