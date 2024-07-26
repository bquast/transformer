#include "insert_sub_mat.h"

void insert_sub_mat(NumericMatrix &mat, const NumericMatrix &sub_Mat, const IntegerVector &col_Index)
{
  int nb_Col_Index = col_Index.size();

  for(int i = 0; i < nb_Col_Index; i++)
  {
    mat(_, col_Index[i]) = sub_Mat(_, i);
  }
}
