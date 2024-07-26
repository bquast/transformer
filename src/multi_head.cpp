#include "multi_head.h"
#include "mat_mult.h"
#include "insert_sub_mat.h"
#include "sub_mat.h"
#include "fill_mat_rnorm.h"
#include "attention.h"

NumericMatrix multi_head(const NumericMatrix &Q,
                         const NumericMatrix &K,
                         const NumericMatrix &V,
                         int d_model,
                         int num_heads)
{
  int depth = d_model / num_heads;
  int nb_Row_Q = Q.nrow();
  int nb_Col_Q = Q.ncol();

  NumericMatrix WQ(d_model, d_model);
  NumericMatrix WK(d_model, d_model);
  NumericMatrix WV(d_model, d_model);
  NumericMatrix WO(d_model, d_model);

  fill_mat_rnorm(WQ);
  fill_mat_rnorm(WK);
  fill_mat_rnorm(WV);
  fill_mat_rnorm(WO);

  NumericMatrix Q_WQ = mat_mult(Q, WQ);
  NumericMatrix K_WK = mat_mult(K, WK);
  NumericMatrix V_WV = mat_mult(V, WV);
  NumericMatrix Q_WQ_Sub;
  NumericMatrix K_WK_Sub;
  NumericMatrix V_WV_Sub;

  IntegerVector index(depth);
  NumericMatrix concat_attention(nb_Row_Q, nb_Col_Q);

  for(int i = 0; i < num_heads; i++)
  {
    index = Rcpp::seq(i * depth, ((i + 1) * depth - 1));
    Q_WQ_Sub = sub_mat(Q_WQ, index);
    K_WK_Sub = sub_mat(K_WK, index);
    V_WV_Sub = sub_mat(V_WV, index);
    insert_sub_mat(concat_attention, attention(Q_WQ_Sub, K_WK_Sub, V_WV_Sub), index);
  }

  NumericMatrix output = mat_mult(concat_attention, WO);

  return(output);
}
