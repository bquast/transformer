#include "attention.h"
#include "mat_mult.h"
#include "SoftMax.h"

NumericMatrix attention(const NumericMatrix &Q,
                        const NumericMatrix &K,
                        const NumericMatrix &V)
{
   int dk = K.ncol();
   NumericMatrix scores = mat_mult(Q, transpose(K)) / std::sqrt(dk);
   NumericMatrix attention_weights = SoftMax(scores);
   NumericMatrix output = mat_mult(attention_weights, V);
   return(output);
}
