#include "mat_sum.h"

NumericMatrix mat_sum(const NumericMatrix &mat1, const NumericMatrix &mat2)
{
  const Eigen::Map<Eigen::MatrixXd> ttm1(as<Eigen::Map<Eigen::MatrixXd>>(mat1));
  const Eigen::Map<Eigen::MatrixXd> ttm2(as<Eigen::Map<Eigen::MatrixXd>>(mat2));
  Eigen::MatrixXd sum = ttm1 + ttm2;
  return(wrap(sum));
}
