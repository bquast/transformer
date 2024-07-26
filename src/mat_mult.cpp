#include "mat_mult.h"

NumericMatrix mat_mult(const NumericMatrix &mat1, const NumericMatrix &mat2)
{
  const Eigen::Map<Eigen::MatrixXd> ttm1(as<Eigen::Map<Eigen::MatrixXd>>(mat1));
  const Eigen::Map<Eigen::MatrixXd> ttm2(as<Eigen::Map<Eigen::MatrixXd>>(mat2));
  Eigen::MatrixXd prod = ttm1 * ttm2;
  return(wrap(prod));
}
