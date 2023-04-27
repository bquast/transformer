#' @name row_vars
#' @title Row Variances
#' @param x matrix
#' @return vector with the variance of each of row of the input matrix
#' @export
#' @examples
#' row_vars(t(matrix(1:5)))
row_vars <- function(x)
  row_means((x - row_means(x)) ^ 2)
