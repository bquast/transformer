#' @name row_vars
#' @title Row Variances
#' @param x matrix
#' @return vector with the variance of each of row of the input matrix
#' @export

row_vars <- function(x)
  row_means((x - row_means(x)) ^ 2)
