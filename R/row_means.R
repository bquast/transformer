#' @name row_means
#' @title Row Means
#' @param x matrix
#' @return vector with the mean of each of row of the input matrix
#' @export

row_means <- function(x)
  apply(x, 1, mean)
