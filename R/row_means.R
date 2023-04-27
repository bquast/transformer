#' @name row_means
#' @title Row Means
#' @param x matrix
#' @return vector with the mean of each of row of the input matrix
#' @export
#' @examples
#' row_means(t(matrix(1:5)))

row_means <- function(x)
  apply(x, 1, mean)
