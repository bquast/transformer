#' @name layer_norm
#' @title Layer Normalization
#' @param x inputs
#' @param epsilon scale
#' @return outputs of layer normalization

layer_norm <- function(x, epsilon = 1e-6) {
  mu <- row_means(x)
  sigma_sq <- row_vars(x)
  normalized_x <- t((t(x) - mu) / sqrt(sigma_sq + epsilon))
  return(normalized_x)
}
