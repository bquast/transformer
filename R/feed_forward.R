#' @name feed_forward
#' @title Feed Forward Layer
#' @param x inputs
#' @param dff dimensions of feed-forward model
#' @param d_model dimensions of the model
#' @return output of the feed-forward layer
#' @importFrom stats rnorm

feed_forward <- function(x, dff, d_model) {
  W1 <- matrix(rnorm(d_model * dff), d_model, dff)
  b1 <- matrix(rnorm(1 * dff), 1, dff)
  W2 <- matrix(rnorm(dff * d_model), dff, d_model)
  b2 <- matrix(rnorm(1 * d_model), 1, d_model)

  hidden <- pmax(x %*% W1 + matrix(rep(b1, nrow(x)), nrow(x), ncol(b1), byrow = TRUE), 0)
  output <- hidden %*% W2 + matrix(rep(b2, nrow(x)), nrow(x), ncol(b2), byrow = TRUE)

  return(output)
}
