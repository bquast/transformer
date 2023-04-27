#' @name multi_head
#' @title Multi-Headed Attention
#' @param Q queries
#' @param K keys
#' @param V values
#' @param d_model dimensions of the model
#' @param num_heads number of heads
#' @param mask optional mask
#' @return multi-headed attention outputs

multi_head <- function(Q, K, V, d_model, num_heads, mask = NULL) {
  depth <- d_model / num_heads

  WQ <- matrix(rnorm(d_model * d_model), d_model, d_model)
  WK <- matrix(rnorm(d_model * d_model), d_model, d_model)
  WV <- matrix(rnorm(d_model * d_model), d_model, d_model)

  Q <- Q %*% WQ
  K <- K %*% WK
  V <- V %*% WV

  Qs <- lapply(1:num_heads, function(i) Q[, ((i - 1) * depth + 1):(i * depth)])
  Ks <- lapply(1:num_heads, function(i) K[, ((i - 1) * depth + 1):(i * depth)])
  Vs <- lapply(1:num_heads, function(i) V[, ((i - 1) * depth + 1):(i * depth)])

  outputs <- lapply(1:num_heads, function(i) {
    attention(Qs[[i]], Ks[[i]], Vs[[i]], mask)
  })

  concat_attention <- do.call(cbind, lapply(outputs, function(x) x[[1]]))

  WO <- matrix(rnorm(d_model * d_model), d_model, d_model)
  output <- concat_attention %*% WO

  return(output)
}
