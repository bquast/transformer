#' @name transformer
#' @title Transformer
#' @param x inputs
#' @param d_model dimensions of the model
#' @param num_heads number of heads
#' @param dff dff
#' @param mask optional mask
#' @return output of the transformer layer
#' @export

transformer <- function(x, d_model, num_heads, dff, mask = NULL) {
  attn_output <- multi_head(x, x, x, d_model, num_heads, mask)
  x1 <- layer_norm(x + attn_output)

  ff_output <- feed_forward(x1, dff, d_model)
  x2 <- layer_norm(x1 + ff_output)

  return(x2)
}
