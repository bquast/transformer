#' @name softmax
#' @title SoftMax function
#' @param x number inputs
#' @return the softmax applied to the inputs
#' @export

softmax <- function(x) {
  exp_x <- exp(x - max(x))
  exp_x / sum(exp_x)
}
