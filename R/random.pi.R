# Generated from rdhi.Rmd: do not edit by hand

#' Calculate shortest prediction interval
#'
#' @param x data
#' @param alpha nominal error rate
#' @param beta a random position. beta must be less than or equal to alpha.
#' @export
random.pi <- function(x, alpha, beta){
  if(is.null(beta)) beta = alpha/2
  if(beta > alpha) stop("Invalid beta!")
  lower_q = lower.quantile(c(x, Inf), prob = beta, w = rep(1, length(x)+1), sorted = FALSE)
  upper_q = upper.quantile(c(x, Inf), prob = beta + (1 - alpha), w = rep(1, length(x)+1), sorted = FALSE)
  PI = c(lower_q, upper_q)
  return(PI)
}
