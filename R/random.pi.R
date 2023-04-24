#' Calculate prediction interval based on random position.
#'
#' @param x data
#' @param alpha nominal error rate
#' @param beta the left tail probability chosen at random. must be less than or equal to alpha.
#' @examples
#' \dontrun{
#' example_data <- rnorm(500, 0, 5)
#' random.pi(x, alpha = 0.05, beta = 0.01)
#' }
#'
random.pi <- function(x, alpha, beta = alpha/2){
  if(is.null(beta)) beta = runif(1, 0, alpha)
  if(beta > alpha) stop("Invalid beta!")
  lower_q = lower.quantile(c(x, Inf), prob = beta, w = rep(1, length(x)+1), sorted = FALSE)
  upper_q = upper.quantile(c(x, Inf), prob = beta + (1 - alpha), w = rep(1, length(x)+1), sorted = FALSE)
  PI = c(lower_q, upper_q)
  return(PI)
}
