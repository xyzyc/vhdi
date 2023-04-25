#' Calculate prediction interval based on random position.
#'
#' @param x data
#' @param alpha nominal error rate
#' @param beta the left tail probability chosen at random. must be less than or equal to alpha.
#' @export
#' @examples
#' \dontrun{
#' example_data <- rnorm(500, 0, 5)
#' random.pi(x, alpha = 0.05, beta = 0.01)
#' }
#'
random.pi <- function(x, alpha, beta = NULL){
  if(is.null(beta)) beta = runif(1, 0, alpha)
  if(beta > alpha) stop("Invalid beta!")
  lower_q = lower.quantile(x, prob = beta, sorted = FALSE)
  upper_q = upper.quantile(x, prob = beta + (1 - alpha), sorted = FALSE)
  PI = c(lower_q, upper_q)
  if(lower_q == -Inf | upper_q == Inf) warning("No finite interval at given rate and random position.")
  return(PI)
}
