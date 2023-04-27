
#' Calculate the lower quantile for data
#'
#' @param v the data that we want to find quantile
#' @param prob find the lower 100*prob% quantile
#' @param sorted whether v has been sorted
#' @export
#' @examples
#' \dontrun{
#' example_data <- rnorm(500, 0, 5)
#'
#' # Find the lower 0.975 quantile
#' lower.quantile(x, prob = 0.975)
#'
#' # Find the lower 0.025 quantile
#' lower.quantile(x, prob = 0.975)
#' }
#'
lower.quantile <- function(v, prob, sorted=FALSE) {
  v  = c(v, Inf)
  w = rep(1,length(v))
  if (!sorted) { o = order(v); v = v[o]; w = w[o] }
  i = which(cumsum(w/sum(w)) <= prob)
  if (length(i)==0) return(-Inf)
  else return(v[max(i)])
}
