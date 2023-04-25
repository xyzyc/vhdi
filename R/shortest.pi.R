
#' Calculate shortest prediction interval
#'
#' @param x data
#' @param alpha nominal error rate
#'
#' @export
#' @examples
#' \dontrun{
#' example_data <- rnorm(500, 0, 5)
#' shortest.pi(example_data, alpha = 0.01)
#' }
#'
shortest.pi <- function(x, alpha){
  alpha_iter = seq(alpha/50, alpha, alpha/50)
  j <- 1
  prev_length = Inf
  find_finite  = 0
  #Find the quantiles producing the shortest 1-alpha interval
  while(j < length(alpha_iter)){
    lower_q = lower.quantile(x, prob = alpha_iter[j],  sorted = FALSE)
    upper_q = upper.quantile(x, prob = alpha_iter[j] + (1 - alpha), sorted = FALSE)
    if((upper_q-lower_q) < prev_length){
      find_finite = 1
      shortest_lower_q = lower_q
      shortest_upper_q = upper_q
      prev_length = upper_q-lower_q
    }
    j = j+1
  }
  if(find_finite){
    PI = c(shortest_lower_q, shortest_upper_q)
  }else {
    warning("No finite interval at given coverage rate, returning min(x) and Inf.")
    PI = c(min(x), Inf)
  }
  return(PI)
}
