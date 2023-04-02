# Generated from rdhi.Rmd: do not edit by hand

#' Calculate shortest prediction interval  
#' 
#' @param x data
#' @param alpha nominal error rate
#' 
#' @export
shortest.pi <- function(x, alpha){
  alpha_iter = seq(alpha/50, alpha, alpha/50)
  j <- 1
  prev_length = Inf
  find_finite  = 0
  #Find the quantiles producing the shortest 1-alpha interval
  while(j < length(alpha_iter)){
    lower_q = lower.quantile(c(x, Inf), prob = alpha_iter[j], w = rep(1, length(x)+1), sorted = FALSE)
    upper_q = upper.quantile(c(x, Inf), prob = alpha_iter[j] + (1 - alpha), w = rep(1, length(x)+1), sorted = FALSE)
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
    stop("No finite interval at given coverage rate")
  }
  return(PI)
}
