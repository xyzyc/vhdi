# Generated from rdhi.Rmd: do not edit by hand

#' Calculate the lower quantile for data with weights
#' 
#' @param v the data that we want to find quantile
#' @param prob find the lower 100*prob% quantile
#' @param w weight for data
#' @param sorted whether v has been sorted
#' 
#' @export
lower.quantile <- function(v, prob, w=NULL, sorted=FALSE) {
  if (is.null(w)) w = rep(1,length(v))
  if (!sorted) { o = order(v); v = v[o]; w = w[o] }
  i = which(cumsum(w/sum(w)) <= prob)
  if (length(i)==0) return(-Inf)
  else return(v[max(i)])
}