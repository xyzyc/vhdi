# Generated from rdhi.Rmd: do not edit by hand

#' Cross validation for the boost of shortest prediction interval  
#' 
#' @param x data
#' @param alpha nominal error rate
#' @param K fold
#' @importFrom lava foldr
#' 
#' @export
cv.pi <- function(x, alpha, K = 2){
  n = length(x)
  t.iter = seq(0, min(1.12*sqrt(alpha/n), alpha - 4/n), 
               by = min(1.12*sqrt(alpha/n), alpha - 4/n)/10)
  folds = foldr(n, K, rep=1)
  pre.cov = 0
  best.rate = t.iter[1]
  for(j in 1:length(t.iter)){
    nominal.rate = alpha - t.iter[j]
    for(k in 1:K){
      fold = as.vector(folds[[1]][[k]])
      xtest = x[fold]
      xtrain  = x[-fold]
      PI = shortest.pi(xtrain, nominal.rate)
      cov.k = sum(PI[1]<= xtest & PI[2] >=xtest)
    }
    cov.k  = cov.k/n
    if(abs(cov.k-1+alpha)< abs(pre.cov-1+alpha)){# more accurate
      best.rate = t.iter[j]
      pre.cov = cov.k
    }
  }
  PI = shortest.pi(x, alpha-best.rate)
  return(PI)
}
