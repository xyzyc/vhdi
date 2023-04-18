# Generated from rdhi.Rmd: do not edit by hand

#' Get the prediction interval using a simulated data set
#'
#' Get the prediction interval by a chosen method using a simulated data set. The simulated data are generated from standard Normal distribution, standard Uniform distribution, standard Exponential distribution or standard Gamma distribution unless more parameters for the distribution are specified.
#'
#' @importFrom stats rexp
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats rgamma
#' @param METHOD a character string or a number giving the method to be used. This must match one of "Random Position", "Shortest", "Cross Validation" or "Conservative", with default "Shortest". 1, 2, 3, 4 refer to  "Random Position", "Shortest", "Cross Validation" and "Conservative" respectively.
#' @param DIST distribution that the simulated data from. This must match one of "Normal", "Uniform", "Exponential", "Gamma", with default "Normal".
#' @param n Number of observations for the simulated data
#' @param test_n Number of observations for the test data
#' @param alpha nominal error rate
#' @param test_data whether generate the test data
#' @param K k-fold cross-validation. Only needed when METHOD is "Cross Validation", with default 2.
#' @param beta a random position. Only needed when METHOD is "Random Position". beta must be less than or equal to alpha.
#' @param shape shape parameter needed for the standard Gamma distribution, with default 5.
#' @param ... arguments passed to or from distributions
#' @returns A list.
#' \item{interval}{prediction interval}
#' \item{data}{simulated data}
#' \item{test_dataset}{test data}
#' @export
#' @examples
#' \dontrun{
#' # Random position method
#' get.interval(METHOD = "Random Position")
#'
#' # Shortest method
#' get.interval(METHOD = "Shortest")
#'
#' # Uniform distribution
#' get.interval(DIST = "Uniform")
#'
#' # Normal distribution
#' get.interval(DIST = "Normal")
#' }
#'
get.interval <- function(METHOD = "Shortest", DIST = "Normal",
                         n = 100, test_n = 100, alpha = 0.05, test_data = F,
                         K = 2, shape = 5, beta, ...){
  if(length(list(...)) == 0){
    if(DIST == "Normal"){
      data = rnorm(n, mean = 0, sd = 1)
      if(test_data) reference_data = rnorm(test_n, mean = 0, sd = 1)
    }else if(DIST == "Uniform"){
      data = runif(n, min = 0, max = 1)
      if(test_data) reference_data = runif(test_n, min = 0, max = 1)
    }else if(DIST == "Exponential"){
      data = rexp(n, rate = 1)
      if(test_data) reference_data = rexp(test_n, rate = 1)
    }else if(DIST == "Gamma"){
      data = rgamma(n, shape = shape, rate = 1)
      if(test_data) reference_data = rgamma(test_n, shape = shape, rate = 1)
    }else{
      stop("DIST is invalid!")
    }
  }else{
    if(DIST == "Normal"){
      data = rnorm(n, ...)
      if(test_data) reference_data = rnorm(test_n, ...)
    }else if(DIST == "Uniform"){
      data = runif(n, ...)
      if(test_data) reference_data = runif(test_n, ...)
    }else if(DIST == "Exponential"){
      data = rexp(n, ...)
      if(test_data) reference_data = rexp(test_n, ...)
    }else if(DIST == "Gamma"){
      data = rgamma(n, ...)
      if(test_data) reference_data = rgamma(test_n, ...)
    }else{
      stop("DIST is invalid!")
    }
  }

  methods = 1:4
  names(methods) = c("Random Position", "Shortest",
                     "Cross Validation", "Conservative")

  METHOD = ifelse(is.character(METHOD), methods[METHOD], METHOD)

  if(METHOD == 1){
      interval = random.pi(data, alpha, beta)
    }else if(METHOD == 2){
      interval = shortest.pi(data, alpha)
    }else if(METHOD == 3){
      interval = cv.pi(data, alpha, K)
    }else if(METHOD == 4){
      interval = conservative.pi(data, alpha)
    }else{
      stop("METHOD is invalid!")
    }

  if(test_data){
    return(list(interval = interval, data = data, test_dataset = reference_data))
  }else{
    return(list(interval = interval, data = data))
  }
}
