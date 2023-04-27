## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE-----------------------------------------------------------
# devtools::install()

## ----setup--------------------------------------------------------------------
library(vhdi)

## -----------------------------------------------------------------------------
set.seed(100723)
## simulate data from normal distribution as an example
x <- rnorm(1000)
alpha <- 0.05
shortest.pi(x, alpha)

## -----------------------------------------------------------------------------
## random positioning
random.pi(x, alpha)

## specify equal tailed positioning
random.pi(x, alpha, beta = alpha/2)

## specify any positioning
random.pi(x, alpha, alpha/10)

## -----------------------------------------------------------------------------
conservative.pi(x, alpha)

## -----------------------------------------------------------------------------
cv.pi(x, alpha, K = 5)

## ---- fig.width=6, fig.height=4-----------------------------------------------
# more details in ?get.interval
# Example: generate the samples from standard normal distribution
# use cross validation method to get the prediction interval using the generated sample
res = get.interval(METHOD = 3, DIST = "Normal", n = 1000)

# things needed for the plot below
lower_q = res$interval[1]
upper_q = res$interval[2]
data = res$data
# A histogram of the sample with a shaded area representing the range of the prediction interval
# inputs: sample data, lower bound of the prediction interval, upper bound of the prediction interval 
# output: a plot
h = hist(data, breaks=50, plot=FALSE)
cuts = cut(h$breaks, c(lower_q, upper_q))
plot(h$breaks, c(h$counts,0) ,type="s",col="black", lwd=2)
plot(h, col="gray"[cuts], lty="blank", add=T)

## ---- fig.width=8, fig.height=6-----------------------------------------------

n <- 1000 ## training sample size
n0 <- 1000 ## test sample size
alpha <- 0.05 ## nominal error rate
n_exp = 100 # number of experiments
n_method  = 4 # number of methods
cov.box = matrix(nrow = n_exp, ncol = n_method)
len.box = matrix(nrow = n_exp, ncol = n_method)
for(i in 1:n_exp){
  beta  = runif(1, 0, alpha)
  x = runif(n)
  x0  = runif(n0)
  
  res = shortest.pi(x, alpha)
  shortest_lower_q = res[1]
  shortest_upper_q = res[2]
  coverage.x0 = mean(shortest_lower_q<= x0 & shortest_upper_q >=x0)
  cov.box[i, 1] = coverage.x0
  len.box[i,1] = mean(shortest_upper_q - shortest_lower_q)
  
  res = random.pi(x, alpha, beta)
  random_lower_q = res[1]
  random_upper_q = res[2]
  coverage.x0 = mean(random_lower_q<= x0 & random_upper_q >=x0)
  cov.box[i,2] = coverage.x0
  len.box[i,2] = mean(random_upper_q - random_lower_q)
  
  res = cv.pi(x, alpha, K = 5)
  cv_lower_q = res[1]
  cv_upper_q = res[2]
  coverage.x0 = mean(cv_lower_q<= x0 & cv_upper_q >=x0)
  cov.box[i,3] = coverage.x0
  len.box[i,3] = mean(cv_upper_q - cv_lower_q)
  
  res = conservative.pi(x, alpha)
  cons_lower_q = res[1]
  cons_upper_q = res[2]
  coverage.x0 = mean(cons_lower_q<= x0 & cons_upper_q >=x0)
  cov.box[i,4] = coverage.x0
  len.box[i,4] = mean(cons_upper_q - cons_lower_q)
}

boxplot(cov.box, main = paste("Coverage", expression(alpha), '=', alpha, 'Uniform'), names = c("shortest", "random", "cross validation", "conservative"))
abline(h = 1-alpha, col = 'red')

boxplot(len.box, main = paste("Length", expression(alpha), '=', alpha, 'Uniform'), names = c("shortest", "random", "cross validation", "conservative"))

## ---- fig.width=8, fig.height=6-----------------------------------------------
n <- 1000 ## training sample size
n0 <- 1000 ## test sample size
alpha <- 0.05 ## nominal error rate
n_exp = 100 # number of experiments
n_method  = 4 # number of methods
cov.box = matrix(nrow = n_exp, ncol = n_method)
len.box = matrix(nrow = n_exp, ncol = n_method)
for(i in 1:n_exp){
  beta  = runif(1, 0, alpha)
  x = rnorm(n, 0, 0.01)
  x0  = rnorm(n0, 0, 0.01)
  
  res = shortest.pi(x, alpha)
  shortest_lower_q = res[1]
  shortest_upper_q = res[2]
  coverage.x0 = mean(shortest_lower_q<= x0 & shortest_upper_q >=x0)
  cov.box[i, 1] = coverage.x0
  len.box[i,1] = mean(shortest_upper_q - shortest_lower_q)
  
  res = random.pi(x, alpha, beta)
  random_lower_q = res[1]
  random_upper_q = res[2]
  coverage.x0 = mean(random_lower_q<= x0 & random_upper_q >=x0)
  cov.box[i,2] = coverage.x0
  len.box[i,2] = mean(random_upper_q - random_lower_q)
  
  res = cv.pi(x, alpha, K = 5)
  cv_lower_q = res[1]
  cv_upper_q = res[2]
  coverage.x0 = mean(cv_lower_q<= x0 & cv_upper_q >=x0)
  cov.box[i,3] = coverage.x0
  len.box[i,3] = mean(cv_upper_q - cv_lower_q)
  
  res = conservative.pi(x, alpha)
  cons_lower_q = res[1]
  cons_upper_q = res[2]
  coverage.x0 = mean(cons_lower_q<= x0 & cons_upper_q >=x0)
  cov.box[i,4] = coverage.x0
  len.box[i,4] = mean(cons_upper_q - cons_lower_q)
}

boxplot(cov.box, main = paste("Coverage", expression(alpha), '=', alpha, 'Normal'), names = c("shortest", "random", "cross validation", "conservative"))
abline(h = 1-alpha, col = 'red')

boxplot(len.box, main = paste("Length", expression(alpha), '=', alpha, 'Normal'), names = c("shortest", "random", "cross validation", "conservative"))

## ---- fig.width=8, fig.height=6-----------------------------------------------
n <- 1000 ## training sample size
n0 <- 1000 ## test sample size
alpha <- 0.05 ## nominal error rate
n_exp = 100 # number of experiments
n_method  = 4 # number of methods
cov.box = matrix(nrow = n_exp, ncol = n_method)
len.box = matrix(nrow = n_exp, ncol = n_method)
for(i in 1:n_exp){
  beta  = runif(1, 0, alpha)
  x = rlnorm(n)
  x0  = rlnorm(n0)
  
  res = shortest.pi(x, alpha)
  shortest_lower_q = res[1]
  shortest_upper_q = res[2]
  coverage.x0 = mean(shortest_lower_q<= x0 & shortest_upper_q >=x0)
  cov.box[i, 1] = coverage.x0
  len.box[i,1] = mean(shortest_upper_q - shortest_lower_q)
  
  res = random.pi(x, alpha, beta)
  random_lower_q = res[1]
  random_upper_q = res[2]
  coverage.x0 = mean(random_lower_q<= x0 & random_upper_q >=x0)
  cov.box[i,2] = coverage.x0
  len.box[i,2] = mean(random_upper_q - random_lower_q)
  
  res = cv.pi(x, alpha, K = 5)
  cv_lower_q = res[1]
  cv_upper_q = res[2]
  coverage.x0 = mean(cv_lower_q<= x0 & cv_upper_q >=x0)
  cov.box[i,3] = coverage.x0
  len.box[i,3] = mean(cv_upper_q - cv_lower_q)
  
  res = conservative.pi(x, alpha)
  cons_lower_q = res[1]
  cons_upper_q = res[2]
  coverage.x0 = mean(cons_lower_q<= x0 & cons_upper_q >=x0)
  cov.box[i,4] = coverage.x0
  len.box[i,4] = mean(cons_upper_q - cons_lower_q)
}

boxplot(cov.box, main = paste("Coverage", expression(alpha), '=', alpha, 'Log normal'), names = c("shortest", "random", "cross validation", "conservative"))
abline(h = 1-alpha, col = 'red')

boxplot(len.box, main = paste("Length", expression(alpha), '=', alpha, 'Log normal'), names = c("shortest", "random", "cross validation", "conservative"))

