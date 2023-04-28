test_that("Numeric input", {
x<-c(1,2,"three")
alpha=0.01
expect_error(conservative.pi(x,alpha))
})

test_that("Data size",{
  x=rnorm(290,0,1)
  alpha=0.01
  expect_warning(conservative.pi(x,alpha))
})

