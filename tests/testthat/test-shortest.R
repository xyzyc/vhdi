test_that("Numeric input", {
  x<-c(1,2,"three")
  alpha=0.01
  expect_error(shortest.pi(x,alpha))
})

test_that("Data size",{
  x=rnorm(10,0,1)
  expect_warning(shortest.pi(x,alpha=0.05))
})
