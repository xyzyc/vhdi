test_that("Numeric input", {
  x<-c(1,2,"three")
  alpha=0.01
  expect_error(cv.pi(x,alpha))
})

test_that("Shortest pi works",{
  x=rnorm(1000,0,1)
  expect_no_error(shortest.pi(x,alpha=0.05))
})

test_that("Data size",{
  x=rnorm(10,0,1)
  expect_warning(cv.pi(x,alpha=0.05))
})
