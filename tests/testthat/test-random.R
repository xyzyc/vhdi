test_that("Numeric input", {
  x<-c(1,2,"three")
  alpha=0.01
  expect_error(random.pi(x,alpha))
})

test_that("Beta larger than alpha",{
  x=rnorm(100,0,1)
  expect_error(random.pi(x,alpha=0.05,beta=0.06))
})

test_that("Auto-generate beta",{
  x=rnorm(1000,0,1)
  expect_no_error(random.pi(x,alpha=0.05))
})

test_that("Data size",{
  x=rnorm(10,0,1)
  expect_warning(random.pi(x,alpha=0.05,beta=0.04))
})
