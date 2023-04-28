test_that("Distribution input", {
  expect_error(get.interval(DIST="x"))
})

test_that("Method input", {
  expect_error(get.interval(METHOD=5))
  expect_error(get.interval(METHOD="typo"))
})

test_that("Functions work",{
  expect_no_error(get.interval(DIST = "Normal"))
  expect_no_error(get.interval(DIST = "Exponential"))
  expect_no_error(get.interval(DIST = "Gamma"))
  expect_no_error(get.interval(DIST = "Uniform"))
})

test_that("Methods work",{
  expect_error(get.interval(METHOD="Random Position",alpha=0.05,beta=0.06))
  expect_no_error(get.interval(METHOD="Cross Validation"))
})
