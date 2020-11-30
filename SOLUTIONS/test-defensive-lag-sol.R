library(testthat)

context("checking lag")

test_that("does the right thing for sensible inputs", {
  expect_equivalent(lag(c(2, 3, 6), TRUE), lag(c(2, 3, 6), 1))
  expect_equivalent(lag(c(2, 3, 6), FALSE), lag(c(2, 3, 6), 0))
  expect_warning(lag(c(2, 3, 6), 1.4), "rounding")
  expect_equivalent(lag(c(2, 3, 6), 1.3), lag(c(2, 3, 6), 1))
  expect_equivalent(lag(c(2, 3, 6), 1.7), lag(c(2, 3, 6), 2))
})

test_that("does the right thing for problematic inputs", {
  expect_error(lag(data.frame(cbind(1, 2, 5, 4), nrow = 2)))
  expect_error(lag(-1))
  expect_error(lag(1, -2))
  expect_error(lag(Inf))
  expect_error(lag(1, Inf))
  expect_error(lag("a"))
  expect_error(lag(2, "a"))
  expect_error(lag(NULL))
  expect_error(lag(1, NULL))
  expect_error(lag(c(NA, 2, 3)))
  expect_error(lag(1, NA))
  expect_error(lag(1, c(1, 2)))
})
