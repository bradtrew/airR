library(airR)
context("Testing accuracy function")

test_that("accuracy returns correct answer", {
  expect_equal(accuracy(c(1)  , c(1))  , 1)
  expect_equal(accuracy(c(0)  , c(0))  , 1)
  expect_equal(accuracy(c(1)  , c(0))  , 0)
  expect_equal(accuracy(c(0)  , c(1))  , 0)
  expect_equal(accuracy(c(1,1), c(1,0)), 0.5)
})
