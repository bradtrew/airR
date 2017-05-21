library(airR)
library(testthat)

context("Testing overall functionality")

test_that("each function exectutes properly", {
  expect_error(accuracy(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(accuracy(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(fbeta_score(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(fbeta_score(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(f1_score(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(f1_score(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(precision(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(precision(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(recall(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(recall(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(tru_neg_rate(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(tru_neg_rate(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(brier_score(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(brier_score(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(conf_mat(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(conf_mat(numeric(0), c(1,0)), "actual is of length 0")
  expect_error(hamming_dist(c(1,1,1), c(1,0)), "different number of elements")
  expect_error(hamming_dist(numeric(0), c(1,0)), "actual is of length 0")
})



context("Testing accuracy function")
test_that("accuracy returns correct answer", {
  expect_equal(accuracy(c(1)  , c(1))  , 1)
  expect_equal(accuracy(c(0)  , c(0))  , 1)
  expect_equal(accuracy(c(1)  , c(0))  , 0)
  expect_equal(accuracy(c(0)  , c(1))  , 0)
  expect_equal(accuracy(c(1,1), c(1,0)), 0.5)
})



