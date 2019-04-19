context("Test Summarization")

test_that("basic1.result", {

  aux = summarization(rep(1, 10), summary=c("min", "max", "mean","median"))
  expect_equal(as.numeric(aux), c(1, 1, 1, 1))
})

test_that("basic2.result", {

  aux = summarization(1:9, summary=c("min", "max", "mean","median"))
  expect_equal(as.numeric(aux), c(1, 9, 5, 5))
})

test_that("basic3.result", {

  set.seed(123)
  aux = summarization(rnorm(10000), summary=c("mean", "sd"))
  expect_equal(as.numeric(aux), c(0, 1), tolerance=0.1)
})

test_that("skewness.result", {

  set.seed(123)
  aux = summarization(runif(100), summary="skewness")
  expect_equal(as.numeric(aux), 0.03363101, tolerance=0.1)
})

test_that("kurtosis.result", {

  set.seed(123)
  aux = summarization(runif(100), summary="kurtosis")
  expect_equal(as.numeric(aux), -1.251052, tolerance=0.1)
})

test_that("quantiles.result", {

  aux = summarization(1:10, summary="quantiles")
  expect_equal(as.numeric(aux), c(1, 3, 5, 8, 10))
})

test_that("iqr.result", {

  aux = summarization(rep(1, 10), summary="iqr")
  expect_equal(as.numeric(aux), 0)

  aux = summarization(1:10, summary="iqr")
  expect_equal(as.numeric(aux), 4.5)
})

test_that("validation.error",{
  expect_error(summarization(c(1,2), multiple=FALSE))
})
