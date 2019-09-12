context("Test Linearity")

test_that("multiclass.result", {

  aux = linearity(Species ~ ., iris, measures="L1", summary="mean")
  expect_equal(as.numeric(aux$L1), 0.004335693)

  aux = linearity(Species ~ ., iris, measures="L2", summary="mean")
  expect_equal(as.numeric(aux$L2), 0.013333333)

  set.seed(123)
  aux = linearity(Species ~ ., iris, measures="L3", summary="mean")
  expect_equal(as.numeric(aux$L3), 0.01)
})

test_that("binary1.result", {

  iris = iris[1:100,]
  iris$Species = factor(iris$Species)

  aux = linearity(Species ~ ., iris, measures="L1", summary="mean")
  expect_equal(as.numeric(aux$L1), 0)

  aux = linearity(Species ~ ., iris, measures="L2", summary="mean")
  expect_equal(as.numeric(aux$L2), 0)

  set.seed(123)
  aux = linearity(Species ~ ., iris, measures="L3", summary="mean")
  expect_equal(as.numeric(aux$L3), 0)
})

test_that("binary2.result", {

  iris = iris[51:150,]
  iris$Species = factor(iris$Species)

  aux = linearity(Species ~ ., iris, measures="L1", summary="mean")
  expect_equal(as.numeric(aux$L1), 0.01300708)

  aux = linearity(Species ~ ., iris, measures="L2", summary="mean")
  expect_equal(as.numeric(aux$L2), 0.04000000)

  set.seed(123)
  aux = linearity(Species ~ ., iris, measures="L3", summary="mean")
  expect_equal(as.numeric(aux$L3), 0.02000000)
})

test_that("regression.result", {

  aux = linearity(speed ~ ., cars, measures="L1", summary="mean")
  expect_equal(as.numeric(aux$L1), 0.11991838)

  aux = linearity(speed ~ ., cars, measures="L2", summary="mean")
  expect_equal(as.numeric(aux$L2), 0.02167897)

  set.seed(123)
  aux = linearity(speed ~ ., cars, measures="L3", summary="mean")
  expect_equal(as.numeric(aux$L3), 0.01797071)
})

test_that("validation.error",{

  expect_error(linearity(Species ~ ., iris, measures="F3"))
  expect_error(linearity(Species ~ ., iris, measures="F4", summary="abc"))
})
