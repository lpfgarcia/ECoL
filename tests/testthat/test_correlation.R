context("Test Correlation")

test_that("regression.result", {

  aux = correlation(speed ~ ., cars, measures="C2", summary="mean")
  expect_equal(as.numeric(aux$C2), 0.8303568, tolerance=0.01)

  aux = correlation(speed ~ ., cars, measures="C3", summary="min")
  expect_equal(as.numeric(aux$C3), 0.0800000)

  aux = correlation(speed ~ ., cars, measures="C4")
  expect_equal(as.numeric(aux$C4), 0.5600000)
})

test_that("validation.error",{

  expect_error(correlation(speed ~ ., cars, measures="S1"))
  expect_error(correlation(speed ~ ., cars, measures="S1", summary="abc"))

  expect_error(correlation(Species ~ ., iris))
  expect_error(correlation(Species ~ ., iris, measures="S1"))
  expect_error(correlation(Species ~ ., iris, measures="S1", summary="abc"))
})
