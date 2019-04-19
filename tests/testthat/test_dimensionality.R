context("Test Dimensionality")

test_that("data.result", {

  aux = dimensionality(Species ~ ., iris)
  expect_equal(as.numeric(aux), c(0.02666667, 0.01333333, 0.50000000))

  aux = dimensionality(speed ~ ., cars)
  expect_equal(as.numeric(aux), c(0.02, 0.02, 1))
})


test_that("validation.error",{

  expect_error(dimensionality(Species ~ ., iris, measures="N1"))
  expect_error(dimensionality(Species ~ ., iris, measures="N1", summary="abc"))
})
