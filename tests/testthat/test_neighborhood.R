context("Test Neighborhood")

test_that("multiclass.result", {

  aux = neighborhood(Species ~ ., iris, measures="N1", summary="mean")
  expect_equal(as.numeric(aux$N1), 0.106666667)

  aux = neighborhood(Species ~ ., iris, measures="N2", summary="mean")
  expect_equal(as.numeric(aux$N2), 0.1981444, tolerance=0.001)

  aux = neighborhood(Species ~ ., iris, measures="N3", summary="mean")
  expect_equal(as.numeric(aux$N3), 0.060000000)

  set.seed(123)
  aux = neighborhood(Species ~ ., iris, measures="N4", summary="mean")
  expect_equal(as.numeric(aux$N4), 0.00666667)

  aux = neighborhood(Species ~ ., iris, measures="N5", summary="return")
  expect_equal(aux$N5, 0.12)

  aux = neighborhood(Species ~ ., iris, measures="N6", summary="mean")
  expect_equal(as.numeric(aux$N6), 0.816400000)
})

test_that("validation.error",{

  expect_error(neighborhood(Species ~ ., iris, measures="C1"))
  expect_error(neighborhood(Species ~ ., iris, measures="C1", summary="abc"))

  expect_error(neighborhood(speed ~ ., cars))
  expect_error(neighborhood(speed ~ ., cars, measures="C1"))
  expect_error(neighborhood(speed ~ ., cars, measures="C1", summary="abc"))
})
