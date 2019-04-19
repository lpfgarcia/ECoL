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
  expect_equal(as.numeric(aux$N4), 0.01333333)

  aux = neighborhood(Species ~ ., iris, measures="T1", summary="return")
  aux = length(aux$T1)/nrow(iris)
  expect_equal(aux, 0.12000000)

  aux = neighborhood(Species ~ ., iris, measures="LSC", summary="mean")
  expect_equal(as.numeric(aux$LSC), 0.816400000)
})

test_that("validation.error",{

  expect_error(neighborhood(Species ~ ., iris, measures="C1"))
  expect_error(neighborhood(Species ~ ., iris, measures="C1", summary="abc"))

  expect_error(neighborhood(speed ~ ., cars))
  expect_error(neighborhood(speed ~ ., cars, measures="C1"))
  expect_error(neighborhood(speed ~ ., cars, measures="C1", summary="abc"))
})
