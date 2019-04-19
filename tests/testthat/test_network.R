context("Test Network")

test_that("multiclass.result", {

  aux = network(Species ~ ., iris, measures="Density")
  expect_equal(as.numeric(aux$Density), 0.8332886)

  aux = network(Species ~ ., iris, measures="ClsCoef")
  expect_equal(as.numeric(aux$ClsCoef), 0.267977, tolerance=0.01)

  aux = network(Species ~ ., iris, measures="Hubs", summary="mean")
  expect_equal(as.numeric(aux$Hubs), 0.8380508, tolerance=0.01)
})

test_that("validation.error",{

  expect_error(network(Species ~ ., iris, measures="T1"))
  expect_error(network(Species ~ ., iris, measures="T1", summary="abc"))

  expect_error(network(speed ~ ., cars))
  expect_error(network(speed ~ ., cars, measures="T1"))
  expect_error(network(speed ~ ., cars, measures="T1", summary="abc"))
})
