context("Test Network")

test_that("multiclass.result", {

  aux = network(Species ~ ., iris, measures="G1")
  expect_equal(as.numeric(aux$G1), 0.8332886)

  aux = network(Species ~ ., iris, measures="G2")
  expect_equal(as.numeric(aux$G2), 0.267977, tolerance=0.01)

  aux = network(Species ~ ., iris, measures="G3", summary="mean")
  expect_equal(as.numeric(aux$G3), 0.8380508, tolerance=0.01)
})

test_that("validation.error",{

  expect_error(network(Species ~ ., iris, measures="T1"))
  expect_error(network(Species ~ ., iris, measures="T1", summary="abc"))

  expect_error(network(speed ~ ., cars))
  expect_error(network(speed ~ ., cars, measures="T1"))
  expect_error(network(speed ~ ., cars, measures="T1", summary="abc"))
})
