context("Test Smoothness")

test_that("regression.result", {

  aux = smoothness(speed ~ ., cars, measures="S1", summary="mean")
  expect_equal(as.numeric(aux$S1), 0.14745010, tolerance=0.1)

  aux = smoothness(speed ~ ., cars, measures="S2", summary="mean")
  expect_equal(as.numeric(aux$S2), 0.09936067)

  aux = smoothness(speed ~ ., cars, measures="S3", summary="mean")
  expect_equal(as.numeric(aux$S3), 0.03390902)

  set.seed(123)
  aux = smoothness(speed ~ ., cars, measures="S4", summary="mean")
  expect_equal(as.numeric(aux$S4), 0.03171115)
})

test_that("validation.error",{

  expect_error(smoothness(speed ~ ., cars, measures="C2"))
  expect_error(smoothness(speed ~ ., cars, measures="C2", summary="abc"))

  expect_error(smoothness(Species ~ ., iris))
  expect_error(smoothness(Species ~ ., iris, measures="C2"))
  expect_error(smoothness(Species ~ ., iris, measures="C2", summary="abc"))
})
