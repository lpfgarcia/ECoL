context("Test Balance")

test_that("multiclass1.result", {

  aux = balance(Species ~ ., iris, measures="C1")
  expect_equal(as.numeric(aux$C1), 1)

  aux = balance(Species ~ ., iris, measures="C2")
  expect_equal(as.numeric(aux$C2), 0)
})

test_that("multiclass2.result", {

  class = c(rep(c(1,2), each=40), rep(3, 20)) 
  data = data.frame(x=runif(100), class=factor(class))

  aux = balance(class ~ ., data, measures="C1")
  expect_equal(as.numeric(aux$C1), 0.96022972)

  aux = balance(class ~ ., data, measures="C2")
  expect_equal(as.numeric(aux$C2), 0.05263158)
})

test_that("binary.result", {

  iris = iris[1:100,]
  iris$Species = factor(iris$Species)

  aux = balance(Species ~ ., iris, measures="C1")
  expect_equal(as.numeric(aux$C1), 1)

  aux = balance(Species ~ ., iris, measures="C2")
  expect_equal(as.numeric(aux$C2), 0)
})

test_that("validation.error",{

  expect_error(balance(Species ~ ., iris, measures="L1"))
  expect_error(balance(Species ~ ., iris, measures="L2", summary="abc"))

  expect_error(overlapping(speed ~ ., cars))
  expect_error(balance(speed ~ ., cars, measures="L1"))
  expect_error(balance(speed ~ ., cars, measures="L2", summary="abc"))
})
