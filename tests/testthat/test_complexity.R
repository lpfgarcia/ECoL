context("Test Complexity")

test_that("multiclass.result", {

  set.seed(123)
  aux = complexity(Species ~ ., iris)

  set.seed(123)
  expect_equal(aux, complexity(iris[1:4], iris[5]))
})

test_that("binary.result", {

  iris = iris[1:100,]
  iris$Species = factor(iris$Species)

  set.seed(123)
  aux = complexity(Species ~ ., iris)

  set.seed(123)
  expect_equal(aux, complexity(iris[1:4], iris[5]))
})

test_that("regression.result", {

  set.seed(123)
  aux = complexity(speed ~ ., cars)

  set.seed(123)
  expect_equal(aux, complexity(cars[2], cars[1]))
})

test_that("validation.error",{

  expect_error(complexity(iris[1:130, 1:4], iris[5]))
  expect_error(complexity(iris[,1:4], factor(c(rep(1, 149), rep(2, 1)))))
  expect_error(complexity(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(complexity(Species ~ ., iris, groups="abc"))
  expect_error(complexity(Species ~ ., runif(100)))
})
