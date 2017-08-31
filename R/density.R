
#' @export
density <- function(...) {
  UseMethod("density")
}

#' @rdname density
#' @export
density.default <- function(x, y, features="all", ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all") {
    features <- ls.density()
  }

  features <- match.arg(features, ls.density(), TRUE)

  x <- binarize(x)
  d <- dist(x)

  sapply(features, function(f) {
    eval(call(f, x=x, y=y, d=d))
  })
}

#' @rdname density
#' @export
density.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  density.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' @expor
ls.density <- function() {
  c("d1", "d2", "d3")
}

volume <- function(x) {
  prod(colMax(x) - colMin(x))
}

d1 <- function(x, d, ...) {
  volume(x)/nrow(x)
}

d2 <- function(x, y, d, ...) {

  aux <- unlist(
    lapply(1:nrow(x), function(i) {
      p <- knn(d, x, y, i)
      volume(x[names(p),])
    })
  )

  return(mean(aux))
}

voting <- function(p, i) {

  if(max(table(p)) >= 2)
    return(which.max(table(p)))
  return(i)
}

d3 <- function(x, y, d, ...) {

  aux <- unlist(
    lapply(1:nrow(x), function(i) {
      p <- knn(d, x, y, i)
      voting(p, y[i])
    })
  )

  return(mean(aux != y))
}

