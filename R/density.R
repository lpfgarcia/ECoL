
#' @export
density <- function(...) {
  UseMethod("density")
}

#' @rdname density
#' @export
density.default <- function(x, y, measures="all", ...) {
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

  if(measures[1] == "all") {
    measures <- ls.density()
  }

  measures <- match.arg(measures, ls.density(), TRUE)

  data <- data.frame(x, class=y)
  data <- binarize(data)
  dst <- dist(data[,-ncol(data)])

  sapply(measures, function(f) {
    eval(call(f, dst=dst, data=data))
  })
}

#' @rdname density
#' @export
density.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  density.default(modFrame[, -1], modFrame[, 1], measures, ...)
}

#' @export
ls.density <- function() {
  c("d1", "d2", "d3")
}

volume <- function(data) {
  data <- data[,-ncol(data), drop=FALSE]
  prod(colMax(data) - colMin(data))
}

d1 <- function(dst, data) {
  volume(data)/nrow(data)
}

d2 <- function(dst, data, k=3) {

  aux <- unlist(
    lapply(rownames(data),
      function(i) {
        tmp <- knn(dst, data, k, i)
        volume(data[names(tmp),])
      })
  )

  return(mean(aux))
}

voting <- function(pred, data, i) {

  if(max(table(pred)) >= 2)
    return(which.max(table(pred)))
  return(data[i,]$class)
}

d3 <- function(dst, data, k=3) {

  aux <- unlist(
    lapply(rownames(data),
      function(i) {
        tmp <- knn(dst, data, k, i)
        voting(tmp, data, i)
    })
  )

  aux <- mean(aux != data$class)
  return(aux)
}

