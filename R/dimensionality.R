
#' @export
dimensionality <- function(...) {
  UseMethod("dimensionality")
}

#' @rdname dimensionality
#' @export
dimensionality.default <- function(x, y, measures="all", ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.dimensionality()
  }

  measures <- match.arg(measures, ls.dimensionality(), TRUE)
  colnames(x) <- make.names(colnames(x))

  x <- binarize(x)

  sapply(measures, function(f) {
    eval(call(f, x=x))
  })
}

#' @rdname dimensionality
#' @export
dimensionality.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  dimensionality.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

#' @export
ls.dimensionality <- function() {
  c("T2", "T3", "T4")
}

pca <- function(x) {
  aux <- stats::prcomp(x, scale=TRUE)
  aux <- which(summary(aux)$importance[3,] <= 0.95)
  return(length(aux))
}

T2 <- function(x) {
  nrow(x)/ncol(x)
}

T3 <- function(x) {
  nrow(x)/pca(x)
}

T4 <- function(x) {
  pca(x)/ncol(x)
}
