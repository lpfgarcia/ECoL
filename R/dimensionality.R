
#' @export
dimensionality <- function(...) {
  UseMethod("dimensionality")
}

#' @rdname dimensionality
#' @export
dimensionality.default <- function(x, y, features="all", ...) {
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
    features <- ls.dimensionality()
  }

  features <- match.arg(features, ls.dimensionality(), TRUE)

  x <- binarize(x)

  sapply(features, function(f) {
    eval(call(f, x=x, y=y))
  })
}

#' @rdname dimensionality
#' @export
dimensionality.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  dimensionality.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' @export
ls.dimensionality <- function() {
  c("m1", "m2", "m3", "c1")
}

pca <- function(x) {
  aux <- prcomp(x, scale=TRUE)$sdev
  aux <- which(cumsum(aux)/sum(aux) >= 0.95)
  return(aux[1])
}

m1 <- function(x, ...) {
  ncol(x)/nrow(x)
}

m2 <- function(x, ...) {
  pca(x)/nrow(x)
}

m3 <- function(x, ...) {
  pca(x)/ncol(x)
}

c1 <- function(x, y, ...) {
  c <- (-1/log(nlevels(y)))
  i <- table(y)/nrow(x)
  c*sum(i*log(i))
}

