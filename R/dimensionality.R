
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

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.dimensionality()
  }

  measures <- match.arg(measures, ls.dimensionality(), TRUE)

  colnames(x) <- make.names(colnames(x))

  data <- data.frame(x, class=y)
  data <- binarize(data)

  sapply(measures, function(f) {
    eval(call(f, data=data))
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

pca <- function(data) {
  aux <- stats::prcomp(data[,-ncol(data)], scale=TRUE)$sdev
  aux <- which(cumsum(aux)/sum(aux) >= 0.95)
  return(aux[1])
}

T2 <- function(data) {
  nrow(data)/(ncol(data)-1)
}

T3 <- function(data) {
  nrow(data)/pca(data)
}

T4 <- function(data) {
  pca(data)/(ncol(data)-1)
}

