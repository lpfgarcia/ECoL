
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

  dimensionality.default(modFrame[, -1], modFrame[, 1], measures, ...)
}

#' @export
ls.dimensionality <- function() {
  c("m1", "m2", "m3", "c1")
}

pca <- function(data) {
  aux <- prcomp(data[,-ncol(data)], scale=TRUE)$sdev
  aux <- which(cumsum(aux)/sum(aux) >= 0.95)
  return(aux[1])
}

m1 <- function(data) {
  (ncol(data)-1)/nrow(data)
}

m2 <- function(data) {
  pca(data)/nrow(data)
}

m3 <- function(data) {
  pca(data)/(ncol(data)-1)
}

c1 <- function(data) {
  c <- (-1/log(nlevels(data$class)))
  i <- table(data$class)/nrow(data)
  aux <- c*sum(i*log(i))
  return(aux)
}

