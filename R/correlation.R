#' Measures of feature correlation
#'
#' These measures calculate the correlation of the values of the features to the
#' outputs. If at least one feature is highly correlated to the output, this 
#' indicates that simpler functions can be fitted to the data.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"C1"}{Maximum feature correlation to the output (C1)}
#'    \item{"C2"}{Average feature correlation to the output (C2)}
#'    \item{"C3"}{Individual feature efficiency (C3)}
#'    \item{"C4"}{Collective feature efficiency (C4)}
#'  }
#' @return A list named by the requested regression correlation measure.
#'
#' @references
#'  Ana C Lorena and Aron I Maciel and Pericles B C Miranda and Ivan G Costa and
#'    Ricardo B C Prudencio. (2018). Data complexity meta-features for 
#'    regression problems. Machine Learning, 107, 1, 209--246.
#'
#' @examples
#' ## Extract all correlation measures
#' data(cars)
#' correlation(speed~., cars)
#' @export
correlation <- function(...) {
  UseMethod("correlation")
}

#' @rdname correlation
#' @export
correlation.default <- function(x, y, measures="all", ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  if(is.factor(y)) {
    stop("label attribute needs to be numeric")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.correlation()
  }

  measures <- match.arg(measures, ls.correlation(), TRUE)
  colnames(x) <- make.names(colnames(x))

  x <- normalize(binarize(x))
  y <- normalize(y)[,1]

  sapply(measures, function(f) {
    eval(call(paste("r", f, sep="."), x=x, y=y))
  })
}

#' @rdname correlation
#' @export
correlation.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  correlation.default(modFrame[, -1,drop=FALSE], modFrame[, 1,drop=FALSE],
    measures, ...)
}

ls.correlation <- function() {
  c("C1", "C2", "C3", "C4")
}

r.C1 <- function(x, y) {
  max(abs(stats::cor(x, y, method="spearman")))
}

r.C2 <- function(x, y) {
  mean(abs(stats::cor(x, y, method="spearman")))
}

r.C3 <- function(x, y, c=0.9) {
  min(apply(x, 2, remove, y, c))
}

r.C4 <- function(x, y, r=0.1) {

  aux <- 0
  rows <- nrow(x)

  repeat {

    aux <- aux + 1
    tmp <- stats::cor(y, x, method="spearman")
    idx <- rev(order(abs(tmp)))[1]

    model <- stats::lm(y ~ x[,idx])
    remove <- abs(model$residuals) > r

    x <- x[remove,,drop=FALSE]
    y <- y[remove]

    if(length(y) <= 1)
      return(0)

    if(sum(!remove) == length(remove) | aux == ncol(x))
      break
  }

  return(length(y)/rows)
}
