#' Measures of linearity
#'
#' Regression task. These measures capture whether a linear function provides a 
#' good fit to the problem. If this is the case, the problem can be considered 
#' simpler than one in which a non-linear function is required.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A response vector with one value for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the output column.
#' @param data A data.frame dataset contained the input and output attributes.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"L1"}{Mean absolute error (L1) averages the absolute values of the 
#'      residues of a multiple linear regressor.}
#'    \item{"L2"}{Residuals variance (L2) averages the square of the residuals 
#'      from a multiple linear regression.}
#'    \item{"L3"}{Non-linearity of a linear regressor (L3) measures how 
#'      sensitive the regressor is to the new randomly interpolated points.}
#'  }
#' @return A list named by the requested linearity measure.
#'
#' @references
#'  Ana C Lorena and Aron I Maciel and Pericles B C Miranda and Ivan G Costa and
#'    Ricardo B C Prudencio. (2018). Data complexity meta-features for 
#'    regression problems. Machine Learning, 107, 1, 209--246.
#'
#' @examples
#' ## Extract all regression linearity measures
#' data(cars)
#' linearity_regr(speed~., cars)
#' @export
linearity_regr <- function(...) {
  UseMethod("linearity_regr")
}

#' @rdname linearity_regr
#' @export
linearity_regr.default <- function(x, y, measures="all", 
                                   summary=c("mean", "sd"), ...) {

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
    measures <- ls.linearity_regr()
  }

  measures <- match.arg(measures, ls.linearity_regr(), TRUE)

  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  x <- normalize(x)
  y <- normalize(y)[,1]

  x <- x[order(y), ,drop=FALSE]
  y <- y[order(y)]

  m <- stats::lm(y ~ ., cbind(y=y, x))

  sapply(measures, function(f) {
    measure = eval(call(paste("r", f, sep="."), m=m, x=x, y=y))
    post.processing(measure, summary, f %in% ls.linearity_regr.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname linearity_regr
#' @export
linearity_regr.formula <- function(formula, data, measures="all", 
                                   summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity_regr.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

ls.linearity_regr <- function() {
  c("L1", "L2", "L3")
}

ls.linearity_regr.multiples <- function() {
  ls.linearity_regr()
}

r.L1 <- function(m, ...) {
  #mean(abs(m$residuals))
  abs(m$residuals)
}

r.L2 <- function(m, ...) {
  #mean(m$residuals^2)
  mean(m$residuals^2)
}

r.L3 <- function(m, x, y) {
  test <- r.generate(x, y, nrow(x))
  pred <- stats::predict.lm(m, test[, -ncol(test), drop=FALSE])
  #mean((pred - test[, ncol(test)])^2)
  (pred - test[, ncol(test)])^2
}
