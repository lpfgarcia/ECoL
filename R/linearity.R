#' Measures of linearity
#'
#' Classification task. The linearity measures try to quantify if it is possible
#' to separate the classes by a hyperplane. The underlying assumption is that a 
#' linearly separable problem can be considered simpler than a problem requiring
#' a non-linear decision boundary.
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
#'    \item{"L1"}{Sum of the error distance by linear programming (L1) computes 
#'      the sum of the distances of incorrectly classified examples to a linear 
#'      boundary used in their classification.}
#'    \item{"L2"}{Error rate of linear classifier (L2) computes the error rate 
#'      of the linear SVM classifier induced from dataset.}
#'    \item{"L3"}{Non-linearity of a linear classifier (L3) creates a new 
#'      dataset randomly interpolating pairs of training examples of the same 
#'      class and then induce a linear SVM on the original data and measure 
#'      the error rate in the new data points.}
#'  }
#' @return A list named by the requested linearity measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat 
#'    Ramon Llull.
#'
#' @examples
#' ## Extract all linearity measures
#' data(iris)
#' linearity.class(Species ~ ., iris)
#' @export
linearity.class <- function(...) {
  UseMethod("linearity.class")
}

#' @rdname linearity.class
#' @export
linearity.class.default <- function(x, y, measures="all", ...) {

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
    measures <- ls.linearity.class()
  }

  measures <- match.arg(measures, ls.linearity.class(), TRUE)
  colnames(x) <- make.names(colnames(x))

  data <- data.frame(x, class=y)
  data <- ovo(data)

  model <- lapply(data, smo)
  sapply(measures, function(f) {
    eval(call(paste("c", f, sep="."), model=model, data=data))
  })
}

#' @rdname linearity.class
#' @export
linearity.class.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity.class.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.linearity.class <- function() {
  c("L1", "L2", "L3")
}

smo <- function(data) {
  e1071::svm(class ~ ., data, scale=TRUE, kernel="linear")
}

c.L1 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d, decision.values=TRUE)
    err <- rownames(d[prd != d$class,])
    dst <- attr(prd, "decision.values")[err,]
    sum(abs(dst))/nrow(d)
  }, m=model, d=data)

  aux <- 1/(mean(aux) + 1)
  return(aux)
}

error <- function(pred, class) {
  1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}

c.L2 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d)
    error(prd, d$class)
  }, m=model, d=data)

  return(mean(aux))
}

c.L3 <- function(model, data) {

  aux <- mapply(function(m, d) {
    tmp <- c.generate(d, nrow(d))
    prd <- stats::predict(m, tmp)
    error(prd, tmp$class)
  }, m=model, d=data)

  return(mean(aux))
}

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
#' linearity.regr(speed~., cars)
#' @export
linearity.regr <- function(...) {
  UseMethod("linearity.regr")
}

#' @rdname linearity.regr
#' @export
linearity.regr.default <- function(x, y, measures="all", ...) {

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
    measures <- ls.linearity.regr()
  }

  measures <- match.arg(measures, ls.linearity.regr(), TRUE)
  colnames(x) <- make.names(colnames(x))

  x <- normalize(x)
  y <- normalize(y)[,1]

  x <- x[order(y), ,drop=FALSE]
  y <- y[order(y)]

  m <- stats::lm(y ~ ., cbind(y=y, x))

  sapply(measures, function(f) {
    eval(call(paste("r", f, sep="."), m=m, x=x, y=y))
  })
}

#' @rdname linearity.regr
#' @export
linearity.regr.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity.regr.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.linearity.regr <- function() {
  c("L1", "L2", "L3")
}

r.L1 <- function(m, ...) {
  mean(abs(m$residuals))
}

r.L2 <- function(m, ...) {
  mean(m$residuals^2)
}

r.L3 <- function(m, x, y) {
  test <- r.generate(x, y, nrow(x))
  pred <- stats::predict.lm(m, test[, -ncol(test), drop=FALSE])
  mean((pred - test[, ncol(test)])^2)
}
