#' Measures of Linearity
#'
#' The linearity measures try to quantify if it is possible to separate the 
#' classes by a hyperplane. The underlying assumption is that a linearly 
#' separable problem can be considered simpler than a problem requiring a 
#' non-linear decision boundary.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param ... Not used.
#' The details section describes the valid values for this group.
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
#' @return A list named by the requested class linearity measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia, and Tin K Ho. (2010). Documentation for 
#'    the data complexity library in C++. Technical Report. La Salle - 
#'    Universitat Ramon Llull.
#'
#' @examples
#' ## Extract all linearity measures
#' linearity(Species ~ ., iris)
#' @export
linearity <- function(...) {
  UseMethod("linearity")
}

#' @rdname linearity
#' @export
linearity.default <- function(x, y, measures="all", ...) {
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
    measures <- ls.linearity()
  }

  measures <- match.arg(measures, ls.linearity(), TRUE)
  colnames(x) <- make.names(colnames(x))

  x <- binarize(x)
  data <- data.frame(x, class=y)
  data <- ovo(data)

  model <- lapply(data, smo)
  sapply(measures, function(f) {
    eval(call(f, model=model, data=data))
  })
}

#' @rdname linearity
#' @export
linearity.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.linearity <- function() {
  c("L1", "L2", "L3")
}

smo <- function(data) {
  e1071::svm(class ~ ., data, scale=TRUE, kernel="linear")
}

hyperretangle <- function(data) {
  data <- data[,-ncol(data), drop=FALSE]
  aux <- sqrt(sum((colMax(data)-colMin(data))^2))
  return(aux)
}

L1 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d, decision.values=TRUE)
    err <- rownames(d[prd != d$class,])
    dst <- attr(prd, "decision.values")[err,]
    sum(abs(dst))/nrow(d)
  }, m=model, d=data)

  aux <- mean(aux)
  return(aux)
}

error <- function(pred, class) {
  1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}

L2 <- function(model, data) {

  aux <- mapply(function(m, d) {
    pred <- stats::predict(m, d)
    error(pred, d$class)
  }, m=model, d=data)

  return(mean(aux))
}

L3 <- function(model, data) {

  aux <- mapply(function(m, d) {
    tmp <- generate(d, nrow(d))
    pred <- stats::predict(m, tmp)
    error(pred, tmp$class)
  }, m=model, d=data)

  return(mean(aux))
}
