#' Measures of class balance
#'
#' Classification task. These measures capture the differences in the number of 
#' examples per class in the dataset. When these differences are severe, 
#' problems related to generalization of the ML classification techniques could 
#' happen because of the imbalance ratio.
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
#'    \item{"C1"}{The entropy of class proportions (C1) capture the imbalance in
#'      a dataset based on the proportions of examples per class.}
#'    \item{"C2"}{The imbalance ratio (C2) is an index computed for measuring
#'      class balance. This is a version of the measure that is also suited for 
#'      multiclass classification problems.}
#'  }
#' @return A list named by the requested class balance measure.
#'
#' @references
#'  Ana C Lorena, Ivan G Costa, Newton Spolaor and Marcilio C P Souto. (2012). 
#'    Analysis of complexity indices for classification problems: Cancer gene 
#'    expression data. Neurocomputing 75, 1, 33--42.
#'
#'  Ajay K Tanwani and Muddassar Farooq. (2010). Classification potential vs. 
#'    classification accuracy: a comprehensive study of evolutionary algorithms 
#'    with biomedical datasets. Learning Classifier Systems 6471, 127--144.
#'
#' @examples
#' ## Extract all balance measures for classification task
#' data(iris)
#' balance(Species ~ ., iris)
#' @export
balance <- function(...) {
  UseMethod("balance")
}

#' @rdname balance
#' @export
balance.default <- function(x, y, measures="all", ...) {

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
    measures <- ls.balance()
  }

  measures <- match.arg(measures, ls.balance(), TRUE)

  sapply(measures, function(f) {
    eval(call(paste("c", f, sep="."), y=y))
  },  simplify=FALSE)
}

#' @rdname balance
#' @export
balance.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  balance.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.balance <- function() {
  c("C1", "C2")
}

c.C1 <- function(y) {
  c <- -1/log(nlevels(y))
  i <- table(y)/length(y)
  aux <- c*sum(i*log(i))
  return(aux)
}

c.C2 <- function(y) {
  ii <- summary(y)
  nc <- length(ii)
  aux <- ((nc - 1)/nc) * sum(ii/(length(y) - ii))
  aux <- 1 - (1/aux)
  return(aux)
}
