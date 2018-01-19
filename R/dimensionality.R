#' Measures of dimensionality
#'
#' These measures give an indicative of data sparsity. They capture how sparse a
#' datasets tend to have regions of low density. These regions are know to be 
#' more difficult to extract good classification models.
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
#'    \item{"T2"}{Average number of points per dimension (T2) is given by the 
#'      ratio between the number of examples and dimensionality of the dataset.} 
#'    \item{"T3"}{Average number of points per PCA (T3) is similar to T2, but 
#'      uses the number of PCA components needed to represent 95% of data 
#'      variability as the base of data sparsity assessment.}
#'    \item{"T4"}{Ratio of the PCA Dimension to the Original (T4) it estimates 
#'      the proportion of relevant and the original dimensions for a dataset.}
#'  }
#' @return A list named by the requested class dimensionality measure.
#'
#' @references
#'  Ana C Lorena, Ivan G Costa, Newton Spolaor, and Marcilio C P Souto. (2012). 
#'    Analysis of complexity indices for classification problems: Cancer gene 
#'    expression data. Neurocomputing 75, 1, 33--42.
#'
#' @examples
#' ## Extract all dimensionality measures
#' dimensionality(Species ~ ., iris)
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

ls.dimensionality <- function() {
  c("T2", "T3", "T4")
}

pca <- function(x) {
  aux <- stats::prcomp(x)
  tmp <- length(which(summary(aux)$importance[3,] < 0.95)) + 1
  return(tmp)
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
