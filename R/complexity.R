#' Extract the complexity measures from datasets
#'
#' This function is responsable to extract the complexity measures from the 
#' datasets. To set specific parameters for each group, use the characterization
#' function.
#'
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param groups A list of complexity measures groups or \code{"all"} to include
#'  all of them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param ... Not used.
#'  The details section describes the valid values for this group.
#' @details
#'  The following groups are allowed for this method:
#'  \describe{
#'    \item{"overlapping"}{The feature overlapping measures characterize how 
#'      informative the available features are to separate the classes See 
#'      \link{overlapping} for more details.}
#'    \item{"neighborhood"}{Neighborhood measures characterize the presence and 
#'      density of same or different classes in local neighborhoods. See 
#'      \link{neighborhood} for more details.}
#'    \item{"linearity"}{Linearity measures try to quantify whether the classes 
#'      can be linearly separated. See \link{linearity} for more details.}
#'    \item{"dimensionality"}{The dimensionality measures compute information on
#'      how smoothly the examples are distributed within the classes. See 
#'      \link{dimensionality} for more details.}
#'    \item{"balance"}{Class balance measures take into account the numbers of 
#'      examples per class in the dataset. See \link{balance} for more details.}
#'    \item{"network"}{Network measures represent the dataset as a graph and 
#'      extract structural information from it. See \link{network} for more 
#'      details.}
#'  }
#' @return A numeric vector named by the requested complexity measures.
#' @export
#'
#' @examples
#' ## Extract all complexity measures
#' complexity(Species ~ ., iris)
#'
#' ## Extract the linearity group
#' complexity(Species ~ ., iris, groups="linearity")
complexity <- function(...) {
  UseMethod("complexity")
}

#' @rdname complexity
#' @export
complexity.default <- function(x, y, groups="all", ...) {
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

  if(groups[1] == "all") {
    groups <- ls.complexity()
  }

  groups <- match.arg(groups, ls.complexity(), TRUE)
  colnames(x) <- make.names(colnames(x))

  unlist(
    sapply(groups, function(group) {
      do.call(group, list(x=x, y=y, ...))
    }, simplify=FALSE)
  )
}

#' @rdname complexity
#' @export
complexity.formula <- function(formula, data, groups="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  complexity.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    groups, ...)
}

#' List the complexity measure groups
#'
#' @return A list of complexity measure groups
#' @export
#'
#' @examples
#' ls.complexity()
ls.complexity <- function() {
  c("overlapping", "neighborhood", "linearity", "dimensionality", "balance",
    "network")
}
