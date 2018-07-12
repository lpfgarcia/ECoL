#' Extract the complexity measures from datasets
#'
#' This function is responsable to extract the complexity measures from the 
#' datasets. For such, they take into account the overlap between classes 
#' imposed by feature values, the separability and distribution of the data 
#' points and the value of structural measures based on the representation of 
#' the dataset as a graph structure. To set specific parameters for each group, 
#' use the characterization function.
#'
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param groups A list of complexity measures groups or \code{"all"} to include
#'  all of them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param type The type of supervised problem: The \code{"class"} is used for 
#'  classification problems and \code{"regr"} for regression problems.
#' @param ... Not used.
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
#'      can be linearly separated. See \link{linearity.class} or 
#'      \link{linearity.regr} for more details.}
#'    \item{"dimensionality"}{The dimensionality measures compute information on
#'      how smoothly the examples are distributed within the classes. See 
#'      \link{dimensionality} for more details.}
#'    \item{"balance"}{Class balance measures take into account the numbers of 
#'      examples per class in the dataset. See \link{balance} for more details.}
#'    \item{"network"}{Network measures represent the dataset as a graph and 
#'      extract structural information from it. See \link{network} for more 
#'      details.}
#'    \item{"correlation"}{\link{correlation}}
#'    \item{"Smoothness"}{\link{smoothness}}
#'  }
#' @return A numeric vector named by the requested complexity measures.
#'
#' @references
#'  Tin K Ho and Mitra Basu. (2002). Complexity measures of supervised 
#'    classification problems. IEEE Transactions on Pattern Analysis and Machine
#'    Intelligence, 24, 3, 289--300.
#'
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for 
#'    the data complexity library in C++. Technical Report. La Salle - 
#'    Universitat Ramon Llull.
#'
#'  Ana C Lorena and Aron I Maciel and Pericles B C Miranda and Ivan G Costa and
#'    Ricardo B C Prudencio. (2018). Data complexity meta-features for 
#'    regression problems. Machine Learning, 107, 1, 209--246.
#'
#' @examples
#' ## Extract all complexity measures for classification problems
#' data(iris)
#' complexity(Species ~ ., iris, type="class")
#'
#' ## Extract all complexity measures for regression problems
#' data(cars)
#' complexity(speed~., cars, type="regr")
#' @export
complexity <- function(...) {
  UseMethod("complexity")
}

#' @rdname complexity
#' @export
complexity.default <- function(x, y, type, groups="all", ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  type.int <- pmatch(type, c("class", "regr"))
  if(is.na(type.int))
    stop("Invalid Type")

  if(type == "class") {
    if(min(table(y)) < 2) {
      stop("number of examples in the minority class should be >= 2")
    }
  } 

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(groups[1] == "all") {
    groups <- ls.complexity(type)
  }

  groups <- match.arg(groups, ls.complexity(type), TRUE)
  colnames(x) <- make.names(colnames(x))

  unlist(
    sapply(groups, function(group) {
      do.call(group, list(x=x, y=y, ...))
    }, simplify=FALSE)
  )
}

#' @rdname complexity
#' @export
complexity.formula <- function(formula, data, type, groups="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  complexity.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    type, groups, ...)
}

ls.complexity <- function(type) {

  switch(type,
    class = {
      c("overlapping", "neighborhood", "linearity.class", "dimensionality",
        "balance", "network")
    }, regr = {
      c("correlation", "linearity.regr", "smoothness", "dimensionality")
    }
  )
}
