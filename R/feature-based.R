#' Measures of feature-based
#'
#' Classification task. The feature-based measures evaluate how informative the 
#' available features are to separate the classes. If there is at least one very
#' discriminative feature in the dataset, the problem can be considered simpler 
#' than if there is no such an attribute. 
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{summarization} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"F1"}{Maximum Fisher's Discriminant Ratio (F1) measures the overlap 
#'      between the values of the features and takes the value of the largest 
#'      discriminant ratio among all the available features.}
#'    \item{"F1v"}{Directional-vector maximum Fisher's discriminant ratio (F1v)
#'      complements F1 by searching for a vector able to separate two classes 
#'      after the training examples have been projected into it.}
#'    \item{"F2"}{Volume of the overlapping region (F2) computes the overlap of 
#'      the distributions of the features values within the classes. F2 can be 
#'      determined by finding, for each feature its minimum and maximum values 
#'      in the classes.}
#'    \item{"F3"}{The maximum individual feature efficiency (F3) of each 
#'      feature is given by the ratio between the number of examples that are 
#'      not in the overlapping region of two classes and the total number of 
#'      examples. This measure returns the maximum of the values found among 
#'      the input features.}
#'    \item{"F4"}{Collective feature efficiency (F4) get an overview on how 
#'      various features may work together in data separation. First the most 
#'      discriminative feature according to F3 is selected and all examples that
#'      can be separated by this feature are removed from the dataset. The 
#'      previous step is repeated on the remaining dataset until all the 
#'      features have been considered or no example remains. F4 returns the 
#'      ratio of examples that have been discriminated.}
#'  }
#' @return A list named by the requested feature-based measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat
#'    Ramon Llull.
#'
#' @examples
#' ## Extract all feature-based measures for classification task
#' data(iris)
#' featurebased(Species ~ ., iris)
#' @export
featurebased <- function(...) {
  UseMethod("featurebased")
}

#' @rdname featurebased
#' @export
featurebased.default <- function(x, y, measures="all", summary=c("mean", "sd"), 
                                ...) {

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
    measures <- ls.featurebased()
  }

  measures <- match.arg(measures, ls.featurebased(), TRUE)

  if (length(summary) == 0) {
    summary <- "return"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  x <- binarize(x)
  data <- data.frame(x, class=y)

  sapply(measures, function(f) {
    measure = eval(call(paste("c", f, sep="."), data=data))
    summarization(measure, summary, f %in% ls.featurebased.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname featurebased
#' @export
featurebased.formula <- function(formula, data, measures="all", 
                                summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  featurebased.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

ls.featurebased <- function() {
  c("F1", "F1v", "F2", "F3", "F4")
}

ls.featurebased.multiples <- function() {
  ls.featurebased()
}

branch <- function(data, j) {
  data[data$class == j, -ncol(data), drop=FALSE]
}

numerator <- function(j, data) {

  tmp <- branch(data, j)
  aux <- nrow(tmp) * (colMeans(tmp) - 
    colMeans(data[,-ncol(data), drop=FALSE]))^2
  return(aux)
}

denominator <- function(j, data) {

  tmp <- branch(data, j)
  aux <- rowSums((t(tmp) - colMeans(tmp))^2)
  return(aux)
}

c.F1 <- function(data) {

  num <- lapply(levels(data$class), numerator, data)
  den <- lapply(levels(data$class), denominator, data)

  aux <- rowSums(do.call("cbind", num)) / 
    rowSums(do.call("cbind", den))

  #aux <- max(aux, na.rm=TRUE)
  aux <- 1/(aux + 1)
  return(aux)
}

dvector <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  c1 <- colMeans(a)
  c2 <- colMeans(b)

  W <- (nrow(a)/nrow(data)) * stats::cov(a) + 
    (nrow(b)/nrow(data)) * stats::cov(b)

  B <- (c1 - c2) %*% t(c1 - c2)
  d <- MASS::ginv(W) %*% (c1 - c2)

  aux <- (t(d) %*% B %*% d)/(t(d) %*% W %*% d)
  return(aux)
}

c.F1v <- function(data) {
  data <- ovo(data)
  #aux <- mean(sapply(data, dvector))
  aux <- sapply(data, dvector)
  aux <- 1/(aux + 1)
  return(aux)
}

regionOver <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  maxmax <- rbind(colMax(a), colMax(b))
  minmin <- rbind(colMin(a), colMin(b))

  over <- colMax(rbind(colMin(maxmax) - colMax(minmin), 0))
  rang <- colMax(maxmax) - colMin(minmin)
  aux <- prod(over/rang, na.rm=TRUE)
  return(aux)
}

c.F2 <- function(data) {

  data <- ovo(data)
  #aux <- mean(sapply(data, regionOver))
  aux <- sapply(data, regionOver)
  return(aux)
}

nonOverlap <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  minmax <- colMin(rbind(colMax(a), colMax(b)))
  maxmin <- colMax(rbind(colMin(a), colMin(b)))

  aux <- do.call("cbind",
    lapply(1:(ncol(data)-1), function(i) {
        data[,i] < maxmin[i] | data[,i] > minmax[i]
    })
  )

  aux <- data.frame(aux)
  rownames(aux) <- rownames(data)
  return(aux)
}

c.F3 <- function(data) {

  data <- ovo(data)
  aux <- mapply(function(d) {
    colSums(nonOverlap(d))/nrow(d)
  }, d=data)

  #aux <- 1 - mean(colMax(aux))
  aux <- 1 - colMax(aux)
  return(aux)
}

removing <- function(data) {

  repeat {

    tmp <- nonOverlap(data)
    col <- which.max(colSums(tmp))
    aux <- rownames(tmp[tmp[,col] != TRUE, , drop=FALSE])
    data <- data[aux,- col, drop=FALSE]

    if(nrow(data) == 0 | ncol(data) == 1 |
      length(unique(data$class)) == 1)
        break
  }

  return(data)
}

c.F4 <- function(data) {

  data <- ovo(data)
  aux <- mapply(function(d) {
    nrow(removing(d))/nrow(d)
  }, d=data)

  #aux <- mean(aux)
  return(aux)
}
