#' Measures of smoothness
#'
#' Regression task. In regression problems, the smoother the function to be 
#' fitted to the data, the simpler it shall be. Larger variations in the inputs
#' and/or outputs, on the other hand, usually indicate the existence of more 
#' intricate relationships between them.
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
#'    \item{"S1"}{Output distribution (S1) monitors whether the examples 
#'      joined in the MST have similar output values. Lower values indicate 
#'      simpler problems, where the outputs of similar examples in the input 
#'      space are also next to each other.}
#'    \item{"S2"}{Input distribution (S2) measure how similar in the input space
#'      are data items with similar outputs based on distance.}
#'    \item{"S3"}{Error of a nearest neighbor regressor (S3) calculates the mean
#'      squared error of a 1-nearest neighbor regressor  using leave-one-out.}
#'    \item{"S4"}{Non-linearity of nearest neighbor regressor (S4) calculates 
#'      the mean squared error of a 1-nearest neighbor regressor to the new 
#'      randomly interpolated points.}
#'  }
#' @return A list named by the requested smoothness measure.
#'
#' @references
#'  Ana C Lorena and Aron I Maciel and Pericles B C Miranda and Ivan G Costa and
#'    Ricardo B C Prudencio. (2018). Data complexity meta-features for 
#'    regression problems. Machine Learning, 107, 1, 209--246.
#'
#' @examples
#' ## Extract all smoothness measures
#' data(cars)
#' smoothness(speed~., cars)
#' @export
smoothness <- function(...) {
  UseMethod("smoothness")
}

#' @rdname smoothness
#' @export
smoothness.default <- function(x, y, measures="all", summary=c("mean", "sd"), 
                               ...) {

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
    measures <- ls.smoothness()
  }

  measures <- match.arg(measures, ls.smoothness(), TRUE)

  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  x <- normalize(x)
  y <- normalize(y)[,1]

  x <- x[order(y), , drop=FALSE]
  y <- y[order(y)]
  d <- dist(x)

  sapply(measures, function(f) {
    measure = eval(call(paste("r", f, sep="."), d=d, x=x, y=y))
    post.processing(measure, summary, ...)
  }, simplify=FALSE)
}

#' @rdname smoothness
#' @export
smoothness.formula <- function(formula, data, measures="all", 
                               summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  smoothness.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

ls.smoothness <- function() {
  c("S1", "S2", "S3", "S4")
}

r.S1 <- function(d, x, y) {

  g <- igraph::graph.adjacency(d, mode="undirected", weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(g)))
  tmp <- which(tree != 0, arr.ind=TRUE)
  #mean(abs(y[tmp[,1]] - y[tmp[,2]]))
  abs(y[tmp[,1]] - y[tmp[,2]])
}

r.S2 <- function(d, x, y) {

  pred <- sapply(2:nrow(d), function(i) {
    d[i-1, i]
  })

  #mean(pred)
  pred
}

r.S3 <- function(d, x, y) {

  diag(d) <- Inf
  pred <- apply(d, 1, function(i) {
    y[which.min(i)]
  })

  #mean((pred - y)^2)
  (pred - y)^2
}

r.S4 <- function(d, x, y) {
  test <- r.generate(x, y, nrow(x))
  pred <- FNN::knn.reg(x, test[, -ncol(test), drop=FALSE], y, k=1)$pred
  #mean((pred - test[, ncol(test)])^2)
  (pred - test[, ncol(test)])^2
}
