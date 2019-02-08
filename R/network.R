#' Measures of network
#'
#' Classification task. The network measures represent the dataset as a graph 
#' and extract structural information from it. The transformation between raw 
#' data and the graph representation is based on the epsilon-NN algorithm. Next,
#' a post-processing step is applied to the graph, pruning edges between 
#' examples of opposite classes.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param eps The percentage of nodes in the graph to be connected.
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"Density"}{Average Density of the network (Density) represents the 
#'      number of edges in the graph, divided by the maximum number of edges 
#'      between pairs of data points.}
#'    \item{"ClsCoef"}{Clustering coefficient (ClsCoef) averages the clustering 
#'      tendency of the vertexes by the ratio of existent edges between its 
#'      neighbors and the total number of edges that could possibly exist 
#'      between them.}
#'    \item{"Hubs"}{Hubs score (Hubs) is given by the number of connections it  
#'      has to other nodes, weighted by the number of connections these 
#'      neighbors have.}
#'  }
#' @return A list named by the requested network measure.
#'
#' @references
#'  Gleison Morais and Ronaldo C Prati. (2013). Complex Network Measures for 
#'    Data Set Characterization. In 2nd Brazilian Conference on Intelligent 
#'    Systems (BRACIS). 12--18.
#'
#'  Luis P F Garcia, Andre C P L F de Carvalho and Ana C Lorena. (2015). Effect
#'    of label noise in the complexity of classification problems. 
#'    Neurocomputing 160, 108--119.
#'
#' @examples
#' ## Extract all network measures
#' data(iris)
#' network(Species ~ ., iris)
#' @export
network <- function(...) {
  UseMethod("network")
}

#' @rdname network
#' @export
network.default <- function(x, y, measures="all", eps=0.15, ...) {

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
    measures <- ls.network()
  }

  measures <- match.arg(measures, ls.network(), TRUE)
  colnames(x) <- make.names(colnames(x))

  dst <- enn(x, y, eps*nrow(x))
  graph <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)

  sapply(measures, function(f) {
    eval(call(paste("c", f, sep="."), graph))
  })
}

#' @rdname network
#' @export
network.formula <- function(formula, data, measures="all", eps=0.15, ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  network.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, eps, ...)
}

ls.network <- function() {
  c("Density", "ClsCoef", "Hubs")
}

enn <- function(x, y, e) {

  dst <- dist(x)

  for(i in 1:nrow(x)) {
    a <- names(sort(dst[i,])[1:e+1])
    b <- rownames(x[y == y[i],])
    dst[i, setdiff(rownames(x), intersect(a, b))] <- 0
  }

  return(dst)
}

c.Density <- function(graph) {
  1 - igraph::graph.density(graph)
}

c.ClsCoef <- function(graph) {
  1 - igraph::transitivity(graph, type="global", isolates="zero")
}

c.Hubs <- function(graph) {
  1 - mean(igraph::hub.score(graph)$vector)
}
