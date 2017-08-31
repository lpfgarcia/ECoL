
#' @export
network <- function(...) {
  UseMethod("network")
}

#' @rdname network
#' @export
network.default <- function(x, y, features="all", epsilon=0.15, ...) {
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

  if(features[1] == "all") {
    features <- ls.network()
  }

  features <- match.arg(features, ls.network(), TRUE)

  graph <- enn(x, y, epsilon)
  graph <- igraph::graph.adjacency(graph, mode="undirected")

  sapply(features, function(f) {
    eval(call(f, graph))
  })
}

#' @rdname network
#' @export
network.formula <- function(formula, data, features="all", epsilon=0.15, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  network.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' @expor
ls.network <- function() {
  c("edges", "avg_degree", "avg_density", "max_componet", 
    "avg_closeness", "avg_betweenness", "avg_hub", 
    "cluster_coefficient", "avg_path_length")
}

enn <- function(x, y, epsilon=0.15) {

  dst <- dist(x)
  e <- epsilon*nrow(x)

  for(i in 1:nrow(dst)) {
    z <- y == y[i]
    w <- rownames(x) %in%  names(sort(dst[i,])[1:e+1])
    dst[i,] <- 0; dst[i, w + z == 2] <- 1
  }

  return(dst)
}

edges <- function(graph) {
  igraph::ecount(graph)
}

avg_degree <- function(graph) {
  mean(igraph::degree(graph))
}

avg_density <- function(graph) {
  igraph::graph.density(graph)
}

max_componet <- function(graph) {
  max(igraph::clusters(graph)$csize)
}

avg_closeness <- function(graph) {
  mean(igraph::closeness(graph))
}

avg_betweenness <- function(graph) {
  mean(igraph::betweenness(graph, directed=FALSE))
}

avg_hub <- function(graph) {
  mean(igraph::hub.score(graph)$vector)
}

cluster_coefficient <- function(graph) {
  igraph::transitivity(graph, type="global")
}

avg_path_length <- function(graph) {

  cls <- igraph::clusters(graph)
  g <- igraph::induced.subgraph(graph, 
    which(cls$membership == which.max(cls$csize)))
  disthist <- igraph::path.length.hist(g, directed=FALSE)$res
  aux <- weighted.mean(1:length(disthist), disthist)  
  return(aux)
}

