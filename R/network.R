
#' @export
network <- function(...) {
  UseMethod("network")
}

#' @rdname network
#' @export
network.default <- function(x, y, measures="all", epsilon=0.15, ...) {
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

  data <- data.frame(x, class=y)

  graph <- enn(data, epsilon)
  graph <- igraph::graph.adjacency(graph, mode="undirected")

  sapply(measures, function(f) {
    eval(call(f, graph))
  })
}

#' @rdname network
#' @export
network.formula <- function(formula, data, measures="all", epsilon=0.15, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  network.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE], 
    measures, ...)
}

#' @export
ls.network <- function() {
  c("Density", "Degree", "Closeness", "ClsCoef", "Hubs")
}

enn <- function(data, epsilon=0.15) {

  dst <- dist(data[,-ncol(data), drop=FALSE])
  e <- epsilon*nrow(data)

  for(i in 1:nrow(dst)) {

    x <- names(sort(dst[i,])[1:e+1])
    y <- rownames(data[data$class == data[i,]$class,])
    dst[i,] <- 0; dst[i, intersect(x, y)] <- 1
  }

  return(dst)
}

Density <- function(graph) {
  igraph::graph.density(graph)
}

Degree <- function(graph) {
  n <- igraph::vcount(graph)
  aux <- sum(igraph::degree(graph))/(n*(n-1))
  return(aux)
}

Closeness <- function(graph) {
  mean(igraph::closeness(graph))
}

ClsCoef <- function(graph) {
  igraph::transitivity(graph, type="global", isolates="zero")
}

Hubs <- function(graph) {
  mean(igraph::hub.score(graph)$vector)
}

