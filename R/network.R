
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

  adj <- enn(x, y, eps)
  graph <- igraph::graph.adjacency(adj, mode="undirected")

  sapply(measures, function(f) {
    eval(call(f, graph))
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

#' @export
ls.network <- function() {
  c("Density", "ClsCoef", "Hubs")
}

enn <- function(x, y, eps) {

  dst <- dist(x)
  eps <- eps*nrow(x)

  for(i in 1:nrow(x)) {
    a <- names(sort(dst[i,])[1:eps+1])
    b <- rownames(x[y == y[i],])
    dst[i,] <- 0; dst[i, intersect(a, b)] <- 1
  }

  return(dst)
}

Density <- function(graph) {
  igraph::graph.density(graph)
}

ClsCoef <- function(graph) {
  igraph::transitivity(graph, type="global", isolates="zero")
}

Hubs <- function(graph) {
  mean(igraph::hub.score(graph)$vector)
}
