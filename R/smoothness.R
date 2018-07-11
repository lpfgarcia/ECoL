#' @export
smoothness <- function(...) {
  UseMethod("smoothness")
}

#' @rdname smoothness
#' @export
smoothness.default <- function(x, y, measures="all", ...) {

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
  colnames(x) <- make.names(colnames(x))

  x <- normalize(x)
  y <- normalize(y)[,1]

  x <- x[order(y),]
  y <- y[order(y)]
  d <- dist(x)

  sapply(measures, function(f) {
    eval(call(paste("r", f, sep="."), d=d, x=x, y=y))
  })
}

#' @rdname smoothness
#' @export
smoothness.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  smoothness.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.smoothness <- function() {
  c("S1", "S2", "S3", "S4")
}

r.S1 <- function(d, x, y) {

  g <- igraph::graph.adjacency(d, mode="undirected", weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(g)))
  tmp <- which(tree != 0, arr.ind=TRUE)
  mean(abs(y[tmp[,1]] - y[tmp[,2]]))
}

r.S2 <- function(d, x, y) {

  pred <- sapply(2:nrow(d), function(i) {
    d[i-1, i]
  })

  mean(pred)
}

r.S3 <- function(d, x, y) {

  diag(d) <- Inf
  pred <- apply(d, 1, function(i) {
    y[minPosition(i)]
  })

  mean((pred - y)^2)
}

r.S4 <- function(d, x, y) {

  tran <- r.generate(x, y, nrow(x))
  pred <- FNN::knn.reg(x, tran[,-ncol(tran)], y, k=1)$pred
  mean((pred - tran[,ncol(tran)])^2)
}
