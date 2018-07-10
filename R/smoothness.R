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

  x <- normalize(binarize(x))
  y <- normalize(y)

  sapply(measures, function(f) {
    eval(call(paste("r", f, sep="."), x=x, y=y))
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

r.S1 <- function(x, y) {

  g <- igraph::graph.adjacency(dist(x), mode="undirected", weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(g)))
  tmp <- which(tree != 0, arr.ind=TRUE)
  mean(abs(y[tmp[,1]] - y[tmp[,2]]))
}

r.S2 <- function(x, y) {

  x <- x[order(y),]
  dst <-  as.matrix(dist(x))
  pred <- sapply(2:nrow(dst), function(i) {
    dst[i-1, i]
  })

  mean(pred)
}

r.S3 <- function(x, y) {

  dst <- dist(x)
  diag(dst) <- Inf
  pred <- apply(dst, 1, function(i) {
    y[minPosition(i)]
  })

  mean((pred - y)^2)
}

r.S4 <- function(x, y) {

  order <- order(y)
  y <- y[order]
  x <- x[order,]

  randomUniform <- runif(nrow(x) - 1)

  newInput <- randomUniform*x[2:nrow(x)-1,] + (1-randomUniform)*x[2:nrow(x),]
  newOutput <- randomUniform*y[2:nrow(x)-1] + (1-randomUniform)*y[2:nrow(x)]
  newPredict <- FNN::knn.reg(x, as.data.frame(newInput), y, k=1)$pred
  mean((newPredict - newOutput)^2)
}
