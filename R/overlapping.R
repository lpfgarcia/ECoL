
#' @export
overlapping <- function(...) {
  UseMethod("overlapping")
}

#' @rdname overlapping
#' @export
overlapping.default <- function(x, y, measures="all", ...) {
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
    measures <- ls.overlapping()
  }

  measures <- match.arg(measures, ls.overlapping(), TRUE)

  data <- data.frame(x, class=y)
  data <- binarize(data)

  sapply(measures, function(f) {
    eval(call(f, data=data))
  })
}

#' @rdname overlapping
#' @export
overlapping.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  overlapping.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE], 
    measures, ...)
}

#' @export
ls.overlapping <- function() {
  c("f1", "f1v", "f2", "f3", "f4")
}

branch <- function(data, j) {
  data[data$class == j, -ncol(data), drop=FALSE]
}

num <- function(data, j) {

  tmp <- branch(data, j)
  aux <- nrow(tmp) * (colMeans(tmp) - 
    colMeans(data[,-ncol(data), drop=FALSE]))^2
  return(aux)
}

den <- function(data, j) {

  tmp <- branch(data, j)
  aux <- rowSums((t(tmp) - colMeans(tmp))^2)
  return(aux)
}

f1 <- function(data) {

  aux <- do.call("cbind", 
    lapply(levels(data$class), function(i) {
      num(data, i)/den(data, i)
    })
  )

  aux <- max(rowSums(aux))
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

f1v <- function(data) {
  data <- ovo(data)
  aux <- unlist(lapply(data, dvector))
  return(mean(aux))
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

f2 <- function(data) {

  data <- ovo(data)
  aux <- unlist(lapply(data, regionOver))
  return(mean(aux))
}

nonOverlap <- function(data) {

  l <- levels(data$class)
  a <- branch(data, l[1])
  b <- branch(data, l[2])

  minmax <- colMin(rbind(colMax(a), colMax(b)))
  maxmin <- colMax(rbind(colMin(a), colMin(b)))

  aux <- do.call("cbind",
    lapply(1:(ncol(data)-1), 
      function(i) {
        data[,i, drop=FALSE] < maxmin[i] | 
          data[,i, drop=FALSE] > minmax[i]
    })
  )

  aux <- data.frame(aux)
  rownames(aux) <- rownames(data)
  return(aux)
}

f3 <- function(data) {

  data <- ovo(data)
  aux <- mapply(function(d) {
    colSums(nonOverlap(d))/nrow(d)
  }, d=data)

  aux <- data.frame(aux)
  aux <- mean(colMax(aux))
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

f4 <- function(data) {

  data <- ovo(data)
  aux <- mapply(function(d) {
    n <- removing(d)
    (nrow(d) - nrow(n))/nrow(d)
  }, d=data)

  aux <- mean(aux)
  return(aux)
}

