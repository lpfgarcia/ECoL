
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

  l <- levels(data$class)
  tmp <- branch(data, l[j])
  if((j+1) > length(l)) return(0)

  aux <- sapply((j+1):length(l), function(k) {
    x <- branch(data, l[k])
    nrow(tmp) * nrow(x) * (colMeans(tmp) - colMeans(x))^2
  })

  aux <- rbind(aux,0)
  return(aux)
}

den <- function(data, j) {

  tmp <- branch(data, j)
  aux <- nrow(tmp) * diag(stats::var(tmp))
  return(aux)
}

f1 <- function(data) {

  aux <- do.call("cbind",
    sapply(1:nlevels(data$class), function(i) num(data, i))
  )

  tmp <- rbind(sapply(levels(data$class), function(i) den(data, i)), 1)
  aux <- max(rowSums(aux)/rowSums(tmp))
  return(aux)
}

dvector <- function(data) {

  m1 <- data[data$class == levels(data$class)[1], -ncol(data)]
  m2 <- data[data$class == levels(data$class)[2], -ncol(data)]

  c1 <- colMeans(m1)
  c2 <- colMeans(m2)

  W <- (nrow(m1)/nrow(data)) * stats::cov(m1) + 
    (nrow(m2)/nrow(data)) * stats::cov(m2)
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

