
#' @export
overlapping <- function(...) {
  UseMethod("overlapping")
}

#' @rdname overlapping
#' @export
overlapping.default <- function(x, y, features="all", ...) {
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
    features <- ls.overlapping()
  }

  features <- match.arg(features, ls.overlapping(), TRUE)

  x <- binarize(x)

  sapply(features, function(f) {
    eval(call(f, x=x, y=y))
  })
}

#' @rdname overlapping
#' @export
overlapping.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  overlapping.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' @expor
ls.overlapping <- function() {
  c("f1", "f1v", "f2", "f3", "f4")
}

num <- function(x, y, j) {
  tmp <- x[y == j, , drop=FALSE]
  aux <- nrow(tmp) * (colMeans(tmp) - colMeans(x))^2
  return(aux)
}

den <- function(x, y, j) {
  tmp <- x[y == j, , drop=FALSE]
  aux <- rowSums((t(tmp) - colMeans(tmp))^2)
  return(aux)
}

f1 <- function(x, y) {

  aux <- do.call("cbind", 
    lapply(levels(y), function(i) {
      num(x, y, i)/den(x, y, i)
    })
  )

  aux[aux == Inf] <- NA
  aux <- rowSums(aux, na.rm=TRUE)
  return(max(aux))
}

f1v <- function(x, y) {
  aux <- predict(MASS::lda(x, y), x)
  f1(aux$x, y)
}

regionOver <- function(x, y) {

  a <- x[y == levels(y)[1],]
  b <- x[y == levels(y)[2],]

  maxmax <- rbind(colMax(a), colMax(b))
  minmin <- rbind(colMin(a), colMin(b))

  over <- colMax(rbind(colMin(maxmax) - colMax(minmin), 0))
  rang <- colMax(maxmax) - colMin(minmin)

  aux <- prod(over/rang, na.rm=TRUE)
  return(aux)
}

f2 <- function(x, y) {

  data <- one.vs.one(x, y)
  aux <- mapply(function(d) {
    regionOver(d[[1]], d[[2]])
  }, d=data)

  return(mean(aux))
}

nonOverlap <- function(x, y) {

  a <- x[y == levels(y)[1],]
  b <- x[y == levels(y)[2],]

  minmax <- colMin(rbind(colMax(a), colMax(b)))
  maxmin <- colMax(rbind(colMin(a), colMin(b)))

  aux <- do.call("cbind",
      lapply(1:ncol(x), function(i) {
          x[,i] < maxmin[i] | 
              x[,i] > minmax[i]
      })
  )

  aux <- data.frame(aux)
  rownames(aux) <- rownames(x)
  return(aux)
}

f3 <- function(x, y) {

  data <- one.vs.one(x, y)
  aux <- mapply(function(d) {
    colSums(nonOverlap(d[[1]], d[[2]]))/nrow(d[[1]])
  }, d=data)

  aux <- data.frame(aux)
  aux <- mean(colMax(aux))
  return(aux)
}

removing <- function(x, y) {

  repeat {
    tmp <- nonOverlap(x, y)
    col <- which.max(colSums(tmp))
    x <- x[tmp[,col] != TRUE, -col, drop=FALSE]
    y <- y[tmp[,col] != TRUE]
    if(nrow(x) == 0 | ncol(x) == 1 |
      length(unique(y)) == 1)
        break
  }

  return(x)
}

f4 <- function(x, y) {

  data <- one.vs.one(x, y)
  aux <- mapply(function(d) {
    n <- removing(d[[1]], d[[2]])
    (nrow(d[[1]]) - nrow(n))/nrow(d[[1]])
  }, d=data)

  aux <- mean(aux)
  return(aux)
}

