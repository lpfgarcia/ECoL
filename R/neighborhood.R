
#' @export
neighborhood <- function(...) {
  UseMethod("neighborhood")
}

#' @rdname neighborhood
#' @export
neighborhood.default <- function(x, y, measures="all", ...) {
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
    measures <- ls.neighborhood()
  }

  measures <- match.arg(measures, ls.neighborhood(), TRUE)
  colnames(x) <- make.names(colnames(x))

  x <- binarize(x)
  data <- data.frame(x, class=y)
  dst <- dist(x)

  sapply(measures, function(f) {
    eval(call(f, dst=dst, data=data))
  })
}

#' @rdname neighborhood
#' @export
neighborhood.formula <- function(formula, data, measures="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  neighborhood.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

#' @export
ls.neighborhood <- function() {
  c("N1","N2", "N3", "N4", "T1", "LSCAvg")
}

knn <- function(dst, data, k=3, i) {
  tmp <- names(sort(dst[i,])[1:k+1])
  aux <- data[tmp,]$class
  names(aux) <- tmp
  return(aux) 
}

N1 <- function(dst, data) {

  g <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(g)))

  tmp <- which(tree != 0, arr.ind=TRUE)
  aux <- which(data[tmp[,1],]$class != data[tmp[,2],]$class)
  aux <- length(unique(tmp[aux,1]))
  return(aux/nrow(data))
}

intra <- function(dst, data, i) {
  tmp <- rownames(data[data$class == data[i,]$class,])
  aux <- sort(dst[i, setdiff(tmp, i)])[1]
  return(aux)
}

inter <- function(dst, data, i) {
  tmp <- rownames(data[data$class != data[i,]$class,])
  aux <- sort(dst[i, tmp])[1]
  return(aux)
}

N2 <- function(dst, data) {

  aux <- sapply(rownames(data), 
    function(i) {
      a <- intra(dst, data, i)
      r <- inter(dst, data, i)
      return(c(a,r))
  })

  aux = sum(aux[1,])/sum(aux[2,])
  return(aux)
}

N3 <- function(dst, data) {

  aux <- unlist(
    lapply(rownames(data), 
      function(i) {
        knn(dst, data, 1, i) != data[i,]$class
    })
  )

  return(mean(aux))
}

N4 <- function(dst, data) {

  aux <- rbind(data, generate(data, nrow(data)))
  vet <- setdiff(rownames(aux), rownames(data))
  dst <- dist(aux[,-ncol(aux), drop=FALSE])

  aux <- unlist(
    lapply(vet, function(i) {
      idx <- which.min(dst[i, rownames(data)])
      data[names(idx),]$class != aux[i,]$class
    })
  )

  return(mean(aux))
}

radios <- function(dst, data, i) {

  di <- inter(dst, data, i)
  j <- names(di)
  dj <- inter(dst, data, j)
  k <- names(dj)

  if(i == k) {
    return(di/2)
  } else {
    tmp <- radios(dst, data, j)
    return(di - tmp)
  }
}

hyperspher <- function(dst, data) {

  r <- rep(0, nrow(data))
  names(r) <- rownames(data)

  for(i in names(r)){
    r[i] <- radios(dst, data, i)
  }

  return(r)
}

translate <- function(dst, r) {

  aux <- do.call("rbind",
    lapply(rownames(dst), 
      function(i) {
        dst[i,] < r[i]
    })
  )

  rownames(aux) <- rownames(dst)
  return(aux)
}

adherence <- function(adh, data) {

  h <- n <- c()

  repeat{

    aux <- which.max(rowSums(adh))
    tmp <- names(which(adh[aux,]))
    dif <- setdiff(rownames(adh), c(tmp, names(aux)))
    adh <- adh[dif, dif, drop=FALSE]

    if(all(dim(adh) != 0)) {
      h <- c(h, length(tmp))
    } else {
      h <- c(h, 1)
    }

    n <- c(n, names(aux))

    if(all(dim(adh)) == 0)
      break
  }

  names(h) <- n
  return(h)
}

T1 <- function(dst, data) {
  r <- hyperspher(dst, data)
  aux <- adherence(translate(dst, r), data)
  aux <- length(aux)/nrow(data)
  return(aux)
}

LSCAvg <- function(dst, data) {
  r <- hyperspher(dst, data)
  aux <- sum(translate(dst, r))/(nrow(dst)^2)
  return(aux)
}
