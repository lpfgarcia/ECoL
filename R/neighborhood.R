
#' @export
neighborhood <- function(...) {
  UseMethod("neighborhood")
}

#' @rdname neighborhood
#' @export
neighborhood.default <- function(x, y, features="all", ...) {
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
    features <- ls.neighborhood()
  }

  features <- match.arg(features, ls.neighborhood(), TRUE)

  x <- binarize(x)
  d <- dist(x)

  sapply(features, function(f) {
    eval(call(f, x=x, y=y, d=d))
  })
}

#' @rdname neighborhood
#' @export
neighborhood.formula <- function(formula, data, features="all", ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  neighborhood.default(modFrame[, -1], modFrame[, 1], features, ...)
}

#' @export
ls.neighborhood <- function() {
  c("n1","n2", "n3", "n4", "t1", "t2", "t4", "LSCAvg")
}

n1 <- function(x, y, d) {

  g <- igraph::graph.adjacency(d, weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(igraph::as.undirected(g))))
  tmp <- which(tree != 0, arr.ind=TRUE)  
  aux <- which(y[tmp[,1]] != y[tmp[,2]])
  aux <- length(unique(tmp[aux,1]))
  return(aux/nrow(x))
}

intra <- function(x, y, d, i) {
  tmp <- rownames(x[y == y[i],])
  aux <- sort(d[i, setdiff(tmp, i)])[1]
  return(aux)
}

inter <- function(x, y, d, i) {
  tmp <- rownames(x[y != y[i],])
  aux <- sort(d[i, tmp])[1]
  return(aux)
}

n2 <- function(x, y, d) {

  aux <- sapply(1:nrow(x), function(i) {
    a <- intra(x, y, d, i)
    r <- inter(x, y, d, i)
    return(c(a,r))
  })

  aux <- sum(aux[1,])/sum(aux[2,])
  return(aux)
}

n3 <- function(x, y, d) {

  aux <- unlist(
    lapply(1:nrow(x), function(i) {
      knn(d, x, y, i, 1) != y[i]
    })
  )

  return(mean(aux))
}


n4 <- function(x, y, ...) {

  g <- generate(x, y)
  a <- rbind(x, g[[1]])
  b <- unlist(list(y, g[[2]]))
  v <- setdiff(rownames(a), rownames(x))
  d <- dist(a)

  aux <- unlist(
    lapply(v, function(i) {
      idx <- rownames(x) %in%  which.min(d[i, rownames(x)])
      y[idx] != b[rownames(a) %in% i]
    })
  )

  return(mean(aux))
}


radios <- function(dst, data, i) {

    di = inter(dst, data, i)
    j = names(di)
    dj = inter(dst, data, j)
    k = names(dj)

    if(i == k) {
        return(di/2)
    } else {
        tmp = radios(dst, data, j)
        return(dj - tmp)
    }
}


hyperspher <- function(dst, data) {

    r = rep(0, nrow(data))
    names(r) = rownames(data)

    for(i in names(r)){
        r[i] = radios(dst, data, i)
    }

    return(r)
}


translate <- function(dst, r) {

    aux = do.call("rbind",
        lapply(rownames(dst), 
            function(i) {
                dst[i,] < r[i]
        })
    )

    rownames(aux) = rownames(dst)
    return(aux)
}


adherence <- function(adh, data) {

    h = n = c()

    repeat{

        aux = which.max(rowSums(adh))
        tmp = names(which(adh[aux,]))
        dif = setdiff(rownames(adh), tmp)
        adh = adh[dif, dif]

        h = c(h, length(tmp))
        n = c(n, names(aux))

        if(is.null(dim(adh)) | sum(adh) == 0 |
            all(dim(adh) == 0))
                break
    }

    names(h) = n
    return(h)
}


t1 <- function(dst, data) {

    r = hyperspher(dst, data)
    aux = adherence(translate(dst, r), data)
    aux = length(aux)/nrow(data)
    return(aux)
}


t2 <- function(dst, data) {

    r = hyperspher(dst, data)
    aux = adherence(translate(dst, r), data)
    return(mean(aux))
}


ball <- function(r, n) {
    (1/sqrt(n*pi))*((2*pi*exp(1)/n)^(n/2))*r^n
}


t4 <- function(dst, data) {

    r = hyperspher(dst, data)
    aux = adherence(translate(dst, r), data)
    tmp = aux/ball(r[names(aux)], ncol(data)-1)
    return(sd(tmp))
}


LSCAvg <- function(dst, data) {

    r = sapply(rownames(data), function(i) {
        as.numeric(inter(dst, data, i))
    })

    aux = adherence(translate(dst, r), data)
    aux = sum(aux)/(length(aux)^2)
    return(aux)
}


ls.neighborhood <- function() {
    c("n1","n2", "n3", "n4", "t1", "t2", "t4", "LSCAvg")
}


neighborhood <- function(data) {

    dst = dist(data[,-ncol(data), drop=FALSE])

    aux = lapply(ls.neighborhood(), 
        function(i) {
            do.call(i, list(dst, data))
    })

    aux = unlist(aux)
    names(aux) = ls.neighborhood()
    return(aux)
}

