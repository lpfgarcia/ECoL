#' Measures of neighborhood
#'
#' Classification task. The Neighborhood measures analyze the neighborhoods of 
#' the data items and try to capture class overlapping and the shape of the 
#' decision boundary. They work over a distance matrix storing the distances 
#' between all pairs of data points in the dataset.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param summary A list of summarization functions or empty for all values. See
#'  \link{post.processing} method to more information. (Default: 
#'  \code{c("mean", "sd")})
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"N1"}{Fraction of borderline points (N1) computes the percentage of 
#'      vertexes incident to edges connecting examples of opposite classes in 
#'      a Minimum Spanning Tree (MST).}
#'    \item{"N2"}{Ratio of intra/extra class nearest neighbor distance (N2)  
#'      computes the ratio of two sums: intra-class and inter-class. The former 
#'      corresponds to the sum of the distances between each example and its 
#'      closest neighbor from the same class. The later is the sum of the 
#'      distances between each example and its closest neighbor from another 
#'      class (nearest enemy).}
#'    \item{"N3"}{Error rate of the nearest neighbor (N3) classifier corresponds
#'      to the error rate of a one Nearest Neighbor (1NN) classifier, estimated 
#'      using a leave-one-out procedure in dataset.}
#'    \item{"N4"}{Non-linearity of the nearest neighbor classifier (N4) creates 
#'      a new dataset randomly interpolating pairs of training examples of the 
#'      same class and then induce a the 1NN classifier on the original data and
#'      measure the error rate in the new data points.}
#'    \item{"T1"}{Fraction of hyperspheres covering data (T1) builds 
#'      hyperspheres centered at each one of the training examples, which have 
#'      their radios growth until the hypersphere reaches an example of another 
#'      class. Afterwards, smaller hyperspheres contained in larger hyperspheres 
#'      are eliminated. T1 is finally defined as the ratio between the number of 
#'      the remaining hyperspheres and the total number of examples in the 
#'      dataset.}
#'    \item{"LSC"}{Local Set Average Cardinality (LSC) is based on Local Set 
#'      (LS) and defined as the set of points from the dataset whose distance of
#'      each example is smaller than the distance from the exemples of the 
#'      different class. LSC is the average of the LS.}
#'  }
#' @return A list named by the requested neighborhood measure.
#'
#' @references
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for the
#'    data complexity library in C++. Technical Report. La Salle - Universitat 
#'    Ramon Llull.
#'
#'  Enrique Leyva, Antonio Gonzalez and Raul Perez. (2014). A Set of Complexity
#'    Measures Designed for Applying Meta-Learning to Instance Selection. IEEE
#'    Transactions on Knowledge and Data Engineering 27, 2, 354--367.
#'
#' @examples
#' ## Extract all neighborhood measures
#' data(iris)
#' neighborhood(Species ~ ., iris)
#' @export
neighborhood <- function(...) {
  UseMethod("neighborhood")
}

#' @rdname neighborhood
#' @export
neighborhood.default <- function(x, y, measures="all", summary=c("mean", "sd"), 
                                 ...) {

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

  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  colnames(x) <- make.names(colnames(x), unique=TRUE)
  data <- data.frame(x, class=y)
  dst <- dist(x)

  sapply(measures, function(f) {
    measure = eval(call(paste("c", f, sep="."), dst=dst, data=data))
    post.processing(measure, summary, ...)
  }, simplify=FALSE)
}

#' @rdname neighborhood
#' @export
neighborhood.formula <- function(formula, data, measures="all", 
                                 summary=c("mean", "sd"), ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  neighborhood.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, summary, ...)
}

ls.neighborhood <- function() {
  c("N1","N2", "N3", "N4", "T1", "LSC")
}

c.N1 <- function(dst, data) {

  g <- igraph::graph.adjacency(dst, mode="undirected", weighted=TRUE)
  tree <- as.matrix(igraph::as_adj(igraph::mst(g)))

  tmp <- which(tree != 0, arr.ind=TRUE)
  aux <- which(data[tmp[,1],]$class != data[tmp[,2],]$class)
  aux <- length(unique(tmp[aux,1]))
  return(aux/nrow(data))
}

intra <- function(dst, data, i) {
  tmp <- rownames(data[data$class == data[i,]$class,])
  aux <- min(dst[i, setdiff(tmp, i)])
  return(aux)
}

inter <- function(dst, data, i) {
  tmp <- rownames(data[data$class != data[i,]$class,])
  aux <- sort(dst[i, tmp])[1]
  return(aux)
}

c.N2 <- function(dst, data) {

  aux <- sapply(rownames(data), function(i) {
    c(intra(dst, data, i), inter(dst, data, i))
  })

  #aux <- sum(aux[1,])/sum(aux[2,])
  aux <- 1 - (1/((aux[1,]/aux[2,]) + 1))
  return(aux)
}

knn <- function(data, dst, k) {
  apply(dst, 1, function(i) {
    tmp <- names(sort(i)[k])
    data[tmp,]$class
  })
}

c.N3 <- function(dst, data) {
  aux <- knn(data, dst, 2) != data$class
  #return(mean(aux))
  return(aux)
}

c.N4 <- function(dst, data) {

  tran <- rbind(data, c.generate(data, nrow(data)))
  test <- utils::tail(tran, nrow(data))

  dst <- dist(tran[,-ncol(tran), drop=FALSE])
  dst <- dst[rownames(test), rownames(data)]

  aux <- knn(data, dst, 1) != test$class
  #return(mean(aux))
  return(aux)
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

  aux <- sapply(rownames(data), function(i) {
    as.numeric(radios(dst, data, i))
  })

  return(aux)
}

translate <- function(dst, r) {

  aux <- t(sapply(rownames(dst), function(i) {
    dst[i,] < r[i]
  }))

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

c.T1 <- function(dst, data) {
  r <- hyperspher(dst, data)
  aux <- adherence(translate(dst, r), data)
  #aux <- length(aux)/nrow(data)
  return(aux/nrow(data))
}

c.LSC <- function(dst, data) {
  
  r <- sapply(rownames(data), function(i) {
    as.numeric(inter(dst, data, i))
  })
  
  aux <- 1 - sum(translate(dst, r))/(nrow(dst)^2)
  return(aux)
}
