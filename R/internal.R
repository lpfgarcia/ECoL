
colMin <- function(x) {
  apply(x, 2, min)
}

colMax <- function(x) {
  apply(x, 2, max)
}

dist <- function(x) {
  as.matrix(cluster::daisy(x, metric="gower", stand=TRUE))
}

form <- function(x) {
  att <- paste(colnames(x), collapse="+")
  stats::formula(paste("~ 0 +", att, sep=" "))
}

binarize <- function(x) {
  data.frame(stats::model.matrix(form(x), x))
}

ovo <- function(data) {

  aux <- utils::combn(levels(data$class), 2)

  tmp <- apply(aux, 2, function(i) {
    vet <- base::subset(data, data$class %in% i)
    vet$class <- factor(vet$class)
    return(vet)
  })

  return(tmp)
}

c.interpolation <- function(data) {

  aux <- data[data$class == sample(data$class, 1),]
  tmp <- aux[sample(nrow(aux), 2),]

  rnd <- stats::runif(1)

  for(i in 1:(ncol(data)-1)) {
    if(is.numeric(data[,i])) {
      tmp[1,i] <- tmp[1,i] + (tmp[2,i] - tmp[1,i]) * rnd
    } else {
      tmp[1,i] <- sample(aux[,i], 1)
    }
  }

  return(tmp[1,])
}

c.generate <- function(data, n) {

  tmp <- do.call("rbind",
    lapply(1:n, function(i) {
      c.interpolation(data)
    })
  )

  return(tmp)
}

maxmin <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

normalize <- function(x) {

  x <- as.data.frame(x)
  for(i in 1:ncol(x))
    if(is.numeric(x[,i]))
      x[,i] <- maxmin(x[,i])
  return(x)
}

spearman <- function(x) {
  1-6*sum(x^2)/(length(x)^3 - length(x))
}

maxPosition <- function(x) {
  order(-x)[1]
}

minPosition <- function(x) {
  order(x)[1]
}

r.interpolation <- function(x, y, i) {

  aux <- x[(i-1):i,,drop=FALSE]

  rnd <- stats::runif(1)
  for(j in 1:ncol(x)) {
    if(is.numeric(x[,j])) {
      aux[1,j] <- aux[1,j] + (aux[2,j] - aux[1,j]) * rnd
    } else {
      aux[1,j] <- sample(aux[,j], 1)
    }
  }

  tmp <- y[(i-1):i]
  rnd <- stats::runif(1)
  tmp[1] <- tmp[1]*rnd + tmp[2]*(1-rnd)

  return(cbind(aux[1,], tmp[1]))
}

r.generate <- function(x, y, n) {

  tmp <- do.call("rbind",
    lapply(2:n, function(i) {
      r.interpolation(x, y, i)
    })
  )

  tmp <- data.frame(tmp)
  colnames(tmp) <- c(colnames(x), "y")
  return(tmp)
}
