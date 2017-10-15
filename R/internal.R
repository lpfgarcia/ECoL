
colMin <- function(data) {
  apply(data, 2, min)
}

colMax <- function(data) {
  apply(data, 2, max)
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

interpolation <- function(data) {

  aux <- data[data$class == sample(data$class, 1),] 
  aux <- aux[sample(nrow(aux), 2, replace=FALSE),]

  for(i in 1:(ncol(data)-1)) {
    if(is.numeric(data[,i])) {
      rnd <- stats::runif(1)
      aux[1,i] <- aux[1,i]*rnd + aux[2,i]*(1-rnd)
    }
  }

  return(aux[1,])
}

generate <- function(data, n) {

  tmp <- do.call("rbind",
    lapply(1:n,
      function(i) {
        interpolation(data)
    })
  )

  return(tmp)
}
