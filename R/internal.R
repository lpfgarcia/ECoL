
colMin <- function(data) {
  apply(data, 2, min)
}

colMax <- function(data) {
  apply(data, 2, max)
}

rowMax <- function(data) {
    apply(data, 1, max)
}

dist <- function(data) {
  as.matrix(cluster::daisy(data, metric="gower"))
}

form <- function(data) {
  att <- paste(colnames(data)[-ncol(data)], collapse="+")
  aux <- stats::formula(paste("~ 0 +", att, sep=" "))
  return(aux)
}

binarize <- function(data) {
  aux <- stats::model.matrix(form(data), data)
  aux <- data.frame(aux, class=data$class)
  return(aux)
}

knn <- function(dst, data, k=3, i) {
  tmp <- names(sort(dst[i,])[1:k+1])
  aux <- data[tmp,]$class
  names(aux) <- tmp
  return(aux) 
}

normalize <- function(data) {

  for(i in 1:(ncol(data)-1))
    if(is.numeric(data[,i]))
      data[,i] <- as.numeric(scale(data[,i]))
  return(data)
}

ovo <- function(data) {

  aux <- utils::combn(levels(data$class), 2)

  tmp <- apply(aux, 2, function(i) {
    vet <- subset(data, data$class %in% i)
    vet$class <- factor(vet$class)
    return(vet)
  })

  return(tmp)
}

interpolation <- function(data) {

  aux <- sample(levels(data$class), 1)
  aux <- data[data$class == aux,] 
  aux <- data[sample(1:nrow(aux), 2, replace=FALSE),]

  for(i in 1:(ncol(data)-1)) {
    if(is.numeric(data[,i])) {
      rnd <- stats::runif(1)
      aux[1,i] <- aux[1,i]*rnd + aux[2,i]*(1-rnd)
    }
  }

  return(aux[1,])
}

generate <- function(data) {

  tmp <- do.call("rbind",
    lapply(1:nrow(data),
      function(i) {
        interpolation(data)
    })
  )

  return(tmp)
}

