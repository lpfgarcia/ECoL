

colMin <- function(x) {
  apply(x, 2, min)
}

colMax <- function(x) {
  apply(x, 2, max)
}

rowMax <- function(x) {
  apply(x, 1, max)
}

dist <- function(x) {
  as.matrix(cluster::daisy(x, metric="gower"))
}

form <- function(x) {
  att <- paste(colnames(x), collapse="+")
  aux <- formula(paste("~ 0 +", att, sep=" "))
  return(aux)
}

binarize <- function(x) {
  data.frame(model.matrix(form(x), x))
}

knn <- function(d, x, y, i, k=3) {
  tmp <- rownames(x) %in% names(sort(d[i,])[1:k+1])
  aux <- y[tmp]
  names(aux) <- rownames(x[tmp,])
  return(aux) 
}

normalize <- function(x) {

  for(i in 1:ncol(x))
    if(is.numeric(x[,i]))
      x[,i] <- as.numeric(scale(x[,i]))
  return(x)
}

one.vs.one <- function(x, y) {

  comb <- utils::combn(levels(y), 2)
  data <- apply(comb, 2, function(i) {
    n <- subset(x, y %in% i)
    p <- subset(y, y %in% i)
    list(n, factor(p))
  })

  return(data)
}


interpolation <- function(x, y) {

  a <- sample(levels(y), 1)
  b <- x[y == a,] %>% sample_n(., 2)

  for(i in 1:ncol(x)) {
    if(is.numeric(x[,i])) {
      rnd <- runif(1)
      b[1,i] <- b[1,i]*rnd + b[2,i]*(1-rnd)
    }
  }

  b <- cbind(b[1,], a)
  return(b)
}

generate <- function(x, y) {

  aux <- do.call("rbind", 
    lapply(1:nrow(x), function(i) {
      interpolation(x, y)
    })
  )

  aux <- list(aux[,1:(ncol(aux)-1)], 
    aux[,ncol(aux)])
  return(aux)
}

