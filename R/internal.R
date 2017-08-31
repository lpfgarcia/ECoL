

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

knn <- function(d, x, y, i) {
  tmp <- rownames(x) %in% names(sort(d[i,])[2:4])
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

