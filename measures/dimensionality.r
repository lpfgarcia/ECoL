# R Code
# Measures based on dimensionality
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Measues based on dimensionality


pca <- function(data) {

    aux = prcomp(data[,-ncol(data)], scale=TRUE)$sdev
    aux = which(cumsum(aux)/sum(aux) >= 0.95)
    return(aux[1])
}


m1 <- function(data) {
    (ncol(data)-1)/nrow(data)
}


m2 <- function(data) {
    pca(data)/nrow(data)
}


m3 <- function(data) {
    pca(data)/(ncol(data)-1)
}


c1 <- function(data) {

    c = (-1/log(nlevels(data$class)))
    i = table(data$class)/nrow(data)
    aux = c*sum(i*log(i))
    return(aux)
}


dimensionality <- function(data) {

    data = binarize(data)
    aux = lapply(DIMENSIONALITY, 
        function(i) {
            do.call(i, list(data))
    })

    aux = unlist(aux)
    names(aux) = DIMENSIONALITY
    return(aux)
}

