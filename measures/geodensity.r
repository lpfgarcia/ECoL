# R Code
# Measures based on Density
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Measues based on density


volume <- function(data) {
    data = data[,-ncol(data)]
    prod(colMax(data) - colMin(data))
}


d2 <- function(dst, data, k=3) {

    aux = unlist(
        lapply(rownames(data),
            function(i) {
                tmp = knn(dst, data, k, i)
                volume(data[names(tmp),])
        })
    )

    return(mean(aux))
}


voting <- function(pred, data, i) {

    if(max(table(pred)) >= 2)
        return(which.max(table(pred)))
    return(data[i,]$class)
}


d3 <- function(dst, data, k=3) {

    aux = unlist(
        lapply(rownames(data),
            function(i) {
                tmp = knn(dst, data, k, i)
                voting(tmp, data, i)
        })
    )

    aux = mean(aux != data$class)
    return(aux)
}


geodensity <- function(data) {

    data = binarize(data)
    dst = dist(data[,-ncol(data)])
    aux = lapply(GEODENSITY, 
        function(i) {
            do.call(i, list(dst, data))
    })

    aux = unlist(aux)
    names(aux) = GEODENSITY
    return(aux)
}

