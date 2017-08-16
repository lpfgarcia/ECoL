# R Code
# Local-Set Based Measures
# L. P. F. Garcia A. C. Lorena and M. de Souto 2017
# The set of Local-Set Based Measures


LSCAvg <- function(dst, data) {

    r = sapply(rownames(data), function(i) {
        as.numeric(inter(dst, data, i))
    })

    aux = adherence(translate(dst, r), data)
    aux = sum(aux)/(length(aux)^2)
    return(aux)
}


localset <- function(data) {

    dst = dist(data[,-ncol(data)])
    aux = lapply(LOCALSET, 
        function(i) {
            do.call(i, list(dst, data))
    })

    aux = unlist(aux)
    names(aux) = LOCALSET
    return(aux)
}